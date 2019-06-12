{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module Chainweb.Test.Pact.Checkpointer (tests) where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad (void)
import Control.Monad.Reader

import Data.Aeson (Value(..), object, (.=), toJSON)
import Data.Default (def)
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Map.Strict as M

import NeatInterpolation (text)

import Pact.Gas (freeGasEnv)
import Pact.Interpreter (EvalResult(..), PactDbEnv(..))
import Pact.Native (nativeDefs)
import Pact.Repl
import Pact.Repl.Types
-- import Pact.Types.Info
import Pact.Types.Runtime (ExecutionMode(Transactional))
import qualified Pact.Types.Hash as H
-- import Pact.Types.Logger (Loggers, newLogger)
import Pact.Types.Logger (newLogger)
import Pact.Types.PactValue
import Pact.Types.Persistence
import Pact.Types.RPC (ContMsg(..))
import Pact.Types.Runtime
import Pact.Types.Server (CommandEnv(..))
import Pact.Types.Type (PrimType(..), Type(..))
import Pact.Types.Exp (Literal(..))


import Test.Tasty
import Test.Tasty.HUnit

-- internal imports

import Chainweb.BlockHash (BlockHash(..), nullBlockHash)
import Chainweb.BlockHeader (BlockHeight(..))
import Chainweb.MerkleLogHash (merkleLogHash)
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.InMemoryCheckpointer (initInMemoryCheckpointEnv)
import Chainweb.Pact.Backend.RelationalCheckpointer
-- import Chainweb.Pact.Backend.MemoryDb (mkPureState)
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.TransactionExec
    (applyContinuation', applyExec', buildExecParsedCode)
-- import Chainweb.Pact.Utils (toEnv', toEnvPersist')
import Chainweb.Pact.Backend.Utils
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils

tests :: ScheduledTest
tests = testGroupSch "Checkpointer" [testInMemory, testKeyset, testRelational, testCase "PactDb Regression" testRegress]

testInMemory :: TestTree
testInMemory = checkpointerTest "In-memory Checkpointer" InMem


defModule :: Text -> Text
defModule idx = [text| ;;

(define-keyset 'k$idx (read-keyset 'k$idx))

(module m$idx 'k$idx

  (defschema sch col:integer)

  (deftable tbl:{sch})

  (defun insertTbl (a i)
    (insert tbl a { 'col: i }))

  (defun readTbl ()
    (sort (map (at 'col)
      (select tbl (constantly true)))))

  (defpact dopact (n)
    (step { 'name: n, 'value: 1 })
    (step { 'name: n, 'value: 2 }))

)
(create-table tbl)
(readTbl)
(insertTbl "a" 1)
|]

tIntList :: [Int] -> Term n
tIntList = toTList (TyPrim TyInteger) def . map toTerm


testKeyset :: TestTree
testKeyset = withResource initializeSQLite freeSQLiteResource (runSQLite keysetTest)

keysetTest ::  IO CheckpointEnv -> TestTree
keysetTest c = testCaseSteps "Keyset test" $ \next -> do

  CheckpointEnv {..} <- c

  let hash00 = nullBlockHash

  next "init"

  _blockenv00 <- restore _cpeCheckpointer Nothing

  save _cpeCheckpointer hash00

  next "next block (blockheight 1, version 0)"

  let bh01 = BlockHeight 1

  _hash01 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001a")

  blockenv01 <- restore _cpeCheckpointer (Just (bh01, hash00))
  addKeyset blockenv01 "k2" (KeySet [] (Name ">=" (Info Nothing)))

  discard _cpeCheckpointer

  next "fork on blockheight = 1"

  let bh11 = BlockHeight 1

  hash11 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001b")
  blockenv11 <- restore _cpeCheckpointer (Just (bh11, hash00))
  addKeyset blockenv11 "k1" (KeySet [] (Name ">=" (Info Nothing)))
  save _cpeCheckpointer hash11

addKeyset :: PactDbEnv' -> KeySetName -> KeySet -> IO ()
addKeyset (PactDbEnv' (PactDbEnv pactdb mvar)) keysetname keyset = _writeRow pactdb Insert KeySets keysetname keyset mvar


data InitData = OnDisk | InMem

testRelational :: TestTree
testRelational = checkpointerTest "Relational Checkpointer" OnDisk

runSQLite :: (IO CheckpointEnv -> TestTree) -> IO (IO (), SQLiteEnv) -> TestTree
runSQLite runTest = runTest . make
  where
    make :: IO (IO (), SQLiteEnv) -> IO CheckpointEnv
    make iosqlenv = do
      (_,sqlenv) <- iosqlenv
      let initBlockState = BlockState 0 Nothing (BlockVersion 0 0) M.empty (newLogger loggers "BlockEnvironment")
          loggers = pactTestLogger False
      snd <$> initRelationalCheckpointer initBlockState sqlenv (newLogger loggers "RelationalCheckpointer") freeGasEnv

checkpointerTest :: String -> InitData -> TestTree
checkpointerTest name initdata =
      case initdata of
        OnDisk -> withResource initializeSQLite freeSQLiteResource (runSQLite runTest)
        InMem -> let loggers = pactTestLogger False
          in withResource (snd <$> initInMemoryCheckpointEnv loggers (newLogger loggers "inMemCheckpointer") freeGasEnv) (const $ return ()) runTest
  where
    runTest :: IO CheckpointEnv -> TestTree
    runTest c  = testCaseSteps name $ \next -> do
          (CheckpointEnv {..}) <-    c
          let ksData :: Text -> Value
              ksData idx = object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]

              runExec :: PactDbEnv'-> Maybe Value -> Text -> IO EvalResult
              runExec (PactDbEnv' pactdbenv) eData eCode = do
                  let cmdenv = CommandEnv Nothing Transactional pactdbenv _cpeLogger _cpeGasEnv def
                  execMsg <- buildExecParsedCode eData eCode
                  applyExec' cmdenv def execMsg [] (H.toUntypedHash (H.hash "" :: H.PactHash)) noSPVSupport


              runCont :: PactDbEnv' -> PactId -> Int -> IO EvalResult
              runCont (PactDbEnv' pactdbenv) pactId step = do
                  let contMsg = ContMsg pactId step False Null
                      cmdenv = CommandEnv Nothing Transactional pactdbenv _cpeLogger _cpeGasEnv def
                  applyContinuation' cmdenv def contMsg [] (H.toUntypedHash (H.hash "" :: H.PactHash)) noSPVSupport
            ------------------------------------------------------------------
            -- s01 : new block workflow (restore -> discard), genesis
            ------------------------------------------------------------------

          next "Step 1 : new block workflow (restore -> discard), genesis"

          let hash00 = nullBlockHash
          blockenvGenesis0 <- restore _cpeCheckpointer Nothing

          void $ runExec blockenvGenesis0 (Just $ ksData "1") $ defModule "1"
          runExec blockenvGenesis0 Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]
          discard _cpeCheckpointer

        -----------------------------------------------------------
        -- s02 : validate block workflow (restore -> save), genesis
        -----------------------------------------------------------

          next "Step 2 : validate block workflow (restore -> save), genesis"
          blockenvGenesis1 <- restore _cpeCheckpointer Nothing
          void $ runExec blockenvGenesis1 (Just $ ksData "1") $ defModule "1"
          runExec blockenvGenesis1 Nothing "(m1.readTbl)"
            >>=  \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

          save _cpeCheckpointer hash00

        ------------------------------------------------------------------
        -- s03 : new block 00
        ------------------------------------------------------------------

          next "Step 3 : new block 00"
          blockenv00 <- restore _cpeCheckpointer (Just (BlockHeight 1, hash00))
          -- start a pact
          -- test is that exec comes back with proper step
          let pactId = "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g"
              pactCheckStep = preview (_Just . peStep) . _erExec

          void $ runExec blockenv00 Nothing "(m1.insertTbl 'b 2)"

          runExec blockenv00 Nothing "(m1.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

          runExec blockenv00 Nothing "(m1.dopact 'pactA)" >>= ((Just 0 @=?) . pactCheckStep)

          discard _cpeCheckpointer

        ------------------------------------------------------------------
        -- s04: validate block 1
        ------------------------------------------------------------------

          next "Step 4: validate block 1"
          hash01 <- BlockHash <$> liftIO (merkleLogHash "0000000000000000000000000000001a")

          blockenv01 <- restore _cpeCheckpointer (Just (BlockHeight 1, hash00))

          void $ runExec blockenv01 Nothing "(m1.insertTbl 'b 2)"

          runExec blockenv01 Nothing "(m1.readTbl)"
            >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

          runExec blockenv01 Nothing "(m1.dopact 'pactA)"
            >>= ((Just 0 @=?) . pactCheckStep)

          save _cpeCheckpointer hash01

        ------------------------------------------------------------------
        -- s05: validate block 02
        -- create m2 module, exercise RefStore checkpoint
        -- exec next part of pact
        ------------------------------------------------------------------

          let msg =   "Step 5: validate block 02\n create m2 module, exercise RefStore checkpoint\n exec next part of pact"

          next msg
          hash02 <- BlockHash <$> merkleLogHash "0000000000000000000000000000002a"

          blockenv02 <- restore _cpeCheckpointer (Just (BlockHeight 2, hash01))

          void $ runExec blockenv02 (Just $ ksData "2") $ defModule "2"

          runExec blockenv02 Nothing "(m2.readTbl)"
            >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

          runCont blockenv02 pactId 1
            >>= ((Just 1 @=?) . pactCheckStep)

          save _cpeCheckpointer hash02

        ------------------------------------------------------------------
        -- s06 : new block 03
        ------------------------------------------------------------------

          next "Step 6 : new block 03"
          blockenv03 <- restore _cpeCheckpointer (Just (BlockHeight 3, hash02))

          void $ runExec blockenv03 Nothing "(m2.insertTbl 'b 2)"

          runExec blockenv03 Nothing "(m2.readTbl)"
            >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

          discard _cpeCheckpointer

        ------------------------------------------------------------------
        -- s07 : validate block 03
        ------------------------------------------------------------------
          next "Step 7 : validate block 03"
          hash03 <- BlockHash <$> merkleLogHash "0000000000000000000000000000003a"

          blockenv13 <- restore _cpeCheckpointer (Just (BlockHeight 3, hash02))

          -- insert here would fail if new block 03 had not been discarded
          void $ runExec blockenv13 Nothing "(m2.insertTbl 'b 2)"

          runExec blockenv13 Nothing "(m2.readTbl)"
            >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1,2]]

          save _cpeCheckpointer hash03

      ------------------------------------------------------------------
        -- s08: FORK! block 02, new hash
        -- recreate m2 module, exercise RefStore checkpoint
        -- exec next part of pact
        ------------------------------------------------------------------

          let msgFork = "Step 8: FORK! block 02, new hash\n recreate m2 module, exercise RefStore checkpoint\n exec next part of pact"

          next msgFork
          hash02Fork <- BlockHash <$> merkleLogHash "0000000000000000000000000000002b"

          blockenv02Fork <- restore _cpeCheckpointer (Just (BlockHeight 2, hash01))

          void $ runExec blockenv02Fork (Just $ ksData "2") $ defModule "2"


          runExec blockenv02Fork Nothing "(m2.readTbl)" >>= \EvalResult{..} -> Right _erOutput @?= traverse toPactValue [tIntList [1]]

          -- this would fail if not a fork
          runCont blockenv02Fork pactId 1 >>= ((Just 1 @=?) . pactCheckStep)

          save _cpeCheckpointer hash02Fork


toTerm' :: ToTerm a => a -> Term Name
toTerm' = toTerm

{- You should probably think about the orphan, their name is
 ExecutionMode, that you are leaving behind. They deserve a home! -}
testRegress :: Assertion
testRegress =
  regressChainwebPactDb >>= fmap (toTup . _benvBlockState) . readMVar >>=
  assertEquals "The final block state is" finalBlockState
  where
    finalBlockState = (2, BlockVersion 0 0, M.empty)
    toTup (BlockState txid _ blockVersion txRecord _) =
      (txid, blockVersion, txRecord)

regressChainwebPactDb :: IO (MVar (BlockEnv SQLiteEnv))
regressChainwebPactDb = do
 withTempSQLiteConnection []  $ \sqlenv -> do
        let initBlockState = BlockState 0 Nothing (BlockVersion 0 0) M.empty (newLogger loggers "BlockEnvironment")
            loggers = pactTestLogger False
        runRegression chainwebpactdb
          (BlockEnv sqlenv initBlockState)
          (\v -> runBlockEnv v initSchema)

 {- this should be moved to pact -}
begin :: PactDb e -> Method e (Maybe TxId)
begin pactdb = _beginTx pactdb Transactional

{- this should be moved to pact -}
commit :: PactDb e -> Method e [TxLog Value]
commit pactdb = _commitTx pactdb

{- this should be moved to pact -}
runRegression ::
  PactDb e -- your pactdb instance
  -> e -- ambient environment
  -> (MVar e -> IO ()) -- schema "creator"
  -> IO (MVar e) -- the final state of the environment
runRegression pactdb e schemaInit = do
  conn <- newMVar e
  schemaInit conn
  Just t1 <- begin pactdb conn
  let user1 = "user1"
      usert = UserTables user1
      toPV :: ToTerm a => a -> PactValue
      toPV = toPactValueLenient . toTerm'
  _createUserTable pactdb user1 "someModule" conn
  assertEquals' "output of commit2"
    [TxLog "SYS:usertables" "user1" $
      object [ ("utModule" .= object [ ("name" .= String "someModule"), ("namespace" .= Null)])
             ]
    ]
    (commit pactdb conn)
  void $ begin pactdb conn
  {- the below line is commented out because we no longer support _getUserTableInfo -}
  -- assertEquals' "user table info correct" "someModule" $ _getUserTableInfo chainwebpactdb user1 conn
  let row = ObjectMap $ M.fromList [("gah", PLiteral (LDecimal 123.454345))]
  _writeRow pactdb Insert usert "key1" row conn
  assertEquals' "usert insert" (Just row) (_readRow pactdb usert "key1" conn)
  let row' = ObjectMap $ M.fromList [("gah",toPV False),("fh",toPV (1 :: Int))]
  _writeRow pactdb Update usert "key1" row' conn
  assertEquals' "user update" (Just row') (_readRow pactdb usert "key1" conn)
  let ks = KeySet [PublicKey "skdjhfskj"] (Name "predfun" def)
  _writeRow pactdb Write KeySets "ks1" ks conn
  assertEquals' "keyset write" (Just ks) $ _readRow pactdb KeySets "ks1" conn
  (modName,modRef,mod') <- loadModule
  _writeRow pactdb Write Modules modName mod' conn
  assertEquals' "module write" (Just mod') $ _readRow pactdb Modules modName conn
  assertEquals "module native repopulation" (Right modRef) $
    traverse (traverse (fromPersistDirect nativeLookup)) mod'
  assertEquals' "result of commit 3"

    [ TxLog { _txDomain = "SYS:KeySets"
            , _txKey = "ks1"
            , _txValue = toJSON ks
            }
    , TxLog { _txDomain = "SYS:Modules"
            , _txKey = asString modName
            , _txValue = toJSON mod'
            }
    , TxLog { _txDomain = "user1"
            , _txKey = "key1"
            , _txValue = toJSON row
            }
    , TxLog { _txDomain = "user1"
            , _txKey = "key1"
            , _txValue = toJSON row'
            }
    ]
    (commit pactdb conn)
  void $ begin pactdb conn
  tids <- _txids pactdb user1 t1 conn
  assertEquals "user txids" [1] tids
  -- assertEquals' "user txlogs"
  --   [TxLog "user1" "key1" row,
  --    TxLog "user1" "key1" row'] $
  --   _getTxLog chainwebpactdb usert (head tids) conn
  assertEquals' "user txlogs" [TxLog "user1" "key1" (ObjectMap $ on M.union _objectMap row' row)] $
    _getTxLog pactdb usert (head tids) conn
  _writeRow pactdb Insert usert "key2" row conn
  assertEquals' "user insert key2 pre-rollback" (Just row) (_readRow pactdb usert "key2" conn)
  assertEquals' "keys pre-rollback" ["key1","key2"] $ _keys pactdb (UserTables user1) conn
  _rollbackTx pactdb conn
  assertEquals' "rollback erases key2" Nothing $ _readRow pactdb usert "key2" conn
  assertEquals' "keys" ["key1"] $ _keys pactdb (UserTables user1) conn
  return conn

assertEquals' :: (Eq a, Show a, NFData a) => String -> a -> IO a -> IO ()
assertEquals' msg a b = assertEquals msg a =<< b

assertEquals :: (Eq a,Show a,NFData a) => String -> a -> a -> IO ()
assertEquals msg a b | [a,b] `deepseq` a == b = return ()
                     | otherwise =
                         throwFail $ "FAILURE: " ++ msg ++ ": expected \n  " ++ show a ++ "\n got \n  " ++ show b

throwFail :: String -> IO a
throwFail = throwIO . userError

loadModule :: IO (ModuleName, ModuleData Ref, PersistModuleData)
loadModule = do
  let fn = "test/pact/simple.repl"
  (r,s) <- execScript' (Script False fn) fn
  let mn = ModuleName "simple" Nothing
  case r of
    Left a -> throwFail $ "module load failed: " ++ show a
    Right _ -> case preview (rEvalState . evalRefs . rsLoadedModules . ix mn) s of
      Just (md,_) -> case traverse (traverse toPersistDirect) md of
        Right md' -> return (mn,md,md')
        Left e -> throwFail $ "toPersistDirect failed: " ++ show e
      Nothing -> throwFail $ "Failed to find module 'simple': " ++
        show (view (rEvalState . evalRefs . rsLoadedModules) s)

nativeLookup :: NativeDefName -> Maybe (Term Name)
nativeLookup (NativeDefName n) = case HM.lookup (Name n def) nativeDefs of
  Just (Direct t) -> Just t
  _ -> Nothing
