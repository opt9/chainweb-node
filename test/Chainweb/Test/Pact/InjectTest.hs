{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.InjectTest
( main
, tests
) where

import Control.Concurrent.MVar
import Control.Exception (throwIO)
import Control.Lens
import Control.Monad.IO.Class

import qualified Data.Aeson as A
import Data.CAS.HashMap hiding (toList)
import qualified Data.HashMap.Strict as HM
import Data.String.Conv (toS)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.IO.Unsafe
import System.IO.Extra
import System.LogLevel

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

import Pact.Parse
import qualified Pact.Types.ChainId as P
import Pact.Types.ChainMeta
import qualified Pact.Types.Command as P
import Pact.Types.Exp
import qualified Pact.Types.Exp as P
import qualified Pact.Types.Hash as P
import Pact.Types.RPC
import qualified Pact.Types.PactValue as P
import Pact.Types.Term

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.Graph
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Templates
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import qualified Chainweb.TreeDB as TDB
import Chainweb.Version

main :: IO ()
main = do
  spec <- testSpec "original error" baseInjTest
  spec2 <- testSpec "fixed" fixedInjTest
  let valTest = validationTest
  defaultMain $ testGroup "Pact injection tests"
      [ spec, spec2, valTest]

tests :: TestTree
tests = testGroup "Pact injection tests"
            [ unsafePerformIO (testSpec "spec" baseInjTest)
            , unsafePerformIO (testSpec "spec" fixedInjTest)
            , validationTest
            ]

baseInjTest :: Spec
baseInjTest =
    describe "Basic injection test" $ do
        it "shows the original Pact injection error" $ do
            z <- mkCoinbaseCmd badMinerId minerKeys0 (ParsedDecimal 1.0)
            case z of
              ExecMsg (ParsedCode pccode _pcexps) _pmdata -> do
                pccode `shouldBe` badCodeStr

fixedInjTest :: Spec
fixedInjTest =
    describe "Fixed injection test" $ do
        it "demonstrates the Pact injection is no longer possible" $ do
            let (_tName, exec) = mkCoinbaseTerm badMinerId minerKeys0 (ParsedDecimal 1.0)
            case exec of
              ExecMsg (ParsedCode pccode _pcexps) _pmdata -> do
                  T.unpack pccode `shouldNotContain` "coinbase"
                  T.unpack pccode `shouldNotContain` "read-keyset"

validationTest ::  TestTree
validationTest =
    withRocksResource $ \rocksIO ->
    withTemporaryDir $ \dir ->
    withBlockHeaderDb rocksIO genBlock $ \bhdb ->
    withPayloadDb $ \pdb ->
        unsafePerformIO $ testBadBlock pdb bhdb cid dir genBlock
  where
    genBlock = genesisBlockHeader testVer cid
    cid = someChainId testVer

testBadBlock
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> ChainId
    -> IO FilePath
    -> BlockHeader
    -> IO TestTree
testBadBlock pdb iobhdb cid dir header = do
    bhdb <- iobhdb
    mVar <- newMVar (0 :: Int)
    return $ withPact testVer Warn pdb iobhdb (testMemPoolAccess cid mVar) dir 100000
        (\reqQIO -> unsafePerformIO (testSpec "someText" (injectPactSpec bhdb header reqQIO )))

injectPactSpec
  :: BlockHeaderDb
  -> BlockHeader
  -> IO PactQueue
  -> Spec
injectPactSpec db parent reqQIO =
    describe "injection vulnerability test" $
        it "tries to inject code via a malicious miner id" $ do
            reqQ <- reqQIO

            -- process new block w/a malicious miner (as occurs when requesting a block to mine...)
            let miner = mkMaliciousMiner
            (_fromNew, _fromVal, _newHeader) <- processBlock miner db parent reqQ

            -- exec a local tx reading the acct used by the malicious miner
            locVar <- getBalanceLocal badMinerAcct >>= (\cwt -> local cwt reqQ)
            loc <- takeMVar locVar
            let theResult = case loc of
                  Left pe -> assertFailure $ show pe
                  Right cmd -> do
                    case P._crResult cmd of
                        P.PactResult (Right (P.PLiteral (P.LDecimal n))) -> return $ fromEnum n
                        _someOther -> assertFailure "Expected Pact Literal (Decimal)"
            theResult `shouldBe` badMinerAmt

getBalanceLocal :: Text -> IO ChainwebTransaction
getBalanceLocal acct = do
    d <- adminData
    fmap (head . V.toList)
      $ toCWTransactions
      $ V.fromList [ PactTransaction ("(coin.getBalance \"" ++ show acct ++ "\")") d ]

toCWTransactions :: Vector PactTransaction -> IO (Vector ChainwebTransaction)
toCWTransactions txs = do
    ks <- testKeyPairs sender00KeyPair Nothing
    mkTestExecTransactions "sender00" "0" ks "1" 100000 0.00001 1000000 0 txs

mkMaliciousMiner :: Miner
mkMaliciousMiner =
    let minerKeySet = mkKeySet
          ["f8880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
          "keys-all"
    in Miner badMinerId $ MinerKeys minerKeySet

processBlock
    :: Miner
    -> BlockHeaderDb
    -> BlockHeader
    -> PactQueue
    -> IO (Int, Int, BlockHeader)
processBlock miner db parent reqQ = do
    let blockCreateTime = BlockCreationTime $ Time $ secondsToTimeSpan $ Seconds $ succ 1000000
    mvNew <- newBlock miner parent blockCreateTime reqQ
    (plwoNew, asIntNew) <- getResult mvNew
    new' <- mkProperNewBlock db plwoNew parent
    mvVal <- runValidateBlock plwoNew new' reqQ
    (_plwoVal, asIntVal) <- getResult mvVal
    return (asIntNew, asIntVal, new')

getResult :: MVar (Either PactException PayloadWithOutputs) -> IO (PayloadWithOutputs, Int)
getResult mvar = do
    res <- takeMVar mvar
    case res of
        (Left pactEx) -> throwIO pactEx
        (Right plwo) -> do
            let outs = V.map snd (_payloadWithOutputsTransactions plwo)
            n <- asSingleResult outs
            return (plwo, n)

asSingleResult :: Vector TransactionOutput -> IO Int
asSingleResult txOuts = do
  theInts <- traverse txAsIntResult txOuts
  let theSum = V.sum theInts
  return theSum

txAsIntResult :: TransactionOutput -> IO Int
txAsIntResult txOut = do
    let theBytes = _transactionOutputBytes txOut
    case A.decode (toS theBytes) :: Maybe (P.CommandResult P.Hash) of
        Nothing -> return 0
        Just cmd -> do
          let res = P._crResult cmd
          case res of
              P.PactResult (Right (P.PLiteral (P.LDecimal n))) -> return $ fromEnum n
              _someOther -> return 0

-- validate the same transactions as sent to newBlock
runValidateBlock
    :: PayloadWithOutputs
    -> BlockHeader
    -> PactQueue
    -> IO (MVar (Either PactException PayloadWithOutputs))
runValidateBlock plwo blockHeader reqQ = do
    let plData = payloadWithOutputsToPayloadData plwo
    validateBlock blockHeader plData reqQ

mkProperNewBlock
    :: BlockHeaderDb
    -> PayloadWithOutputs
    -> BlockHeader
    -> IO BlockHeader
mkProperNewBlock db plwo parentHeader = do
    let adjParents = BlockHashRecord HM.empty
    let matchingPlHash = _payloadWithOutputsPayloadHash plwo
    creationTime <- getCurrentTimeIntegral
    let newHeader = newBlockHeader adjParents matchingPlHash (Nonce 0)
          (BlockCreationTime creationTime) (ParentHeader parentHeader)
    liftIO $ TDB.insert db newHeader
    return newHeader

testMemPoolAccess :: ChainId -> MVar Int -> MemPoolAccess
testMemPoolAccess cid mvar =
    let pactCid = P.ChainId $ chainIdToText cid
    in MemPoolAccess
      { mpaGetBlock = \_validate bh hash _header ->
          (getBlockFromHeight pactCid mvar) (fromIntegral bh) hash
      , mpaSetLastHeader = \_ -> return ()
      , mpaProcessFork = \_ -> return ()
      }
  where
    getBlockFromHeight pCid mv bHeight _bHash = do
        txs <- txsFromHeight mv bHeight
        let f = modifyPayloadWithText . set (P.pMeta . pmCreationTime)
            g = modifyPayloadWithText . set (P.pMeta . pmTTL)
            h = modifyPayloadWithText . set (P.pMeta . pmChainId)

        outtxs' <- toCWTransactions pCid txs
        currentTime <- getCurrentTimeIntegral
        let outtxs = flip V.map outtxs' $ \tx ->
                let ttl = TTLSeconds $ ParsedInteger $ 24 * 60 * 60 -- 24 hours
                in fmap (h pCid . g ttl . f (toTxCreationTime currentTime)) tx
        return outtxs

txsFromHeight :: MVar Int -> Int -> IO (Vector PactTransaction)
txsFromHeight _mvar 0 = error "txsFromHeight called for Genesis block"
txsFromHeight mvar 1 = do
    _ <- modifyMVar mvar (\n -> return (n + 1, n + 1))
    d <- adminData
    moduleStr <- readFile' $ testPactFilesDir ++ "inject-test.pact"
    return $ V.fromList
        ( [ PactTransaction { _pactCode = T.pack moduleStr , _pactData = d } ] )
txsFromHeight mvar 2 = do
    _ <- modifyMVar mvar (\n -> return (n + 1, n + 1))
    d <- adminData
    return $ V.fromList
        ( [ PactTransaction { _pactCode = injectAttempt1 , _pactData = d } ] )
txsFromHeight mvar _h = do
    _newCount <- modifyMVar mvar (\n -> return (n + 1, n + 1))
    return V.empty

modifyPayloadWithText
    :: (P.Payload PublicMeta P.ParsedCode -> P.Payload PublicMeta P.ParsedCode)
    -> PayloadWithText
    -> PayloadWithText
modifyPayloadWithText f pwt = mkPayloadWithText newPayload
  where
    oldPayload = payloadObj pwt
    newPayload = f oldPayload

----------------------------------------------------------------------------------------------------
-- Test data
----------------------------------------------------------------------------------------------------
badMinerAmt :: Double
badMinerAmt = 9999999.99

badMinerAcct :: Text
badMinerAcct = "alpha"

badMinerId :: MinerId
badMinerId = MinerId ("alpha\" (read-keyset \"miner-keyset\")"
  `T.append` (show badMinerAmt) `T.append` ")(coin.coinbase \"alpha\")")

badCodeStr :: Text
badCodeStr = "(coin.coinbase \"alpha\" (read-keyset \"miner-keyset\")"
  `T.append` (show badMinerAmt) `T.append`
  ") (coin.coinbase \"alpha\" (read-keyset \"miner-keyset\") (read-decimal \"reward\"))"

_testVersion :: ChainwebVersion
_testVersion = FastTimedCPM petersonChainGraph

minerKeys0 :: MinerKeys
minerKeys0 = MinerKeys $ mkKeySet
  ["f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
  "default"

_minerId0 :: MinerId
_minerId0 = MinerId "default miner"

testVer :: ChainwebVersion
testVer = FastTimedCPM petersonChainGraph

injectAttempt1 :: Text
injectAttempt1 = "(free.inject-test.read-account \"Acct2\")"
