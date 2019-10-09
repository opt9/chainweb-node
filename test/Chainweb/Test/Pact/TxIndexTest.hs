{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Chainweb.Test.Pact.TxIndexTest where

import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

import Data.Aeson
import Data.Bytes.Put (runPutS)
import Data.CAS.HashMap
import Data.IORef
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..), T3(..))
import qualified Data.Vector as V
import Data.Word

import NeatInterpolation

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- pact imports

import Pact.ApiReq
import Pact.Types.Command

-- chainweb imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.ChainId
import Chainweb.Difficulty
-- import Chainweb.Mempool.Mempool (MempoolPreBlockCheck)
import Chainweb.Miner.Core (HeaderBytes(..), TargetBytes(..), mine, usePowHash)
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
-- import Chainweb.Transaction
import Chainweb.TreeDB
import Chainweb.Utils (runGet)
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Tests

testVer :: ChainwebVersion
testVer = Development

testChainId :: ChainId
testChainId = someChainId testVer

tests :: ScheduledTest
tests = ScheduledTest label $
  withRocksResource $ \rocksIO ->
  withPayloadDb $ \pdb ->
  withBlockHeaderDb rocksIO genblock $ \bhdb ->
  withTemporaryDir $ \dir ->
  testGroup label
  [
    withTime $ \iot -> do
      let mempoolAccess = testMemPoolAccess iot
      withPact testVer Warn pdb bhdb mempoolAccess dir $ \reqQIO ->
        testCase "check for dupe txhashes" $
            run genblock pdb bhdb mempoolAccess reqQIO

  ]
  where
    label = "Chainweb.Test.Pact.TxIndexTest"
    genblock = genesisBlockHeader testVer cid
    cid = someChainId testVer

testMemPoolAccess :: IO (Time Integer) -> MemPoolAccess
testMemPoolAccess iot = MemPoolAccess
    { mpaGetBlock = \validate bh hash _header -> do
        t <- f bh <$> iot
        getTestBlock t validate bh hash
    , mpaSetLastHeader = const (return ())
    , mpaProcessFork = const (return ())
    }
  where
    f :: BlockHeight -> Time Integer -> Time Integer
    f b tt =
        foldl' (flip add) tt (replicate (fromIntegral b) millisecond)
    getTestBlock txOrigTime validate bHeight@(BlockHeight bh) hash = do
        akp0 <- stockKey "sender00"
        kp0 <- mkKeyPairs [akp0]
        let nonce = T.pack . show @(Time Integer) $ txOrigTime
        outtxs <- mkTestExecTransactions
                  "sender00" "0" kp0
                  nonce 10000 0.00000000001
                  3600 (toTxCreationTime txOrigTime) (tx bh)
        oks <- validate bHeight hash outtxs
        when (not $ V.and oks) $ do
            fail $ mconcat [ "tx faild validation! input list: \n"
                                 , show $ tx bh
                                 , "\n\nouttxs: "
                                 , show outtxs
                                 , "\n\noks: "
                                 , show oks
                                 ]
        return outtxs
      where
        ksData :: Text -> Value
        ksData idx = object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]
        tx nonce = V.singleton $ PactTransaction (code nonce) (Just $ ksData (T.pack $ show nonce))
        code nonce = defModule (T.pack $ show nonce)

defModule :: Text -> Text
defModule idx = [text| ;;

(define-keyset 'k$idx (read-keyset 'k$idx))

(module m$idx 'k$idx

  (defschema sch col:integer)

  (deftable tbl:{sch})

  (defun insertTbl (a i)
    (insert tbl a { 'col: i }))

  (defun updateTbl (a i)
    (update tbl a { 'col: i}))

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

-- -------------------------------------------------------------------------- --
-- Utils

run
    :: BlockHeader
    -> IO (PayloadDb HashMapCas)
    -> IO (BlockHeaderDb)
    -> MemPoolAccess
    -> IO PactQueue
    -> Assertion
run genesisBlock iopdb iobhdb mempoolAccess rr = do
    nonceCounter <- newIORef (1 :: Word64)
    void $ mineLine genesisBlock nonceCounter 1
  where
    mineLine start ncounter len =
      evalStateT (runReaderT (mapM (const go) [startHeight :: Word64 .. (startHeight + len)]) rr) start
        where
          startHeight = fromIntegral $ _blockHeight start
          go = do
              r <- ask
              pblock <- get
              n <- liftIO $ Nonce <$> readIORef ncounter
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock n iopdb iobhdb mempoolAccess r
              liftIO $ modifyIORef' ncounter succ
              put newblock
              return ret

mineBlock
    :: BlockHeader
    -> Nonce
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> MemPoolAccess
    -> IO PactQueue
    -> IO (T3 BlockHeader BlockHeader PayloadWithOutputs)
mineBlock parentHeader nonce iopdb iobhdb mempoolAccess r = do

     -- assemble block without nonce and timestamp
     creationTime <- getCurrentTimeIntegral
     mv <- r >>= newBlock noMiner parentHeader (BlockCreationTime creationTime)
     payload <- takeMVar mv >>= \case
        Right x -> return x
        Left e -> throwM $ TestException
            { _exInnerException = toException e
            , _exNewBlockResults = Nothing
            , _exValidateBlockResults = Nothing
            , _exNewBlockHeader = Nothing
            , _exMessage = "failure during newBlock"
            }

     let bh = newBlockHeader
              (BlockHashRecord mempty)
              (_payloadWithOutputsPayloadHash payload)
              nonce
              creationTime
              parentHeader
         hbytes = HeaderBytes . runPutS $ encodeBlockHeaderWithoutHash bh
         tbytes = TargetBytes . runPutS . encodeHashTarget $ _blockTarget bh

     HeaderBytes newBytes  <- usePowHash testVer (\p -> mine p (_blockNonce bh) tbytes) hbytes
     newHeader <- runGet decodeBlockHeaderWithoutHash newBytes

     -- get block's txhashes

     trans <- mpaGetBlock mempoolAccess mempty (_blockHeight newHeader) (_blockHash newHeader) parentHeader

     let hashes = V.map _cmdHash trans

     liftIO $ print hashes

     mresult <- r >>= lookupPactTxs (Just $ T2 (_blockHeight parentHeader) (_blockHash parentHeader)) hashes

     readMVar mresult >>= either throwM (assertBool "should be non-empty" . not) . fmap (maybe False (not . null) . sequence)

     mv' <- r >>= validateBlock newHeader (toPayloadData payload)

     payload' <- takeMVar mv' >>= \case
        Right x -> return x
        Left e -> throwM $ TestException
            { _exInnerException = toException e
            , _exNewBlockResults = Just payload
            , _exValidateBlockResults = Nothing
            , _exNewBlockHeader = Just newHeader
            , _exMessage = "failure during validateBlock"
            }

     pdb <- iopdb
     addNewPayload pdb payload

     bhdb <- iobhdb
     insert bhdb newHeader `catch` \e -> throwM $ TestException
        { _exInnerException = e
        , _exNewBlockResults = Just payload
        , _exValidateBlockResults = Just payload'
        , _exNewBlockHeader = Just newHeader
        , _exMessage = "failure during insert in block header db"
        }

     return $ T3 parentHeader newHeader payload

     where
       toPayloadData :: PayloadWithOutputs -> PayloadData
       toPayloadData d = PayloadData
                 { _payloadDataTransactions = fst <$> _payloadWithOutputsTransactions d
                 , _payloadDataMiner = _payloadWithOutputsMiner d
                 , _payloadDataPayloadHash = _payloadWithOutputsPayloadHash d
                 , _payloadDataTransactionsHash = _payloadWithOutputsTransactionsHash d
                 , _payloadDataOutputsHash = _payloadWithOutputsOutputsHash d
                 }

data TestException = TestException
    { _exInnerException :: !SomeException
    , _exNewBlockResults :: !(Maybe PayloadWithOutputs)
    , _exValidateBlockResults :: !(Maybe PayloadWithOutputs)
    , _exNewBlockHeader :: !(Maybe BlockHeader)
    , _exMessage :: !T.Text
    }
    deriving (Show)

instance Exception TestException
