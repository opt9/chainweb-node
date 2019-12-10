{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Test.Pact.InjectFix
( main
, tests
) where

import Data.Text (Text)
import qualified Data.Text as T

import System.IO.Unsafe

import Test.Tasty
import Test.Tasty.Hspec

import Pact.Parse
import Pact.Types.Exp
import Pact.Types.RPC
import Pact.Types.Term

import Chainweb.Graph
import Chainweb.Miner.Pact
import Chainweb.Pact.Templates
import Chainweb.Version

main :: IO ()
main = do
  spec <- testSpec "original error" baseInjTest
  spec2 <- testSpec "fixed" fixedInjTest
  defaultMain $ testGroup "Pact injection tests"
      [ spec, spec2 ]

tests :: TestTree
tests = testGroup "Pact injection tests"
            [ unsafePerformIO (testSpec "spec" baseInjTest)
            , unsafePerformIO (testSpec "spec" fixedInjTest)
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

----------------------------------------------------------------------------------------------------
-- Test data
----------------------------------------------------------------------------------------------------
badMinerId :: MinerId
badMinerId = MinerId "a30ffd3ba7fa08d4aafcfd9f15594d20e732b8fdda43f02470d76b7ebfa467e6\" (read-keyset \"miner-keyset\") 23045.23) (coin.coinbase \"a30ffd3ba7fa08d4aafcfd9f15594d20e732b8fdda43f02470d76b7ebfa467e6"

badCodeStr :: Text
badCodeStr = "(coin.coinbase \"a30ffd3ba7fa08d4aafcfd9f15594d20e732b8fdda43f02470d76b7ebfa467e6\" (read-keyset \"miner-keyset\") 23045.23) (coin.coinbase \"a30ffd3ba7fa08d4aafcfd9f15594d20e732b8fdda43f02470d76b7ebfa467e6\" (read-keyset \"miner-keyset\") (read-decimal \"reward\"))"

_testVersion :: ChainwebVersion
_testVersion = FastTimedCPM petersonChainGraph

minerKeys0 :: MinerKeys
minerKeys0 = MinerKeys $ mkKeySet
  ["f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
  "default"

_minerId0 :: MinerId
_minerId0 = MinerId "default miner"
