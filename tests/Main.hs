{-# LANGUAGE CPP, DataKinds, TypeOperators, TypeApplications, TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE NoStarIsType #-}
#endif
{-# OPTIONS_GHC -fplugin GHC.TypeLits.AppendSymbol #-}

import Data.List (isInfixOf)
import Data.Proxy
import Data.Type.Bool
import Control.Exception
import Test.Inspection
import Test.Tasty
import Test.Tasty.HUnit

import ErrorTests

import GHC.TypeLits

test1
  :: (AppendSymbol dom "_rst" ~ "system_rst")
  => Proxy dom
  -> Proxy "system"
test1 = id

test2
  :: (AppendSymbol "rst_" dom ~ "rst_system")
  => Proxy dom
  -> Proxy "system"
test2 = id

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ghc-typelits-appendsymbol"
  [ testGroup "Basic functionality"
    [ testCase "AppendSymbol x \"B\" ~ \"AB\" implies x ~ \"A\"" $
      show (test1 (Proxy @"system")) @?=
      "Proxy"
    , testCase "AppendSymbol \"A\" y ~ \"AB\" implies y ~ \"B\"" $
      show (test2 (Proxy @"system")) @?=
      "Proxy"
    ]
  , testGroup "errors"
    [ testCase "AppendSymbol x \"B\" ~ \"AC\" does not imply x ~ \"A\"" $
        shouldFail testFail1Result
    , testCase "AppendSymbol \"A\" y ~ \"DB\" does not imply y ~ \"B\"" $
        shouldFail testFail2Result
    ]
  ]

shouldFail :: Result -> Assertion
shouldFail (Failure _) = return ()
shouldFail (Success _) = assertFailure "No type error"
