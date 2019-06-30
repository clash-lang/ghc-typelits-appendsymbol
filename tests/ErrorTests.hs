{-# LANGUAGE CPP, DataKinds, ScopedTypeVariables, TypeOperators,
             TypeApplications, TypeFamilies, TemplateHaskell #-}
#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE NoStarIsType #-}
#endif
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.AppendSymbol #-}

module ErrorTests where

import Control.Exception.Base
import Data.Proxy
import GHC.TypeLits
import Test.Inspection

testFail1
  :: forall dom
   . AppendSymbol dom "_rst" ~ "system_rts"
  => Proxy dom
  -> Proxy "system"
testFail1 = id

testFail1Result :: Result
testFail1Result = $(inspectTest ('testFail1 `doesNotUse` 'typeError))

testFail2
  :: forall dom
   . AppendSymbol "rst_" dom ~ "rts_system"
  => Proxy dom
  -> Proxy "system"
testFail2 = id

testFail2Result :: Result
testFail2Result = $(inspectTest ('testFail2 `doesNotUse` 'typeError))
