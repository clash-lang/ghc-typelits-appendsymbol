{-|
Copyright  :  (C) 2019, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

To use the plugin, add the

@
{\-\# OPTIONS_GHC -fplugin GHC.TypeLits.AppendSymbol \#-\}
@

pragma to the header of your file

-}

{-# LANGUAGE CPP           #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module GHC.TypeLits.AppendSymbol
  ( plugin )
where

-- external
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe                (catMaybes)
import GHC.TcPluginM.Extra       (evByFiat, tracePlugin)

-- GHC API
import FastString          (unpackFS)
import Outputable          (Outputable (..), (<+>), ($$), text)
import Plugins             (Plugin (..), defaultPlugin)
#if MIN_VERSION_ghc(8,6,0)
import Plugins             (purePlugin)
#endif
import TcEvidence          (EvTerm)
import TcPluginM           (TcPluginM, matchFam, tcPluginTrace)
import TcRnTypes
  (Ct, TcPlugin(..), TcPluginResult (..), ctEvidence, ctEvPred, isGivenCt,
   isWantedCt)
import TcType              (coreView, typeKind)
import Type
  (EqRel (NomEq), Kind, PredTree (EqPred), TyVar, classifyPredType, eqType)
import TyCoRep             (TyLit (StrTyLit), Type (..))
import TysWiredIn          (typeSymbolKind)
import TcTypeNats          (typeSymbolAppendTyCon)
#if MIN_VERSION_ghc(8,4,0)
import GHC.TcPluginM.Extra (flattenGivens)
#else
import TcPluginM           (zonkCt)
import Control.Monad       ((<=<))
#endif

-- | A constraint-solver plugin that witnesses the injectivity of
-- `GHC.TypeLits.AppendSymbol`.
--
-- To use the plugin, add
--
-- @
-- {\-\# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver \#-\}
-- @
--
-- To the header of your file.
plugin :: Plugin
plugin
  = defaultPlugin
  { tcPlugin = const $ Just normalisePlugin
#if MIN_VERSION_ghc(8,6,0)
  , pluginRecompile = purePlugin
#endif
  }

normalisePlugin :: TcPlugin
normalisePlugin = tracePlugin "ghc-typelits-appendsymbol"
  TcPlugin { tcPluginInit  = pure ()
           , tcPluginSolve = decideInjectiveAppendSymbol
           , tcPluginStop  = const (return ())
           }

decideInjectiveAppendSymbol
  :: ()
  -> [Ct]
  -- ^ Given
  -> [Ct]
  -- ^ Derived
  -> [Ct]
  -- ^ Wanted
  -> TcPluginM TcPluginResult
decideInjectiveAppendSymbol _ _givens _deriveds [] =
  return (TcPluginOk [] [])
decideInjectiveAppendSymbol _ givens  _deriveds wanteds = do
  -- GHC 7.10.1 puts deriveds with the wanteds, so filter them out
  let wanteds' = filter isWantedCt wanteds
  unit_wanteds <- catMaybes <$> mapM (runMaybeT . toAppendSymbol) wanteds'
  case unit_wanteds of
    [] -> return (TcPluginOk [] [])
    _  -> do
#if MIN_VERSION_ghc(8,4,0)
      unit_givens <- catMaybes <$> mapM (runMaybeT . toAppendSymbol) (givens ++ flattenGivens givens)
#else
      unit_givens <- catMaybes <$> mapM ((runMaybeT . toAppendSymbol) <=< zonkCt) givens
#endif
      sr <- solveAppend (unit_givens ++ unit_wanteds)
      tcPluginTrace "solved" (ppr sr)
      case sr of
        Simplified evs -> return (TcPluginOk (filter (isWantedCt . snd) evs) [])
        Impossible (ct,_,_) -> return (TcPluginContradiction [ct])

data AppendSymbol
  = Var TyVar
  | Symbol String
  | AppendSymbol AppendSymbol AppendSymbol

instance Outputable AppendSymbol where
  ppr (Symbol s)         = text s
  ppr (Var v)            = ppr v
  ppr (AppendSymbol a b) = text "AppendSymbol" <+> ppr a <+> ppr b

type SymbolEquality = (Ct,AppendSymbol,AppendSymbol)

data SimplifyResult
  = Simplified [(EvTerm,Ct)]
  | Impossible SymbolEquality

instance Outputable SimplifyResult where
  ppr (Simplified evs) = text "Simplified" $$ ppr evs
  ppr (Impossible eq)  = text "Impossible" <+> ppr eq

data AppendResult = Solved | Fail | Add [(TyVar,String)]

instance Outputable AppendResult where
  ppr Solved   = text "Solved"
  ppr Fail     = text "Fail"
  ppr (Add ks) = text "Draw" <+> ppr ks

solveAppend :: [SymbolEquality] -> TcPluginM SimplifyResult
solveAppend eqs = tcPluginTrace "solveAppend" (ppr eqs) >> simples [] [] eqs
 where
  simples
    :: [(TyVar,String)]
    -> [Maybe (EvTerm, Ct)]
    -> [SymbolEquality]
    -> TcPluginM SimplifyResult
  simples _ evs [] = return (Simplified (catMaybes evs))
  simples m evs (eq@(ct,u,v):eqs') = do
    ur <- unifyAppend m ct u v
    tcPluginTrace "simples result" (ppr ur)
    case ur of
      Solved -> simples m (((,) <$> evMagic ct <*> pure ct):evs) eqs'
      Fail   -> if null evs && null eqs'
                   then return (Impossible eq)
                   else simples m evs eqs'
      Add ks -> simples (ks++m) evs eqs'

unifyAppend
  :: [(TyVar,String)]
  -> Ct
  -> AppendSymbol
  -> AppendSymbol
  -> TcPluginM AppendResult
unifyAppend _ ct (AppendSymbol (Symbol prefix0) (Var v)) (Symbol s)
  | isGivenCt ct
  , let (prefix1,suffix) = splitAt (length prefix0) s
  , prefix0 == prefix1
  = return (Add [(v,suffix)])
unifyAppend _ ct (AppendSymbol (Var v) (Symbol suffix0)) (Symbol s)
  | isGivenCt ct
  , let (prefix,suffix1) = splitAt (length s - length suffix0) s
  , suffix0 == suffix1
  = return (Add [(v,prefix)])
unifyAppend m ct (Var v) (Symbol s0)
  | isWantedCt ct
  , Just s1 <- lookup v m
  = if s0 == s1 then return Solved else return Fail

unifyAppend _ _ _ _ = return (Add [])

-- Extract the Nat equality constraints
toAppendSymbol :: Ct -> MaybeT TcPluginM SymbolEquality
toAppendSymbol ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
  EqPred NomEq t1 t2
    | isSymbolKind (typeKind t1) || isSymbolKind (typeKind t2)
    -> (ct,,) <$> normaliseSymbol t1 <*> normaliseSymbol t2
  _ -> fail "Nothing"
 where
  isSymbolKind :: Kind -> Bool
  isSymbolKind = (`eqType` typeSymbolKind)

  normaliseSymbol :: Type -> MaybeT TcPluginM AppendSymbol
  normaliseSymbol ty | Just ty1 <- coreView ty = normaliseSymbol ty1
  normaliseSymbol (TyVarTy v) = pure (Var v)
  normaliseSymbol (LitTy (StrTyLit s)) = pure (Symbol (unpackFS s))
  normaliseSymbol (TyConApp tc [x,y])
    | tc == typeSymbolAppendTyCon
    = AppendSymbol <$> normaliseSymbol x <*> normaliseSymbol y
  normaliseSymbol (TyConApp tc tys) = do
    tyM  <- lift (matchFam tc tys)
    case tyM of
      Just (_,ty) -> normaliseSymbol ty
      _ -> fail "No AppendSymbol"
  normaliseSymbol _ = fail "No AppendSymbol"

-- Utils
evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> Just (evByFiat "ghc-typelits-appendsymbol" t1 t2)
    _                  -> Nothing
