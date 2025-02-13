-- |implementation of the NHORPO ordering
module Termination.NHORPO.Ordering where

import Prelude hiding ((&&),(||),and,or)

import Control.Monad (zipWithM)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT,runReaderT,asks)
import Data.List (permutations,(\\))
import Language.Hasmtlib (Equatable(..),Orderable(..),Boolean(..),and,or,false)

import Utils.Type (Accessor(..))
import Utils.RPO (IsStatus(..),RPOInfo(..))
import Utils.SMT (Constraint,(<&&>),(<&&),(<||>))
import Utils.FreshMonad (FreshM,freshVar)
import Typ.Type (Typ(..))
import Typ.Ops (applTyp)
import ATerm.Type (ATerm(..),AT(..),EId)
import ATerm.Ops (varLam,dbToVar,notIn,nonVersatile,versatile)

-- |A wrapper for NHORPO which makes it a correct constituent of a normal higher-order reduction ordering.
-- Both terms are assumed to be in beta-eta normal form.
nhorpoWrapper :: (Orderable a, IsStatus b, Equatable b) => RPOInfo a b -> ATerm -> ATerm -> FreshM Constraint
nhorpoWrapper rpoinfo s t = if typ s /= typ t
  then pure false
  else runReaderT (nhorpo s t) rpoinfo <&&> case term s of
    (AAp u (ATerm {term = AFV v, typ = a})) -> nhorpoWrapper rpoinfo u (varLam a v t)
    (AAp _ v) -> pure . bool $ nonVersatile v
    _ -> pure true

-- |Implementation of NHORPO. Both terms are assumed to be in beta-eta normal form.
-- The first argument determines whether the nonversatility precondition should be
-- checked. Unlike the definition in the paper, this implementation does not enforce
-- nonversatility for >=.
nhorpo :: (Orderable a, IsStatus b, Equatable b) =>
  ATerm -> ATerm -> ReaderT (RPOInfo a b) FreshM Constraint
nhorpo s@(ATerm {term = sTerm, typ = a}) t@(ATerm {term = tTerm, typ = b}) =
  typeOrder a b <&&> case sTerm of
    AFun f ss -> or <$> traverse (`nhorpoWeak` t) ss <||> case tTerm of
      (AFun g ts) -> funFunCase s f g ss ts
      (AAp u v) -> propA s ss [u,v]
      (ALam _ v) -> if 0 `notIn` v then nhorpo s v else pure false
      _ -> pure false
    AAp u v -> if versatile s
      then pure false
      else or <$> traverse (`nhorpoWeak` t) [u,v] <||> case tTerm of
        (AAp u' v') -> nhorpoMul [u,v] [u',v']
        _ -> pure false
    ALam c u -> if versatile s
      then pure false
      else do
        z <- lift freshVar
        let u' = dbToVar z 0 u
        nhorpoWeak u' t <||> case tTerm of
          (ALam d v) -> if c == d then nhorpo u v else pure false
          _ -> case applTyp b c of
            Just e -> nhorpoWeak u' (ATerm {term = AAp t (ATerm {term = AFV z, typ = c}), typ = e})
            _ -> pure false
    _ -> pure false

-- |case s = f(ss) > g(ts)
funFunCase :: (Orderable a, IsStatus b, Equatable b) =>
  ATerm -> EId -> EId -> [ATerm] -> [ATerm] -> ReaderT (RPOInfo a b) FreshM Constraint
funFunCase s f g ss ts = do
  st <- asks stat
  cp <- asks cPrec
  let mulLex = isLex (st ! f) <&& nhorpoLex s ss ts <||> isMul (st ! f) <&& nhorpoMul ss ts
  if f == g
    then mulLex
    else cp ! f >? cp ! g <&& propA s ss ts <||> (cp ! f === cp ! g && st ! f === st ! g) <&& mulLex
      

-- |property A from NHORPO's definition
propA :: (Orderable a, IsStatus b, Equatable b) =>
  ATerm -> [ATerm] -> [ATerm] -> ReaderT (RPOInfo a b) FreshM Constraint
propA s ss ts = and <$> traverse (\v -> nhorpo s v <||> or <$> traverse (`nhorpoWeak` v) ss) ts

-- |lexicographic extension of NHORPO
nhorpoLex :: (Orderable a, IsStatus b, Equatable b) =>
  ATerm -> [ATerm] -> [ATerm] -> ReaderT (RPOInfo a b) FreshM Constraint
nhorpoLex s ss ts = go ss ts where
  go [] _ = pure false
  go (_:_) [] = pure true
  go (si:ssr) (ti:ttr) = if si == ti
    then go ssr ttr
    else nhorpo si ti <&&> propA s ss ttr

-- |multiset extension of NHORPO
nhorpoMul :: (Orderable a, IsStatus b, Equatable b) =>
  [ATerm] -> [ATerm] -> ReaderT (RPOInfo a b) FreshM Constraint
nhorpoMul [] [] = pure false
nhorpoMul ss ts =
  and <$> traverse (\t -> or <$> traverse (\s -> nhorpo s t) (ss \\ ts)) (ts \\ ss)

-- |weak HORPO orientation (strict orientation or a suitable notion of equivalence)
nhorpoWeak :: (Orderable a, IsStatus b, Equatable b) =>
  ATerm -> ATerm -> ReaderT (RPOInfo a b) FreshM Constraint
nhorpoWeak s t = equivalentATerms s t <||> nhorpo s t

-- |equivalence of terms for weak orientations
equivalentATerms :: (Orderable a, IsStatus b, Equatable b) =>
  ATerm -> ATerm -> ReaderT (RPOInfo a b) FreshM Constraint
equivalentATerms as at = case (term as, term at) of
  (AFV v1, AFV v2) -> pure . bool $ v1 == v2
  (ADB i, ADB j) -> pure . bool $ i == j
  (ALam _ s, ALam _ t) -> equivalentATerms s t
  (AFun f ss, AFun g ts) -> do
    st <- asks stat
    cp <- asks cPrec
    let mulLex = isLex (st ! f) <&& (and <$> zipWithM equivalentATerms ss ts) <||>
                 isMul (st ! f) <&& (or <$> sequence  [and <$> zipWithM equivalentATerms ss ts'
                                                      | ts' <- permutations ts])
    if length ss == length ts
    then if f == g
         then mulLex
         else (cp ! f === cp ! g && st ! f === st ! g) <&& mulLex
    else pure false
  (AAp s t, AAp u v) -> equivalentATerms s u <&&> equivalentATerms t v
  (_,_) -> pure false

-- |A weak order on types for NHORPO as defined in the 2007 article on HORPO.
typeOrder :: Orderable a => Typ -> Typ -> ReaderT (RPOInfo a b) FreshM Constraint
typeOrder (Base a) (Base b) = do
  tp <- asks tPrec
  pure $ tp ! a >=? tp ! b
typeOrder (Ar a b) c = typeOrder b c <||> case c of
  (Ar d e) -> equivalentTypes a d <&&> typeOrder b e
  _ -> pure false
typeOrder _ _ = pure false

-- |equivalence of types for weak orientations
equivalentTypes :: Equatable a => Typ -> Typ -> ReaderT (RPOInfo a b) FreshM Constraint
equivalentTypes (Base a) (Base b) = do
  tp <- asks tPrec
  pure $ tp ! a === tp ! b
equivalentTypes (Ar a b) (Ar c d) = equivalentTypes a c <&&> equivalentTypes b d
equivalentTypes _ _ = pure false
