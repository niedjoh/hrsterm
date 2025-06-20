-- |implementation of NCPO for beta-eta long normal forms
module Termination.NCPO.OrderingLnf(ncpoWrapperLnf) where

import Prelude hiding ((&&),(||),and,or,not)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT,runReaderT,asks)
import Data.List ((\\))
import Data.Set (Set)
import qualified Data.Set as S
import Language.Hasmtlib (Equatable(..),Orderable(..),Boolean(..),and,or,false)

import Utils.Type (Accessor(..))
import Utils.SMT (Constraint,(<&&>),(<&&),(<||),(<||>))
import Utils.FreshMonad (FreshM,freshVar)
import Typ.Type (Typ(..))
import Typ.Ops (base,flattenTyp,unflattenTyp,applTyp,equatableByTypApp,posOf)
import Term.Type (Var)
import ATerm.Type (ATerm(..),AT(..),EId)
import ATerm.Ops (dbToVar,strip,unstrip,danglingDB)
import ATerm.BetaEta (betaElApp,etaExpandVar)
import Utils.RPO (IsStatus(..),RPOInfo(..))
import Termination.NCPO.Type

-- |custom data type which indicates whether types should be compared in a given NCPO call
data TypeComparison = Compare | NoCompare

type CompareFun a b = ATerm -> ATerm -> ReaderT (CPOInfo a b) FreshM Constraint
type ApplCompareFun a b = ATerm -> ReaderT (CPOInfo a b) FreshM Constraint

-- |if-like function for 'TypeComparison'
ifCompare :: TypeComparison -> a -> a -> a
ifCompare Compare x _ = x
ifCompare NoCompare _ y = y

-- |A wrapper for NCPO. Both terms are assumed to be in beta-eta long normal form.
ncpoWrapperLnf :: (Orderable a, IsStatus b, Equatable b) => CPOInfo a b -> ATerm -> ATerm -> FreshM Constraint
ncpoWrapperLnf cpoinfo s t = runReaderT (ncpo Compare S.empty s t) cpoinfo

-- |Implementation of NCPO. The first term is assumed to be in beta-eta long normal form.
ncpo :: (Orderable a, IsStatus b, Equatable b) =>
  TypeComparison -> Set (Var,Typ) -> ATerm -> ATerm -> ReaderT (CPOInfo a b) FreshM Constraint
ncpo typeComp vars s@(ATerm {term = sTerm, typ = a}) t@(ATerm {term = tTerm, typ = b}) = do
  ifCompare typeComp (weakTypeOrder a b) (pure true) <&&> case sTerm of
    AFun f ss -> if any (== t) (S.map (\(v,b) -> etaExpandVar v (flattenTyp b)) vars)
      then pure true
      else or <$> traverse (\u -> bawo u t) ss <||>  case tTerm of
        (AFun g ts) -> funFunCase vars s f g ss ts
        (AAFV var ts) -> if all (\ti -> any (\(v,b) -> ti == etaExpandVar v (flattenTyp b)) vars) ts
          then pure false
          else and <$> traverse (ncpo NoCompare vars s) (etaExpandVar var (map typ ts,b) : ts)
        (ALam c v) -> do
          z <- lift freshVar
          ncpo NoCompare (S.insert (z,c) vars) s (dbToVar z 0 v)
        _ -> pure false
    ALam c u -> do
      z <- lift freshVar
      let u' = dbToVar z 0 u
      ncpoWeak Compare vars u' t <||> case tTerm of
        ALam d v -> do
          let v' = dbToVar z 0 v
          if c == d
            then ncpo NoCompare vars u' v'
            else ncpo NoCompare vars s v'
        _ -> pure false
    _ -> pure false
  
-- |case s = f(ss) > g(ts) where f is big
funFunCase :: (Orderable a, IsStatus b, Equatable b) =>
  Set (Var,Typ) -> ATerm -> EId -> EId -> [ATerm] -> [ATerm] -> ReaderT (CPOInfo a b) FreshM Constraint
funFunCase vars s f g ss ts = do
  st <- asks (stat . rpoInfo)
  cp <- asks (cPrec . rpoInfo)
  let mulLex = isLex (st ! f) <&& ncpoLex (sswo vars) (ncpo NoCompare vars s) ss ts <||>
               isMul (st ! f) <&& ncpoMul (sswo vars) ss ts
  if f == g
    then mulLex
    else cp ! f >? cp ! g <&& (and <$> traverse (ncpo NoCompare vars s) ts) <||>
         (cp ! f === cp ! g && st ! f === st ! g) <&& mulLex

-- |lexicographic extension of NCPO generalized by comparison functions
ncpoLex :: (Orderable a, IsStatus b, Equatable b) =>
  CompareFun a b -> ApplCompareFun a b -> [ATerm] -> [ATerm] -> ReaderT (CPOInfo a b) FreshM Constraint
ncpoLex comp applComp = go where
  go [] _ = pure false
  go _ [] = pure false
  go (si:ssr) (ti:ttr) = if si == ti
    then go ssr ttr
    else comp si ti <&&> (and <$> traverse applComp ttr)

-- |multiset extension of NCPO generalized by comparison function
ncpoMul :: (Orderable a, IsStatus b, Equatable b) =>
  CompareFun a b -> [ATerm] -> [ATerm] -> ReaderT (CPOInfo a b) FreshM Constraint
ncpoMul _ [] [] = pure false
ncpoMul comp ss ts =
  and <$> traverse (\t -> or <$> traverse (\s -> comp s t) (ss \\ ts)) (ts \\ ss)

-- |weak NCPO orientation (reflexive closure)
ncpoWeak :: (Orderable a, IsStatus b, Equatable b) =>
 TypeComparison  -> Set (Var,Typ) -> ATerm -> ATerm -> ReaderT (CPOInfo a b) FreshM Constraint
ncpoWeak typComp vars s t = if s == t
  then pure true
  else ncpo typComp vars s t

-- |A strict order on types for NCPO as defined in the 2015 LMCS article
typeOrder :: Orderable a => Typ -> Typ -> ReaderT (CPOInfo a b) FreshM Constraint
typeOrder (Base a) (Base b) = do
  tp <- asks (tPrec . rpoInfo)
  pure $ tp ! a >? tp ! b
typeOrder (Ar a b) c = if b == c
  then pure true
  else case c of
    Ar d e -> if a == d
      then typeOrder b e
      else pure false
    _ -> pure false
typeOrder _ _ = pure false

-- |reflexive closure of 'typeOrder'
weakTypeOrder :: Orderable a => Typ -> Typ -> ReaderT (CPOInfo a b) FreshM Constraint
weakTypeOrder a b = if a == b
  then pure true
  else typeOrder a b

-- |The two arguments are connected by the composition of the following relations:
-- * reflexive closure of basic subterm relation
-- * reflexive closure of accessibility relation
-- * weak orient with NCPO
--
-- Note that we only allow to proceed to subterms via "nonversatile paths"
bawo :: (Orderable a, IsStatus b, Equatable b) => ATerm -> ATerm -> ReaderT (CPOInfo a b) FreshM Constraint
bawo s t = awo s t <||> go s t false where
  varCond u = not . bool $ danglingDB u
  go u@(ATerm {term = AFun _ us, typ = Base idt}) v recCase = do
    basic <- asks isBasic
    ((recCase && basic ! idt && varCond v) <&& awo u v) <||> (or <$> traverse (\u' -> go u' v true) us)
  go (ATerm {term = AFun _ us}) v _ = or <$> traverse (\u' -> go u' v true) us
  go u@(ATerm {term = ALam _ w}) v _ = go w v true
  go _ _ _ = pure false

-- |accessibility subterm relation with generic compare function for base case
accSubt :: CompareFun a b -> ATerm -> ATerm -> ReaderT (CPOInfo a b) FreshM Constraint
accSubt comp (ATerm {term = AFun f us}) t = do
  acc <- asks isAccessible
  let fun i = let ui = us !! i in acc ! (f,i) <&& (comp ui t <||> accSubt comp ui t)
  or <$> traverse fun [0..length us - 1]
accSubt _ _ _ = pure false

-- |The two arguments are connected by the composition of the following relations:
-- * reflexive closure of accessibility relation
-- * weak orient with NCPO
--
-- Note that we only allow to proceed to subterms for applied function symbols
awo :: (Orderable a, IsStatus b, Equatable b) => ATerm -> ATerm -> ReaderT (CPOInfo a b) FreshM Constraint
awo s t = ncpoWeak Compare S.empty s t <||> accSubt (ncpoWeak Compare S.empty) s t

-- |structurally smaller + weak orient
--
-- Note that we only allow to proceed to subterms via "nonversatile paths"
-- (nonversatility is preserved by applying a nonversatile term to arbitrary terms)
sswo :: (Orderable a, IsStatus b, Equatable b) => Set (Var,Typ) -> ATerm -> ATerm -> ReaderT (CPOInfo a b) FreshM Constraint
sswo vars s@(ATerm {typ = st}) t = ncpoWeak Compare S.empty s t <||> (bool (base st) <&& accSubt comp s t) where
  (as, a) = flattenTyp st
  idtA = case a of
    Base idt -> idt
    _ -> error "impossible case"
  comp u@(ATerm {typ = ut}) _ = do
    let (bs,b) = flattenTyp ut
    case equatableByTypApp (as,a) (bs,b) of
      Nothing ->
        pure false
      Just cs -> do
        let candidateVars = [filter ((== c) . snd) (S.toList vars) | c <- cs]
        if [] `elem` candidateVars || any (\c -> bool $ posOf idtA c /= S.empty) cs
          then pure false
          else do
            let possibleVarLists = allPossibilities candidateVars
                f xs = pure . bool $ (betaElApp u (map (\(x,c) -> etaExpandVar x (flattenTyp  c)) xs)) == t -- TODO possible? ncpoWeak Compare S.empty (betaElApp u (map (\(x,c) -> etaExpandVar x (flattenTyp  c)) xs)) t   
            or <$> traverse f possibleVarLists
  allPossibilities [] = [[]]
  allPossibilities (xs:xss) = concat [map (x:) (allPossibilities xss) | x <- xs]
