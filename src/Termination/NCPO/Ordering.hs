-- |implementation of the NCPO ordering
module Termination.NCPO.Ordering where

import Prelude hiding ((&&),(||),and,or,not)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT,runReaderT,asks)
import Data.List ((\\))
import Data.Set (Set)
import qualified Data.Set as S
import Language.Hasmtlib (Equatable(..),Orderable(..),Boolean(..),and,or,false)

import Utils.Type (Accessor(..))
import Utils.SMT (Constraint,(<&&>),(<&&),(<||>))
import Utils.FreshMonad (FreshM,freshVar)
import Typ.Type (Typ(..))
import Typ.Ops (base,flattenTyp,applTyp,equatableByTypApp,posOf)
import Term.Type (Var)
import ATerm.Type (ATerm(..),AT(..),EId)
import ATerm.Ops (dbToVar,versatile,strip,unstrip,danglingDB)
import ATerm.BetaEta (betaEta)
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

-- |A wrapper for NCPO. Both terms are assumed to be in beta-eta normal form.
ncpoWrapper :: (Orderable a, IsStatus b, Equatable b) => CPOInfo a b -> ATerm -> ATerm -> FreshM Constraint
ncpoWrapper cpoinfo s t = runReaderT (ncpo Compare S.empty s t) cpoinfo

-- |Implementation of NCPO. The first term is assumed to be in beta-eta normal form.
ncpo :: (Orderable a, IsStatus b, Equatable b) =>
  TypeComparison -> Set (Var,Typ) -> ATerm -> ATerm -> ReaderT (CPOInfo a b) FreshM Constraint
ncpo typeComp vars s@(ATerm {term = sTerm, typ = a}) t@(ATerm {term = tTerm, typ = b}) = do
  small <- asks isSmall
  ifCompare typeComp (weakTypeOrder a b) (pure true) <&&> case sTerm of
    AFun f ss -> (not (small ! f) <&& bigCase vars s f ss t) <||> (small ! f <&& smallCase vars s f ss t)
    AAp u v -> if versatile s
      then pure false
      else ncpoWeak NoCompare vars u t <||> ncpoWeak Compare vars v t <||> case tTerm of
        AAp u' v' -> (if u == u' then ncpo NoCompare vars v v' else pure false) <||> (f u' <&&> f v') where
            f x = ncpo Compare vars u x <||>
                  ncpoWeak Compare vars v x <||>
                  ncpo Compare vars s x
        ALam _ w -> do
          z <- lift freshVar
          ncpo NoCompare vars s (dbToVar z 0 w)
        AFun g vs -> small ! g <&& (and <$> traverse (ncpo Compare vars s) vs)
        AFV var -> if (var,b) `S.member` vars then pure true else pure false
        _ -> pure false
    ALam c u -> if versatile s
      then pure false
      else do
        z <- lift freshVar
        let u' = dbToVar z 0 u
        ncpoWeak Compare vars u' t <||> lambdaSubtEta vars u' t c z <||> case tTerm of
          ALam d v -> do
             let v' = dbToVar z 0 v
             if c == d
               then ncpo NoCompare vars u' v'
               else ncpo NoCompare vars s v'
          AFun g vs -> small ! g <&& (and <$> traverse (ncpo Compare vars s) vs)
          AFV var -> if (var,b) `S.member` vars then pure true else pure false
          _ -> pure false
    _ -> pure false

-- |f is big function symbol
bigCase :: (Orderable a, IsStatus b, Equatable b) =>
  Set (Var,Typ) -> ATerm -> EId -> [ATerm] -> ATerm -> ReaderT (CPOInfo a b) FreshM Constraint
bigCase vars s f ss t@(ATerm {term = tTerm, typ = b}) =
  (or <$> traverse (\u -> bawo u t) ss) <||> case tTerm of
    (AFun g ts) -> funFunCaseBig vars s f g ss ts
    (AAp u v) -> and <$> traverse (ncpo NoCompare vars s) [u,v]
    (ALam c v) -> do
      z <- lift freshVar
      ncpo NoCompare (S.insert (z,c) vars) s (dbToVar z 0 v)
    (AFV var) -> if (var,b) `S.member` vars then pure true else pure false
    _ -> pure false

-- |f is small function symbol
smallCase :: (Orderable a, IsStatus b, Equatable b) =>
  Set (Var,Typ) -> ATerm -> EId -> [ATerm] -> ATerm -> ReaderT (CPOInfo a b) FreshM Constraint
smallCase vars s f ss t@(ATerm {term = tTerm, typ = b}) =
  (or <$> traverse (\u -> ncpoWeak Compare S.empty u t) ss) <||> case tTerm of
    (AFun g ts) -> funFunCaseSmall vars s f g ss ts
    (AAp u v) -> and <$> traverse (ncpo Compare vars s) [u,v]
    (AFV var) -> if (var,b) `S.member` vars then pure true else pure false
    _ -> pure false

-- |case s = f(ss) > g(ts) where f is big
funFunCaseBig :: (Orderable a, IsStatus b, Equatable b) =>
  Set (Var,Typ) -> ATerm -> EId -> EId -> [ATerm] -> [ATerm] -> ReaderT (CPOInfo a b) FreshM Constraint
funFunCaseBig vars s f g ss ts = do
  st <- asks (stat . rpoInfo)
  cp <- asks (cPrec . rpoInfo)
  let mulLex = isLex (st ! f) <&& ncpoLex (sswo vars) (ncpo NoCompare vars s) ss ts <||>
               isMul (st ! f) <&& ncpoMul (sswo vars) ss ts
  if f == g
    then mulLex
    else cp ! f >? cp ! g <&& (and <$> traverse (ncpo NoCompare vars s) ts) <||>
         (cp ! f === cp ! g && st ! f === st ! g) <&& mulLex

-- |case s = f(ss) > g(ts) where f is small
funFunCaseSmall :: (Orderable a, IsStatus b, Equatable b) =>
  Set (Var,Typ) -> ATerm -> EId -> EId -> [ATerm] -> [ATerm] -> ReaderT (CPOInfo a b) FreshM Constraint
funFunCaseSmall vars s f g ss ts = do
  st <- asks (stat . rpoInfo)
  cp <- asks (cPrec . rpoInfo)
  let mulLex = isLex (st ! f) <&& ncpoLex (ncpo Compare S.empty) (\_ -> pure true) ss ts <||>
               isMul (st ! f) <&& ncpoMul (ncpo Compare S.empty) ss ts
      sBiggerThanArgs = and <$> traverse (ncpo Compare vars s) ts
  if f == g
    then mulLex <&&> sBiggerThanArgs
    else cp ! f >? cp ! g <&& sBiggerThanArgs <||>
         (cp ! f === cp ! g && st ! f === st ! g) <&& mulLex <&&> sBiggerThanArgs

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

-- |Case 11 from NHORPO
lambdaSubtEta :: (Orderable a, IsStatus b, Equatable b) =>
  Set (Var,Typ) -> ATerm -> ATerm -> Typ -> Var ->  ReaderT (CPOInfo a b) FreshM Constraint
lambdaSubtEta vars u' t@(ATerm {typ=b}) c z = case applTyp b c of
  Just a -> ncpoWeak Compare vars u' (ATerm {term = AAp t zTerm, typ=a}) where
    zTerm = ATerm {term = AFV z, typ=c}
  Nothing -> pure false

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
  go u@(ATerm {term = AAp u1 u2, typ = Base idt}) v recCase = do
    basic <- asks isBasic
    if versatile u
      then (recCase && basic ! idt && varCond v) <&& awo u v
      else ((recCase && basic ! idt && varCond v) <&& awo u v) <||> go u1 v true <||> go u2 v true
  go u@(ATerm {term = AAp u1 u2}) v _ =
    if versatile u
      then pure false
      else go u1 v true <||> go u2 v true
  go u@(ATerm {term = AFun _ us, typ = Base idt}) v recCase = do
    basic <- asks isBasic
    ((recCase && basic ! idt && varCond v) <&& awo u v) <||> (or <$> traverse (\u' -> go u' v true) us)
  go (ATerm {term = AFun _ us}) v _ = or <$> traverse (\u' -> go u' v true) us
  go u@(ATerm {term = ALam _ w}) v _ =
    if versatile u
      then pure false
      else go w v true
  go _ _ _ = pure false

-- |accessibility subterm relation with generic compare function for base case
accSubt :: CompareFun a b -> (ATerm,[ATerm]) -> ATerm -> ReaderT (CPOInfo a b) FreshM Constraint
accSubt comp (ATerm {term = AFun f us1},us2) t = do
  acc <- asks isAccessible
  let us = us1 ++ us2
  let fun i = let ui = us !! i in acc ! (f,i) <&& (comp ui t <||> accSubt comp (strip ui) t)
  or <$> traverse fun [0..length us - 1]
accSubt _ _ _ = pure false

-- |The two arguments are connected by the composition of the following relations:
-- * reflexive closure of accessibility relation
-- * weak orient with NCPO
--
-- Note that we only allow to proceed to subterms via "nonversatile paths"
awo :: (Orderable a, IsStatus b, Equatable b) => ATerm -> ATerm -> ReaderT (CPOInfo a b) FreshM Constraint
awo s t = ncpoWeak Compare S.empty s t <||> accSubt (ncpoWeak Compare S.empty) (strip s) t

-- |structurally smaller + weak orient
--
-- Note that we only allow to proceed to subterms via "nonversatile paths"
-- (nonversatility is preserved by applying a nonversatile term to arbitrary terms)
sswo :: (Orderable a, IsStatus b, Equatable b) => Set (Var,Typ) -> ATerm -> ATerm -> ReaderT (CPOInfo a b) FreshM Constraint
sswo vars s@(ATerm {typ = st}) t = ncpoWeak Compare S.empty s t <||> (bool (base st) <&& accSubt comp (strip s) t) where
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
                f xs = case unstrip (u,map (\(x,c) -> ATerm {term = AFV x, typ = c}) xs) of
                  Just u' -> bool (betaEta u' == u') <&& ncpoWeak Compare S.empty u' t
                  Nothing -> error "impossible case"
            or <$> traverse f possibleVarLists
  allPossibilities [] = [[]]
  allPossibilities (xs:xss) = concat [map (x:) (allPossibilities xss) | x <- xs]

                            
