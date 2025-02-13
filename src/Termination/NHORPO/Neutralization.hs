{-# LANGUAGE OverloadedStrings #-}

-- |implementation of the higher-order interpretation "neutralization" from
-- Jouannaud & Rubio's 2015 ACM Trans. Comp. Log. article
module Termination.NHORPO.Neutralization where

import Control.Monad.State (State,runState,get,put)
import Data.List (subsequences)
import Data.List.Extra (nubOrdOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import Utils.Type (Id(..))
import Typ.Type (Typ(..),FTyp)
import Typ.Ops (arity)
import ATerm.Type (ATerm(..),AT(..),EId(..),AEquation(..),AES)
import ATerm.BetaEta (betaEta)
import Termination.NHORPO.Type

-- $setup
--
-- Setup code for examples:
--
-- >>> :set -XOverloadedStrings
-- >>> import Control.Monad.State (evalState)
-- >>> import Prettyprinter (pretty)
-- >>> import Typ.Parse (parseTyp)
-- >>> import Term.Type (Env(..))
-- >>> import Term.Parse (parseTerm)
-- >>> import ATerm.Ops (termToATerm)
-- >>> a = parseTyp "a"
-- >>> aa = parseTyp "a>a"
-- >>> a4 = parseTyp "(a>a)>a>a"
-- >>> env = Env { cs = [(Id "diff",a4),(Id "sin",aa)], fvs = [(Id "F",aa)] }
-- >>> ar = M.fromList [(Id "diff",1),(Id "sin",1)]
-- >>> fty = M.fromList [(Id "diff",([aa,a],a)),(Id "sin",([a],a))]

-- |Neutralization of level i N_i(s,<t1,...t2>)
-- I assume that no base type is named "$" (the minimal base type)
-- which is guaranteed by the TPTP parser.
--
-- >>> s = parseTerm env "λx:a.sin (F x)"
-- >>> pretty $ evalState (neutralization 1 (termToATerm ar s) []) S.empty
-- (λx0:a.sin(F x0)) ⊥<a>
neutralization :: Int -> ATerm -> [ATerm] -> State (Set EId) ATerm
neutralization i s ts
  | i < 0  = do
    bcs <- get
    let o = Base (Id "$")
        coerce = Coerce (typ s) o
    put (S.insert coerce bcs)
    pure $ ATerm {term = AFun coerce [s], typ = o}
  | i == 0 = pure s
  | i > 0  = case typ s of
      Base _ -> pure s
      Ar a b -> do
        bcs <- get
        let bot = Bot (map typ ts) a
            at = ATerm {term = AAp s (ATerm {term = AFun bot ts, typ = a}), typ = b}
        put (S.insert bot bcs)
        neutralization (i-1) at ts
  | otherwise = error "impossible case"

-- |full neutralization FN(s)
--
-- >>> s = parseTerm env "diff (λx:a.sin (F x))"
-- >>> nl = M.fromList [((Id "diff",0),1),((Id "sin",0),0)]
-- >>> ap = M.fromList [((Id "diff",0),[]),((Id "sin",0),[])]
-- >>> pretty . betaEta $ evalState (fullNeutralization nl ap (termToATerm ar s)) S.empty
-- diff_new(sin_new(F ⊥<a>))
fullNeutralization :: NeutralizationLevel -> ArgumentPosSubset -> ATerm -> State (Set EId) ATerm
fullNeutralization _ _ s@(ATerm {term = AFV _}) = pure s
fullNeutralization _ _ s@(ATerm {term = ADB _}) = pure s
fullNeutralization nLevel aPos s@(ATerm {term = AFun (Normal idt) ts}) = do
    let f (t,i) = do
          t' <- fullNeutralization nLevel aPos t
          us <- mapM (\k -> fullNeutralization nLevel aPos (ts !! k)) (aPos M.! (idt,i))
          neutralization (nLevel M.! (idt,i)) t' us
    ts' <- mapM f (zip ts [0..])
    pure s{term = AFun (New idt) ts'}
fullNeutralization nLevel aPos s@(ATerm {term = AAp u v}) = do
    u' <- fullNeutralization nLevel aPos u
    v' <- fullNeutralization nLevel aPos v
    pure s{term = AAp u' v'}
fullNeutralization nLevel aPos s@(ATerm {term = ALam a u}) = do
    u' <- fullNeutralization nLevel aPos u
    pure s{term = ALam a u'}
fullNeutralization _ _ (ATerm {term = AFun (New _) _}) = error "impossible case"
fullNeutralization _ _ (ATerm {term = AFun (Bot _ _) _}) = error "impossible case"
fullNeutralization _ _ (ATerm {term = AFun (Coerce _ _) _}) = error "impossible case"

-- |Lifts 'fullNeutralization' to equations.
fullNeutralizationEq :: NeutralizationLevel -> ArgumentPosSubset -> AEquation -> State (Set EId) AEquation
fullNeutralizationEq nl ap e = do
  nlhs <- betaEta <$> fullNeutralization nl ap (alhs e)
  nrhs <- betaEta <$> fullNeutralization nl ap (arhs e)
  pure AEq { alhs = nlhs, arhs = nrhs, aisRule = aisRule e}

-- |Lifts 'fullNeutralizationEq' to ESs.
fullNeutralizationES :: NeutralizationLevel -> ArgumentPosSubset -> AES -> State (Set EId) AES
fullNeutralizationES nl ap es = mapM (fullNeutralizationEq nl ap) es

-- |Compute all possible interpretations using full neutralization for a given ES.
--
-- >>> s = parseTerm env "diff (λx:a.sin (F x))"
-- >>> t = parseTerm env "F"
-- >>> e = AEq { alhs = termToATerm ar s, arhs = termToATerm ar t, aisRule = True}
-- >>> pretty $ map (\(x,_,_,_) -> x) (allInterpretations ar fty [e])
-- [ [diff_new(coerce<(a > a) > $>(λx0:a.sin_new(coerce<a > $>(F x0)))) → F]
-- , [diff_new(coerce<(a > a) > $>(λx0:a.sin_new(F x0))) → F]
-- , [diff_new(λx0:a.sin_new(coerce<a > $>(F x0))) → F]
-- , [diff_new(λx0:a.sin_new(F x0)) → F]
-- , [diff_new(sin_new(coerce<a > $>(F ⊥<a>))) → F]
-- , [ diff_new(sin_new(coerce<a > $>(F ⊥<(a > a) > a>((λx0:a.sin_new(coerce<a > $>(F x0))))))) → F ]
-- , [diff_new(sin_new(F ⊥<a>)) → F]
-- , [diff_new(sin_new(F ⊥<(a > a) > a>((λx0:a.sin_new(F x0))))) → F] ]
allInterpretations :: Map Id Int -> Map Id FTyp -> AES -> [(AES,[EId],NeutralizationLevel,ArgumentPosSubset)]
allInterpretations ar fty es = nubOrdOn (\(x,_,_,_) -> x)
  [ (aes,S.toList bcs,nl,ap)
  | nl <- allCombinationsOfNeutralizationLevels ar fty
  , ap <- allCombinationsOfArgumentPositionSubsets ar fty
  , (aes,bcs) <- [runState (fullNeutralizationES nl ap es) S.empty]
  ]

-- |Given a map from function symbols to their arities as well as a map from function symbols
-- to their flattened types, this function generates all possible combinations of neutralization levels.
--
-- >>> allCombinationsOfNeutralizationLevels ar fty
-- [fromList [((Id "diff",0),-1),((Id "sin",0),-1)],fromList [((Id "diff",0),-1),((Id "sin",0),0)],fromList [((Id "diff",0),0),((Id "sin",0),-1)],fromList [((Id "diff",0),0),((Id "sin",0),0)],fromList [((Id "diff",0),1),((Id "sin",0),-1)],fromList [((Id "diff",0),1),((Id "sin",0),0)]]
allCombinationsOfNeutralizationLevels :: Map Id Int -> Map Id FTyp -> [NeutralizationLevel]
allCombinationsOfNeutralizationLevels ar fty = [M.fromAscList $ zip args is | is <- go argArities] where
  (args,argArities) = unzip [ ((idt,i), arity (as !! i))
                            | (idt,(as,_)) <- M.toAscList fty
                            , i <- [0..(ar M.! idt)-1] ]
  go [] = [[]]
  go (n:ns) = do
    i <- [-1..n]
    is <- go ns
    return $ i : is

-- |Given a map from function symbols to their arities,
-- this fnction generates all possible combinations of subsets of arguments positions.
--
-- >>> allCombinationsOfArgumentPositionSubsets ar fty
-- [fromList [((Id "diff",0),[]),((Id "sin",0),[])],fromList [((Id "diff",0),[0]),((Id "sin",0),[])]]
allCombinationsOfArgumentPositionSubsets :: Map Id Int -> Map Id FTyp -> [ArgumentPosSubset]
allCombinationsOfArgumentPositionSubsets ar fty = [M.fromAscList $ zip args iss | iss <- go arities] where
  (args,arities) = unzip [ ((idt,i),m)
                         | (idt,n) <- M.toAscList ar
                         , let (as,_) = fty M.! idt
                         , i <- [0..(ar M.! idt)-1]
                         , let m = if arity (as !! i) == 0 then 0 else n] -- no argumentPositionSubset for base type arguments
  go [] = [[]]
  go (n:ns) = do
    aps <- subsequences [0..n-1]
    apss <- go ns
    return $ aps : apss
