{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

-- |solver for NCPO
module Termination.NCPO.Solver where

import Prelude hiding ((&&),(||),and,or,not)

import Control.Monad (when)
import Control.Monad.Trans.State (evalState)
import Data.Default (def)
import Data.List (groupBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Language.Hasmtlib as SMT
import Language.Hasmtlib (Orderable(..),Boolean(..),and)
import Prettyprinter (line,vsep,hsep,pretty,(<+>),Doc,line,punctuate,hsep)

import Utils.RPO
  ( TypePrecedence(..)
  , Precedence(..)
  , Status(..)
  , RPOInfo(..)
  , tPrecToList
  , cPrecToLists
  , maxArities
  )
import Utils.Type (Id(..),Accessor(..))
import Utils.SMT (SMTSolver(..),Constraint,smtVarMap)
import Typ.Type (Typ(..),FTyp,BaseTypes)
import Typ.Ops (flattenTyp,unflattenTyp,posOf,posPos,sPos)
import Term.Type (ConstTypeMap)
import qualified Term.Ops as TO
import Term.BetaEta (etaExpand)
import Equation.Type (Equation(..),ES)
import Equation.Ops (aritiesES)
import ATerm.Type (AES,AEquation(..),EId(..))
import ATerm.Ops (termToATerm,lnfTermToATerm)
import Termination.NCPO.Type
import Termination.NCPO.Ordering (ncpoWrapper)
import Termination.NCPO.OrderingLnf (ncpoWrapperLnf)

-- |result of termination proof attempt by NCPO
data NCPORes = NCPORes
  { status :: Bool
  , mSol :: Maybe CPOSolution
  , algRep :: AES
  }
  
-- |Check termination of an HRS using NCO
checkTermination :: SMTSolver -> Bool -> Bool -> BaseTypes -> ConstTypeMap -> ES -> IO NCPORes
checkTermination (Solver _ s _) debug betaEtaLong bts constTyM hrs = do
  let ar = M.union (aritiesES hrs) (maxArities constTyM)
      fty = M.map flattenTyp constTyM
      conv s = if betaEtaLong then lnfTermToATerm (etaExpand s (flattenTyp (TO.typ s))) else termToATerm ar s
      arep = map (\e -> AEq { alhs = conv (lhs e), arhs = conv (rhs e), aisRule = isRule e})
             hrs
      cs = map Normal (M.keys fty)
      afty = M.mapKeysMonotonic Normal fty
  (res,msol) <- SMT.solveWith (if debug then SMT.solver (SMT.debugging def s) else SMT.solver s) $ do
    SMT.setLogic "ALL"
    st <- Stat <$> smtVarMap @SMT.BoolSort cs
    tp <- TPrec <$> smtVarMap @SMT.IntSort bts
    cp <- Prec <$> smtVarMap @SMT.IntSort cs
    basic <- Basic <$> smtVarMap @SMT.BoolSort bts
    acc <- Acc <$> smtVarMap @SMT.BoolSort [(c,i) | c <- cs, i <- [0..(length . fst $ afty M.! c) - 1]]
    small <- Small <$> smtVarMap @SMT.BoolSort cs
    let rInfo = RPOInfo { tPrec = tp, stat = st, cPrec = cp }
    let cpoinfo = CPOInfo { rpoInfo = rInfo, bTypes = bts, isBasic = basic, isAccessible = acc, isSmall = small }
    mapM_ (SMT.assert . basicCond tp acc basic bts cs afty) bts
    mapM_ SMT.assert [accessibleCond tp acc c i a bId | c <- cs, (as,Base bId) <- [afty M.! c], (a,i) <- zip as [0..]]
    when (not betaEtaLong) $ do
      mapM_ SMT.assert [not (cp ! c1 >? cp ! c2) || small ! c2 || not (small ! c1) | c1 <- cs, c2 <- cs, c1 /= c2]
      mapM_ (SMT.assert . smallCond tp acc small ar afty) cs
    let order = if betaEtaLong then ncpoWrapperLnf else ncpoWrapper
        constraints = evalState (mapM (\e -> order cpoinfo (alhs e) (arhs e)) arep) 0
    mapM_ SMT.assert constraints
    return (tp,st,cp,basic,acc,small)
  let boolRes = case res of
        SMT.Sat -> True
        SMT.Unsat -> False
        SMT.Unknown -> False
  return $ NCPORes { status = boolRes, mSol = msol, algRep = arep }

-- |Determines whether all base type occuring in a type are
-- less than or equal than a given base type.
baseTypeLeq :: Orderable a => TypePrecedence a -> Id -> Typ -> Constraint
baseTypeLeq tp idt1 (Base idt2) = tp ! idt1 >=? tp ! idt2
baseTypeLeq tp idt (Ar a b) = baseTypeLeq tp idt a && baseTypeLeq tp idt b

-- |transitive closure of the strict type order extended by the left subterm relation
-- as defined for CPO in the 2015 LMCS article
-- TODO this should be transitive but I have not written down a proof
extendedTypeOrder :: Orderable a => TypePrecedence a -> Typ -> Typ -> Constraint
extendedTypeOrder tp (Base a) (Base b) = do
  tp ! a >? tp ! b
extendedTypeOrder tp (Ar a b) c
  | a == c    = true
  | b == c    = true
  | otherwise = extendedTypeOrder tp a c || extendedTypeOrder tp b c || case c of
    Ar d e -> if a == d
      then extendedTypeOrder tp b e
      else false
    _ -> false
extendedTypeOrder _ _ _ = false

-- |Generates the condition for an argument of a constant is accessible.
-- arguments:
-- * type precedence
-- * accessible map
-- * identifier of the constant
-- * index of the argument
-- * type of the argument
-- * base return type of constant
accessibleCond :: Orderable a => TypePrecedence a -> Acc Constraint -> EId -> Int -> Typ -> Id -> Constraint
accessibleCond tp acc fId i a bId =
  not (acc ! (fId,i)) || (bool (posOf bId a `S.isSubsetOf` posPos a) && baseTypeLeq tp bId a)

-- |Generates the condition for a base type to be basic.
-- arguments:
-- * type precedence
-- * accessible map
-- * basic map
-- * list of base type names
-- * list of constant names
-- * map to access types of constants
-- * identifier of the base type
basicCond :: Orderable a => TypePrecedence a -> Acc Constraint -> Basic Constraint ->
  [Id] -> [EId] -> Map EId FTyp -> Id -> Constraint
basicCond tp acc basic baseTypes cs cTyps idt  = not (basic ! idt) ||
  (and . map (\b -> not (tp ! b <? tp ! idt) || basic ! b) . filter (/= idt) $ baseTypes) &&
  (and . map f $ relevantAccs)
  where
    f (c,Base idt',i) = not (acc ! (c,i)) || bool (idt == idt') || basic ! idt'
    f (c,_,i) = not (acc ! (c,i))
    relevantAccs = [(c,a,i) | c <- cs, (as,Base idt') <- [cTyps M.! c], idt == idt', (a,i) <- zip as [0..]]

-- |Conditions on the types of small constants.
-- arguments:
-- * type precedence
-- * accessible map
-- * small symbol map
-- * arity of functino symbols
-- * map to access types of constants
smallCond :: Orderable a => TypePrecedence a -> Acc Constraint -> Small Constraint -> Map Id Int -> Map EId FTyp -> EId -> Constraint
smallCond tp acc small ar afty eid@(Normal idt) = let
    (as,a) = afty M.! eid
    aId = case a of
      Base idt' -> idt'
      _ -> error "impossible case"
    arity = ar M.! idt
    sortCase b = baseTypeLeq tp aId b && (bool . S.null $ sPos aId b)
    arrowCase cs b = baseTypeLeq tp aId b && extendedTypeOrder tp (unflattenTyp (cs,a)) b
  in not (small ! eid) || case splitAt arity as of
    (as1,[]) -> and . map sortCase $ as1
    (as1,as2) -> (and . map (\i -> not (acc ! (eid,i))) $ [0..length as - 1]) && (and . map (arrowCase as2) $ as1)
smallCond _ _ _ _ _ (New _) = error "impossible case"
smallCond _ _ _ _ _ (Bot _ _) = error "impossible case"
smallCond _ _ _ _ _ (Coerce _ _) = error "impossible case"

-- |Print the result of a solution attempt to a termination check using NCPO.
resultDoc :: Bool -> NCPORes -> ES -> Doc ann
resultDoc betaEtaLong res hrs = let
   prettyTypPrec prec =
     hsep . punctuate " >" $ [ hsep . punctuate " ~" $ [pretty idt | idt <- eqs]
                             | eqs <- tPrecToList prec]
   prettyConstPrec prec st = vsep
     [hsep . punctuate " >" $ [ hsep . punctuate " ~" $ [pretty idt | idt <- eqs]
                              | eqs <- precedence]
     | precedence <- cPrecToLists prec st]
   prettyStatus (Stat m) = vsep $ map (\(idt,s) -> pretty idt <> ":" <+> pretty s) (M.assocs m)
   prettyBasic (Basic m) = hsep . map pretty . M.keys . M.filter id $ m
   prettyAcc (Acc m) = vsep [pretty c <> ":" <+> "{" <> hsep (punctuate "," (map pretty (i:map snd xs))) <> "}"
                            | ((c,i):xs) <- groupBy (\x y -> fst x == fst y) . M.keys . M.filter id $ m]
   prettySmall (Small m) = hsep . map pretty . M.keys . M.filter id $ m
   inputHRS = line <> "input HRS:" <> line <> line <> vsep (map pretty hrs)
   ahrsText = if betaEtaLong
     then "corresponding algebraic representation in beta-eta long NF:"
     else "corresponding algebraic representation with fixed arity:"
   inputAHRS =  ahrsText <> line <> line <>
     vsep (map (\e -> pretty (alhs e) <+> "→" <+> pretty (arhs e)) (algRep res))
 in case mSol res of
  Just (tp,st,cp,basic,acc,small) -> inputHRS <> line <> line <> inputAHRS <> line <> line <>
    "status:" <> line <> line <>  prettyStatus st <> line <> line <>
    "type precedence:" <> line <> line <> prettyTypPrec tp <> line <> line <>
    "constant precedence:" <> line <> line <> prettyConstPrec cp st <> line <> line <>
    "basic base types:" <> line <> line <> prettyBasic basic <> line <> line <>
    "accessible arguments:" <> line <> line <> prettyAcc acc <> line <> line <>
    (if betaEtaLong then "" else "small symbols:" <> line <> line <> prettySmall small <> line <> line)
  Nothing -> inputHRS <> line <> line <> inputAHRS <> line <> line
