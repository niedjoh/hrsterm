{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

-- |solver for NHORPO
module Termination.NHORPO.Solver (NHORPORes(status),checkTermination,resultDoc) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State (evalState)
import Data.Default (def)
import qualified Data.Map.Strict as M
import qualified Language.Hasmtlib as SMT
import Prettyprinter (line,vsep,pretty,(<+>),Doc,line,punctuate,hsep)

import Utils.Type (Id(..),Accessor(..))
import Utils.RPO
  ( TypePrecedence(..)
  , Precedence(..)
  , Status(..)
  , RPOInfo(..)
  , Solution
  , tPrecToList
  , cPrecToLists
  , maxArities
  )
import Utils.SMT (SMTSolver(..),smtVarMap)
import Typ.Type (BaseTypes)
import Typ.Ops (flattenTyp)
import Term.Type (ConstTypeMap)
import Equation.Type (Equation(..),ES)
import Equation.Ops (aritiesES)
import ATerm.Type (AES,AEquation(..),EId(..))
import ATerm.Ops (termToATerm,isCoerce)
import Termination.NHORPO.Type (ArgumentPosSubset,NeutralizationLevel)
import Termination.NHORPO.Neutralization (allInterpretations)
import Termination.NHORPO.Ordering (nhorpoWrapper)

-- |result of termination proof attempt by NHORPO
data NHORPORes = NHORPORes
  { status :: Bool
  , mSol :: Maybe Solution
  , neutralized :: Bool
  , neutrL :: NeutralizationLevel
  , argPosS :: ArgumentPosSubset
  , algRep :: AES
  }

-- |Check termination of an HRS using NHORPO
checkTermination ::SMTSolver -> Bool -> Bool -> BaseTypes -> ConstTypeMap -> ES -> IO (Maybe NHORPORes)
checkTermination (Solver _ _ s) debug neutr bts constTyM hrs = do
  let ar = M.union (aritiesES hrs) (maxArities constTyM)
      fty = M.map flattenTyp constTyM
      arep = map (\e -> AEq { alhs = termToATerm ar (lhs e), arhs = termToATerm ar (rhs e), aisRule = isRule e}) hrs
      problems = if neutr
                 then allInterpretations ar fty arep
                 else [(arep,[],M.empty,M.empty)]
      inputConsts = map Normal (M.keys fty)
      consts = if neutr
               then inputConsts ++ map New (M.keys fty)
               else inputConsts 
  SMT.interactiveWith (if debug then (SMT.debugging def s) else s) $ do
    SMT.setLogic "ALL"
    SMT.setOption $ SMT.ProduceModels True
    solveOneOf neutr consts bts problems

solveOneOf :: (MonadIO m, SMT.MonadIncrSMT s m) =>
  Bool -> [EId] -> [Id] -> [(AES,[EId],NeutralizationLevel,ArgumentPosSubset)] -> m NHORPORes
solveOneOf neutr consts bts [(aes,bcs,nl,ap)] = do
  (res,msol) <- solve (bcs ++ consts) bts aes
  return $ NHORPORes { status = res
                     , mSol = msol
                     , neutralized = neutr
                     , neutrL = nl
                     , argPosS = ap
                     , algRep = aes }
solveOneOf neutr consts bts ((aes,bcs,nl,ap):rest) = do
  (res,msol) <- solve (bcs ++ consts) bts aes
  if res
  then return $ NHORPORes { status = res
                          , mSol = msol
                          , neutralized = neutr
                          , neutrL = nl
                          , argPosS = ap
                          , algRep = aes }
  else solveOneOf neutr consts bts rest
solveOneOf _ _ _ [] = error "impossible case"

solve :: (MonadIO m, SMT.MonadIncrSMT s m) => [EId] -> [Id] -> [AEquation] -> m (Bool,Maybe Solution)
solve consts bts aes = do
  SMT.push
  let containsCoerceSymb = not . null . filter isCoerce $ consts
  st <- Stat <$> smtVarMap @SMT.BoolSort consts
  tp <- TPrec <$> smtVarMap (if containsCoerceSymb then Id "$" : bts else bts)
  cp <- Prec <$> smtVarMap consts
  let rpoinfo = RPOInfo { tPrec = tp, stat = st, cPrec = cp }
  let typeConstraints = if containsCoerceSymb then [tp ! a SMT.>=? tp ! (Id "$") | a <- bts, a /= (Id "$")] else []
  mapM_ SMT.assert typeConstraints
  let constraints = evalState (mapM (\e -> nhorpoWrapper rpoinfo (alhs e) (arhs e)) aes) 0
  mapM_ SMT.assert constraints
  (res,sol) <- SMT.solve
  SMT.pop
  let result = case res of
        SMT.Sat -> True
        SMT.Unsat -> False
        SMT.Unknown -> False
  return (result, SMT.decode sol (tp,st,cp))

-- |Print the result of a solution attempt to a termination check using NHORPO.
resultDoc :: NHORPORes -> ES -> Doc ann
resultDoc res hrs = let
   prettyTypPrec prec =
     hsep . punctuate " >" $ [ hsep . punctuate " ~" $ [pretty idt | idt <- eqs]
                             | eqs <- tPrecToList prec]
   prettyConstPrec prec st = vsep
     [hsep . punctuate " >" $ [ hsep . punctuate " ~" $ [pretty idt | idt <- eqs]
                              | eqs <- precedence]
     | precedence <- cPrecToLists prec st]
   prettyStatus (Stat m) = vsep $ map (\(idt,s) -> pretty idt <> ":" <+> pretty s) (M.assocs m)
   prettyNL m = if neutralized res
     then "neutralization levels:" <> line <> line <>
       vsep (map (\((idt,i),j) -> pretty idt <> "_" <> pretty i <+> "=" <+> pretty j) (M.assocs m)) <>
       line <> line
     else ""
   prettyAP m = if neutralized res
     then "subsets of argument positions:" <> line <> line <>
     vsep (map (\((idt,i),js) -> pretty idt <> "_" <> pretty i <+> "=" <+> pretty js) (M.assocs m)) <>
     line <> line
     else ""
   inputHRS = line <> "input HRS:" <> line <> line <> vsep (map pretty hrs)
   neutr = if neutralized res
     then " neutralized"
     else ""
   inputAHRS = "corresponding" <> neutr <+> "algebraic representation with fixed arity:" <> line <> line <>
     vsep (map (\e -> pretty (alhs e) <+> "→" <+> pretty (arhs e)) (algRep res))
 in case mSol res of
  Just (tp,st,cp) -> inputHRS <> line <> line <> inputAHRS <> line <> line <>
     prettyNL (neutrL res) <> prettyAP (argPosS res) <>
    "status:" <> line <> line <>  prettyStatus st <> line <> line <>
    "type precedence:" <> line <> line <> prettyTypPrec tp <> line <> line <>
    "constant precedence:" <> line <> line <> prettyConstPrec cp st <> line <> line
  Nothing -> inputHRS <> line <> line
        
