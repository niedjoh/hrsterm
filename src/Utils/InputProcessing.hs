{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- |utility functions for input processing
module Utils.InputProcessing(InputType(..),processInput) where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Void (Void)
import Prettyprinter (Pretty,Doc,(<+>),pretty,layoutPretty,defaultLayoutOptions,vsep)
import Prettyprinter.Render.String (renderString)
import Text.Megaparsec (ParseError,ParseErrorBundle(..),errorBundlePretty)
import Text.Megaparsec.Error (ErrorFancy(..))
import Text.Megaparsec.Error.Builder (errFancy,fancy)
import Text.Megaparsec.State (initialPosState)

import Utils.Type (Id(..))
import Utils.Parse (ITerm(..),IEquation(..),ProblemParser)
import Typ.Type (Typ(..),BaseTypes)
import Term.Type (Term(..),ConstTypeMap)
import Term.Ops (isHeadedByFreeVar)
import Term.BetaEta (betaEta)
import Equation.Type (Equation(..),ES)
import Equation.Ops (varCondition)

-- |type of input system
data InputType = ES | HRS | PRS

instance Pretty InputType where
  pretty ES = "ES"
  pretty HRS = "HRS"
  pretty PRS = "PRS"

-- |Processes the input with consistent error handling
-- * parsing
-- * type check
-- * input type check
processInput :: FilePath -> Text -> ProblemParser -> Either String (BaseTypes,ConstTypeMap,ES,ES)
processInput file input parser = do
  (textTypeMap,axs,conjs) <- first errorBundlePretty (parser file input)
  let (btM,cTyM) = M.partition (\a -> Base (Id "$tType") == a) textTypeMap
  mapM_ (first (errorBundlePretty . toBundle) . typeCheckIEq) axs
  mapM_ (first (errorBundlePretty . toBundle) . typeCheckIEq) conjs
  axs' <- mapM (first (errorBundlePretty . toBundle) . checkInput) axs
  return (map Id (M.keys btM), M.mapKeysMonotonic Id cTyM, map iEqToEq axs', map iEqToEq conjs) where
    iState = initialPosState file input
    toBundle pe = ParseErrorBundle { bundleErrors = pe :| [], bundlePosState = iState }

-- |Checks whether a term is well-typed.
typeCheckITerm :: ITerm -> Either (ParseError Text Void) Typ
typeCheckITerm = go where
  go (IC _ _ a) = Right a
  go (IFV _ _ a) = Right a
  go (IDB _ _ _ a) = Right a
  go (IAp ps pt is it) = do
    a <- go is
    b <- go it
    case a of
      (Ar c d) -> if   c == b
                  then pure d
                  else Left $ constructTCError pt (pretty it <> ":" <+> pretty b)
                                                  (pretty it <> ":" <+> pretty c)
      _ -> Left $ constructTCError ps (pretty is <> ":" <+> pretty a) (pretty is <> ":" <+> "* > *")
  go (ILam _ _ a s) = Ar a <$> go s

-- |Checks whether both sides of an equation are well-typed and have the same type.
typeCheckIEq :: IEquation -> Either (ParseError Text Void) Typ
typeCheckIEq ie = do
  a <- typeCheckITerm (ilhs ie)
  b <- typeCheckITerm (irhs ie)
  let pr = pretty . irhs $ ie
  if a == b
    then Right a
    else Left $ constructTCError (iposr ie) (pr <> ":" <+> pretty b) (pr <> ":" <+> pretty a)

-- |Checks whether an equation adheres to the mandated input type.
checkInput :: IEquation -> Either (ParseError Text Void) IEquation
checkInput ie
  | isHeadedByFreeVar . iTermToTerm . ilhs $ ie = Left $ constructITError HRS (iposl ie)
                                                         "free variable in head position of left-hand side"
  | not . varCondition . iEqToEq $ ie   = Left $ constructITError HRS (iposr ie)
                                                 "extra variables on right-hand side"
  | otherwise                           = Right $ ie { iisRule = True }

-- |Conversion from the intermediate representation of terms to terms.
iTermToTerm :: ITerm -> Term
iTermToTerm = \case
  IC _ idt a -> C idt a
  IFV _ v a -> FV v a
  IDB _ _ i a -> DB i a
  IAp _ _ is it -> Ap (iTermToTerm is) (iTermToTerm it)
  ILam _ _ a is -> Lam a (iTermToTerm is)

-- |Conversion from the intermediate representation of equations to beta-eta normalized equations.
iEqToEq :: IEquation -> Equation
iEqToEq ie = Equation { lhs = betaEta . iTermToTerm $ ilhs ie
                      , rhs = betaEta . iTermToTerm $ irhs ie
                      , isRule = iisRule ie }

-- |Helper function to construct a type check error which looks like a parse error and
-- points to the corresponding position in the input.
constructTCError :: Int -> Doc ann -> Doc ann -> ParseError Text Void
constructTCError i ud ed = errFancy i (fancy . ErrorFail $ s) where
  s = toString (vsep ["type check failed","have" <+> ud,"want" <+> ed])
  toString = renderString . layoutPretty defaultLayoutOptions

-- |Helper function to construct a input type error which looks like a parse error and
-- points to the corresponding position in the input.
constructITError :: InputType -> Int -> Doc ann -> ParseError Text Void
constructITError inputType i d = errFancy i (fancy . ErrorFail $ s) where
  s = toString (vsep ["not a valid" <+> pretty inputType <+> "rule",d])
  toString = renderString . layoutPretty defaultLayoutOptions
