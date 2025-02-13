{-# LANGUAGE OverloadedStrings #-}

-- |types for module Equation
module Equation.Type where

import Term.Type
import Prettyprinter (Pretty,pretty,(<+>))

-- |An equation is an oriented pair of terms. The flag 'isRule' can be used
-- to distinguish equations from rules of a PRS.
data Equation = Equation {lhs :: Term, rhs :: Term, isRule :: Bool} deriving (Eq,Show)

-- |An equational system (ES) is a collection of equations
type ES = [Equation]

instance Pretty Equation where
  pretty e = pretty (lhs e) <+> symb <+> pretty (rhs e) where
    symb = if isRule e then "→" else "≈" 
