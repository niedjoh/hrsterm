{-# LANGUAGE OverloadedStrings #-}

-- |types for module 'Typ'
module Typ.Type where

import Prettyprinter (Pretty,pretty,(<+>))

import Utils.Pretty (parensIf)
import Utils.Type (Id)

-- |Standard representation of simple types with binary arrow constructor.
data Typ = Base Id | Ar Typ Typ deriving (Eq,Ord,Show)

-- |Flattened type representation
type FTyp = ([Typ],Typ)

-- |List of base types as identifiers
type BaseTypes = [Id]

instance Pretty Typ where
  pretty = go (0 :: Int) where
    go _ (Base a) = pretty a
    go p (Ar a b) =
      let p' = 6
      in parensIf (p > p') $ go (p'+1) a <+> ">" <+> go p' b
