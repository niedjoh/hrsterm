{-# LANGUAGE OverloadedStrings #-}

-- |Types for module Term
module Term.Type where

import Data.Map (Map)
import Data.Text (Text)
import Prettyprinter (Pretty,pretty,(<+>))

import Utils.Pretty (parensIf)
import Utils.Type (Id(..))
import Typ.Type (Typ)

-- |Variables are either named or fresh variables which are referenced by a unique integer.
data Var = Named Id | Fresh Int deriving (Eq,Ord,Show)

-- |De Brujin indices in tuple form
type DB = (Int,Typ)

-- |Terms are simply-typed lambda terms with full local type information.
data Term = C Id Typ | FV Var Typ | DB Int Typ | Ap Term Term | Lam Typ Term deriving (Eq,Show)

-- |Environment consisting of constant and free variable declarations.
data Env = Env { cs :: [(Id,Typ)], fvs :: [(Id,Typ)] } deriving Show

-- |A map from constant identifiers to types
type ConstTypeMap = Map Id Typ

instance Pretty Var where
  pretty (Named idt) = pretty idt
  pretty (Fresh i) = pretty ("変数" :: Text) <> pretty i

instance Pretty Term where
  pretty = go [] (0 :: Int) (0 :: Int) where
    go _ _ _ (C idt _) = pretty idt
    go _ _ _ (FV idt  _) = pretty idt
    go ctx _ _ (DB i _)
      | i < 0          = "-∞"
      | i < length ctx = ctx !! i
      | otherwise      = error "dangling DB"
    go ctx d p (Ap s t) =
      let p' = 6
      in parensIf (p > p') $ go ctx d p' s <+> go ctx d (p'+1) t
    go ctx d p (Lam a s) =
      let var = "x" <> pretty d
          p' = 5
      in parensIf (p > p') $ "λ" <> var <> ":" <> pretty a <> "." <> go (var:ctx) (d+1) p' s
