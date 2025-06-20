{-# LANGUAGE OverloadedStrings #-}

-- |Types for module ATerm
module ATerm.Type where

import Prettyprinter (Pretty,pretty,(<+>),concatWith)

import Utils.Type (Id)
import Utils.Pretty (parensIf)
import Term.Type (Var)
import Typ.Type (Typ)
import Typ.Ops (unflattenTyp)

-- |Type for algebraic terms with annotated type information for each subterm.
data ATerm = ATerm { term :: AT , typ :: Typ } deriving (Eq,Ord,Show)

-- |Extended identifiers
data EId = Bot [Typ] Typ | Coerce Typ Typ | Normal Id | New Id deriving (Eq,Ord,Show)

-- |Algebraic terms as a variation of the usual term structure where constants are
-- seen as funcion symbols which directly take arguments.
data AT =
    AFV Var
  | ADB Int
  | AFun EId [ATerm]
  | AAFV Var [ATerm]
  | AADB Int [ATerm]
  | AAp ATerm ATerm
  | ALam Typ ATerm deriving (Eq,Ord,Show)

-- |Equation consisting of algebraic terms.
data AEquation = AEq { alhs :: ATerm, arhs :: ATerm, aisRule :: Bool } deriving (Eq,Ord)

-- |ES consisting of algebraic terms.
type AES = [AEquation]

instance Pretty EId where
  pretty (Normal idt) = pretty idt
  pretty (New idt) = pretty idt <> "_new"
  pretty (Bot as a) = "⊥<" <> pretty (unflattenTyp (as,a)) <> ">"
  pretty (Coerce a b) = "coerce<" <> pretty (unflattenTyp ([a],b)) <> ">"

instance Pretty ATerm where
  pretty = go [] (0 :: Int) (0 :: Int) where
    go _ _ _ (ATerm {term = AFV idt}) = pretty idt
    go ctx _ _ (ATerm {term = ADB i})
      | i < 0          = "-∞"
      | i < length ctx = ctx !! i
      | otherwise      = error "dangling DB"
    go ctx d p (ATerm {term = AFun idt ts}) = if null ts
      then pretty idt
      else pretty idt <> "(" <> concatWith (\x y -> x <> "," <> y) (map (go ctx d p) ts) <> ")"
    go ctx d p (ATerm {term = AAFV idt ts}) = if null ts
      then pretty idt
      else pretty idt <> "(" <> concatWith (\x y -> x <> "," <> y) (map (go ctx d p) ts) <> ")"
    go ctx d p s@(ATerm {term = AADB i ts}) = if null ts
      then go ctx d p s{term = ADB i}
      else go ctx d p s{term = ADB i} <> "(" <> concatWith (\x y -> x <> "," <> y) (map (go ctx d p) ts) <> ")"
    go ctx d p (ATerm {term = AAp s t}) =
      let p' = 6
      in parensIf (p > p') $ go ctx d p' s <+> go ctx d (p'+1) t
    go ctx d p (ATerm {term = ALam a s}) =
      let var = "x" <> pretty d
          p' = 5
      in parensIf (p > p') $ "λ" <> var <> ":" <> pretty a <> "." <> go (var:ctx) (d+1) p' s

instance Pretty AEquation where
  pretty e = pretty (alhs e) <+> symb <+> pretty (arhs e) where
    symb = if aisRule e then "→" else "≈" 
