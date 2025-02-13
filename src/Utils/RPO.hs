{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |data types and utility function for recursive path orderings.
module Utils.RPO where

import Data.List (sortBy,groupBy,partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Language.Hasmtlib as SMT
import Prettyprinter (Pretty,pretty)

import Utils.Type (Id(..),Accessor(..))
import Utils.SMT (Constraint)
import Typ.Ops (flattenTyp)
import Term.Type (ConstTypeMap)
import ATerm.Type (EId(..))

-- |type class which can differentiate between lexicographic/multiset status
class IsStatus a where
  isLex :: a -> Constraint
  isMul :: a -> Constraint

-- |Different types of argument orderings (lexicographic and multiset).
data ArgOrd = Lex | Mul deriving Eq

instance IsStatus ArgOrd where
  isLex x = SMT.bool (x == Lex)
  isMul x = SMT.bool (x == Mul)

instance IsStatus (SMT.Expr SMT.BoolSort) where
  isLex x = x SMT.=== SMT.false
  isMul x = x SMT.=== SMT.true

instance Pretty ArgOrd where
  pretty Lex = "lex"
  pretty Mul = "mul"

-- |the status of a constant implemented as a map
newtype Status a = Stat (Map EId a)

-- |the precedence of a constant implemented as a map
newtype Precedence a = Prec (Map EId a) deriving (Foldable,Functor,Traversable)

-- |the precedence of a constant implemented as a map
newtype TypePrecedence a = TPrec (Map Id a) deriving (Foldable,Functor,Traversable)

-- |type precedence, status and constant precedence bundled into a record data type.
data RPOInfo a b = RPOInfo { tPrec :: TypePrecedence a
                           , stat :: Status b
                           , cPrec :: Precedence a }

instance Accessor Status where
  type AccessorKey Status = EId
  (Stat m) ! k = m M.! k

instance Accessor Precedence where
  type AccessorKey Precedence = EId
  (Prec m) ! k = m M.! k

instance Accessor TypePrecedence where
  type AccessorKey TypePrecedence = Id
  (TPrec m) ! k = m M.! k

instance SMT.Codec (Status (SMT.Expr SMT.BoolSort)) where
  type Decoded (Status (SMT.Expr SMT.BoolSort)) = Status ArgOrd
  encode (Stat m) = Stat $ fmap f m where
    f Lex = SMT.false
    f Mul = SMT.true
  decode sol (Stat m) = Stat <$> traverse f m where
    f x = do
      i <- SMT.decode sol x
      case i of
        False -> Just Lex
        True -> Just Mul

instance SMT.Codec a => SMT.Codec (Precedence a) where
  encode = fmap SMT.encode
  decode sol = traverse (SMT.decode sol)

instance SMT.Codec a => SMT.Codec (TypePrecedence a) where
  encode = fmap SMT.encode
  decode sol = traverse (SMT.decode sol)

-- |The return type of SMT solver calls.
type Solution = (TypePrecedence Integer, Status ArgOrd, Precedence Integer)

-- |Converts a type precedence to an easily printable list representation
-- (a list of lists of equivalent elements in decreasing order)
--
-- >>> prec = TPrec . M.fromList $ [(Id "a",(2 :: Integer)),(Id "b",3),(Id "c",1),(Id "d",2)]
-- >>> pretty $ tPrecToList prec
-- [[b], [a, d], [c]]
tPrecToList :: Ord a => TypePrecedence a -> [[Id]]
tPrecToList (TPrec m) = map (map fst) $ groupBy (\x y -> snd x == snd y)
  (sortBy (\x y -> compare (snd y) (snd x)) (M.assocs m))

-- |Converts a constant precedence to an easily printable list representation
-- (a list of list of lists of equivalent elements in decreasing order)
-- the outermost list layer is needed such that constants with different stati
-- are not considered to be equivalent in the precedence.
--
-- >>> neid x = Normal (Id x)
-- >>> st = Stat . M.fromList $ [(neid "a",Lex),(neid "b",Mul),(neid "c",Lex),(neid "d",Mul)]
-- >>> cp = Prec . M.fromList $ [(neid "a",(2 :: Integer)),(neid "b",3),(neid "c",1),(neid "d",2)]
-- >>> pretty $ cPrecToLists cp st
-- [[[b], [a], [c]], [[b], [d], [c]]]
cPrecToLists :: Ord a => Precedence a -> Status ArgOrd -> [[[EId]]]
cPrecToLists (Prec m) (Stat stm) = f iRepr where
  iRepr = map (partition stateIsLex . map fst) $ groupBy (\x y -> snd x == snd y)
    (sortBy (\x y -> compare (snd y) (snd x)) (M.assocs m))
  stateIsLex x = stm M.! x == Lex
  f (([],[]):_) = error "impossible case"
  f ((as,[]):rest) = do
    xss <- f rest
    pure $ as : xss
  f (([],bs):rest) = do
    xss <- f rest
    pure $ bs : xss
  f ((as,bs):rest) = do
    cs <- [as,bs]
    xss <- f rest
    pure $ cs : xss
  f [] = [[]]

-- |constraints which ensure that a given encoding of a status is correct
-- legalStatus :: (SMT.Orderable a, Num a) => Status a -> [Constraint]
-- legalStatus (Stat m) = [k SMT.>=? 0 SMT.&& k SMT.<=? 1 | k <- M.elems m] 

maxArities :: ConstTypeMap -> Map Id Int
maxArities = M.map (length . fst . flattenTyp)
