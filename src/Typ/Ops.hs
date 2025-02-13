-- |Collection of useful functions for simple types
module Typ.Ops where

import Data.Set (Set)
import qualified Data.Set as S

import Utils.Type (Id(..))
import Typ.Type (FTyp, Typ(..))

-- $setup
--
-- Setup code for examples:
--
-- >>> :set -XOverloadedStrings

-- |Checks whether a type is a base type.
base :: Typ -> Bool
base (Base _) = True
base _ = False

-- |Conversion from type to flattened type.
--
-- >>> flattenTyp $ Ar (Base (Id "a")) (Ar (Ar (Base (Id "b")) (Base (Id "c"))) (Ar (Base (Id "d")) (Base (Id "e"))))
-- ([Base (Id "a"),Ar (Base (Id "b")) (Base (Id "c")),Base (Id "d")],Base (Id "e"))
flattenTyp :: Typ -> FTyp
flattenTyp a@(Base _) = ([],a)
flattenTyp (Ar a b) = let
  (as,a') = flattenTyp b
  in (a:as,a')

-- |Conversion from flattened type to type.
--
-- >>> unflattenTyp ([Base (Id "a"),Ar (Base (Id "b")) (Base (Id "c")),Base (Id "d")],Base (Id "e"))
-- Ar (Base (Id "a")) (Ar (Ar (Base (Id "b")) (Base (Id "c"))) (Ar (Base (Id "d")) (Base (Id "e"))))
unflattenTyp :: FTyp -> Typ
unflattenTyp (as,a) = foldr Ar a as

-- |Determines whether the second type is equatable to the first type by applications.
-- If successful, a list of the types which need to be applied in order to get the
-- same type is returned.
--
-- >>> equatableByTypApp ([], Base (Id "a")) ([], Base (Id "b"))
-- Nothing
--
-- >>> equatableByTypApp ([Base (Id "a")], Base (Id "b")) ([Base (Id "a")], Base (Id "b"))
-- Just []
--
-- >>> equatableByTypApp ([], Base (Id "b")) ([Base (Id "a")], Base (Id "b"))
-- Just [Base (Id "a")]
--
-- >>> equatableByTypApp ([Base (Id "a")], Base (Id "b")) ([Base (Id "a"), Base (Id "a")], Base (Id "b"))
-- Just [Base (Id "a")]
--
-- >>> equatableByTypApp ([Base (Id "a"), Base (Id "a")], Base (Id "b")) ([Base (Id "a")], Base (Id "b"))
-- Nothing
equatableByTypApp :: FTyp -> FTyp -> Maybe [Typ]
equatableByTypApp (as,a) (bs,b) = go (a : reverse as) (b : reverse bs) where
  go [] [] = Just []
  go (_:_) [] = Nothing
  go (x:xs) (y:ys) = if x == y then go xs ys else Nothing
  go [] ys = Just ys

-- |Arity of a given type.
--
-- >>> a = Base (Id "a")
-- >>> arity a
-- 0
--
-- >>> arity (Ar a a)
-- 1
--
-- >>> arity (Ar a (Ar (Ar a a) a))
-- 2
arity :: Typ -> Int
arity = length . fst . flattenTyp

-- |Determine the order of a given type.
--
-- >>> order (Base (Id "a"))
-- 0
--
-- >>> order (Ar (Base (Id "a")) (Base (Id "a")))
-- 1
--
-- >>> order (Ar (Ar (Base (Id "a")) (Base (Id "a"))) (Base (Id "a")))
-- 2
order :: Typ -> Int
order (Base _) = 0
order (Ar a b) = max (order a + 1) (order b)

-- |Determine the typ of an application.
--
-- >>> applTyp (Ar (Base (Id "a")) (Base (Id "a"))) (Base (Id "a"))
-- Just (Base (Id "a"))
--
-- >>> applTyp (Ar (Base (Id "a")) (Base (Id "a"))) (Base (Id "b"))
-- Nothing
--
-- >>> applTyp (Ar (Ar (Base (Id "a")) (Base (Id "a"))) (Base (Id "a"))) (Ar (Base (Id "a")) (Base (Id "a")))
-- Just (Base (Id "a"))
--
-- >>> applTyp (Ar (Ar (Base (Id "a")) (Base (Id "a"))) (Base (Id "a"))) (Ar (Base (Id "a")) (Base (Id "b")))
-- Nothing
applTyp :: Typ -> Typ -> Maybe Typ
applTyp (Ar a b) c = if a == c
  then Just b
  else Nothing
applTyp _ _ = Nothing

-- |Same as 'applTyp' but for multiple applications.
--
-- >>> applTyps (Ar (Base (Id "a")) (Ar (Base (Id "a")) (Base (Id "a")))) [Base (Id "a"),Base (Id "a")]
-- Just (Base (Id "a"))
--
-- >>> applTyps (Ar (Base (Id "a")) (Base (Id "a"))) []
-- Just (Ar (Base (Id "a")) (Base (Id "a")))
applTyps :: Typ -> [Typ] -> Maybe Typ
applTyps a [] = Just a
applTyps (Ar a b) (c:cs) = if a == c
  then applTyps b cs
  else Nothing
applTyps _ _ = Nothing

-- |Set of base type positions in a type.
--
-- >>> pos $ Ar (Ar (Base (Id "a")) (Base (Id "b"))) (Base (Id "b"))
-- fromList ["11","12","2"]
pos :: Typ -> Set String
pos (Base _) = S.singleton ""
pos (Ar a b) = S.mapMonotonic ('1':) (pos a) `S.union` S.mapMonotonic ('2':) (pos b)

-- |Set of positions of a given basetype name in a type.
--
-- >>> posOf (Id "a") $ Ar (Ar (Base (Id "a")) (Base (Id "b"))) (Base (Id "b"))
-- fromList ["11"]
--
-- >>> posOf (Id "b") $ Ar (Ar (Base (Id "a")) (Base (Id "b"))) (Base (Id "b"))
-- fromList ["12","2"]
posOf :: Id -> Typ -> Set String
posOf idt1 (Base idt2)
  | idt1 == idt2 = S.singleton ""
  | otherwise    = S.empty
posOf idt (Ar a b) = S.mapMonotonic ('1':) (posOf idt a) `S.union` S.mapMonotonic ('2':) (posOf idt b)

-- |Set of positive base type positions in a type.
--
-- >>> posPos $ Ar (Ar (Base (Id "a")) (Base (Id "b"))) (Base (Id "b"))
-- fromList ["11","2"]
posPos :: Typ -> Set String
posPos (Base _) = S.singleton ""
posPos (Ar a b) = S.mapMonotonic ('1':) (posNeg a) `S.union` S.mapMonotonic ('2':) (posPos b)

-- |Set of negative base type positions in a type.
--
-- >>> posNeg $ Ar (Ar (Base (Id "a")) (Base (Id "b"))) (Base (Id "b"))
-- fromList ["12"]
posNeg :: Typ -> Set String
posNeg (Base _) = S.empty
posNeg (Ar a b) = S.mapMonotonic ('1':) (posPos a) `S.union` S.mapMonotonic ('2':) (posNeg b)

-- |Set of positions regarding the property (comp-small) of CPO
cPos :: Id -> Typ -> Set String
cPos idt1 (Base idt2)
  | idt1 == idt2 = S.singleton ""
  | otherwise    = S.empty
cPos idt c@(Ar _ _) = nPos idt c

-- |Set of positions regarding the property (comp-sn) of CPO
sPos :: Id -> Typ -> Set String
sPos _ (Base _) = S.empty
sPos idt (Ar a b) = S.mapMonotonic ('1':) (nPos idt a) `S.union` S.mapMonotonic ('2':) (sPos idt b)

-- |Set of positions regarding the property (comp-red) of CPO
rPos :: Id -> Typ -> Set String
rPos _ (Base _) = S.empty
rPos idt c@(Ar _ _) = sPos idt c

-- |Set of positions regarding the property (comp-neutral) of CPO
nPos :: Id -> Typ -> Set String
nPos _ (Base _) = S.empty
nPos idt (Ar a b) = S.mapMonotonic ('1':) (sPos idt a) `S.union` S.mapMonotonic ('2':) (lPos idt b `S.union` cPos idt b)

-- |Set of positions regarding the property (comp-lam) of CPO
lPos :: Id -> Typ -> Set String
lPos _ (Base _) = S.empty
lPos idt c@(Ar a b) = cPos idt c `S.union`
  S.mapMonotonic ('1':) (sPos idt a `S.union` nPos idt a) `S.union`
  S.mapMonotonic ('2':) (lPos idt b `S.union` cPos idt b)
