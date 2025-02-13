-- |collection of useful functions for substitutions
module Equation.Ops where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Utils.Type (Id(..))
import Typ.Ops (base)
import Term.Ops (freeVars,isHeadedByFreeVar,typ,arities)
import Equation.Type (Equation(..),ES)

-- $setup
--
-- Setup code for examples:
--
-- >>> :set -XOverloadedStrings
-- >>> import Prettyprinter (pretty)
-- >>> import Data.Bifunctor (bimap)
-- >>> import Typ.Parse (parseTyp)
-- >>> import Term.Type (Env(..))
-- >>> import Equation.Parse (parseEquation)

-- |Lifts the function 'Term.Ops.Arities' to equations.
--
-- >>> env = Env { cs = [(Id "f", parseTyp "a>a>a"),(Id "c", parseTyp "a"),(Id "g", parseTyp "(a>a)>a")], fvs = [] }
-- >>> aritiesEq $ parseEquation env "f c c ≈ g (f c)"
-- fromList [(Id "c",0),(Id "f",1),(Id "g",1)]
aritiesEq :: Equation -> Map Id Int
aritiesEq e = M.unionWith min (arities (lhs e)) (arities (rhs e))

-- |Lifts the function 'Equation.Ops.aritiesEq' to ESs.
aritiesES :: ES -> Map Id Int
aritiesES es = M.unionsWith min $ map aritiesEq es

-- |Checks whether an equation fulfills the variable condition:
-- The right-hand side only uses free variables which are also present
-- in the left-hand side.
--
-- >>> aaa = parseTyp "a>a>a"
-- >>> a = parseTyp "a"
-- >>> env = Env { cs = [(Id "c", aaa),(Id "d", aaa)], fvs = [(Id "X", a),(Id "Y", a),(Id "Z", a)] }
-- >>> varCondition $ parseEquation env "c X Y ≈ d Y X"
-- True
--
-- >>> varCondition $ parseEquation env "c X Y ≈ d Z X"
-- False
varCondition :: Equation -> Bool
varCondition e = freeVars (rhs e) `S.isSubsetOf` freeVars (lhs e)

-- |Checks whether the given equation fulfills the conditions to be a rule:
-- * both terms are of the same base type
-- * the left-hand side does not have a free variable as its head
-- * the variable condition holds
--
-- >>> env = Env { cs = [(Id "c", parseTyp "a>a"),(Id "d", parseTyp "a>a")], fvs = [(Id "X", parseTyp "a")] }
-- >>> rule $ parseEquation env "c X ≈ d X"
-- True
--
-- >>> rule $ parseEquation env "c ≈ d"
-- False
rule :: Equation -> Bool
rule e = (a == b) && base a && not (isHeadedByFreeVar (lhs e)) && varCondition e where
  a = typ (lhs e)
  b = typ (rhs e)
