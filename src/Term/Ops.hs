{-# LANGUAGE OverloadedStrings #-}

-- |collection of useful functions for simply-typed terms
module Term.Ops where

import Data.List (sort,group)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import Utils.Type (Id(..))
import Typ.Type (Typ(..))
import Term.Type (Term(..),Var)

-- $setup
--
-- Setup code for examples:
--
-- >>> import Prettyprinter (pretty)
-- >>> import Data.Bifunctor (bimap)
-- >>> import Typ.Parse (parseTyp)
-- >>> import Term.Type (Env(..))
-- >>> import Term.Parse (parseTerm)

-- |Computes the size of the given term where each application, lambda abstraction,
-- constant and free/bound variable has size 1.
size :: Term -> Int
size (Lam _ s) = size s + 1
size (Ap s t) = size s + size t + 1
size _ = 1

-- |Computes the type of the supplied term.
--
-- >>> env = Env { cs = [(Id "c", parseTyp "b")], fvs = [(Id "F", parseTyp "b>a")] }
-- >>> pretty . typ $ parseTerm env "(λx:b>a. x c) F"
-- a
typ :: Term -> Typ
typ (C _ a) = a
typ (FV _ a) = a
typ (DB _ a) = a
typ (Ap s t) = case typ s of
  Ar a b ->
    if   a == typ t
    then b
    else error "types of application do not match"
  _ -> error "application of non-function"
typ (Lam a s) = Ar a (typ s)

-- |Decomposes a term into head an spine.
--
-- >>>  env = Env { cs = [(Id "c", parseTyp "a>b>a"), (Id "d", parseTyp "a"), (Id "e", parseTyp "b")], fvs = [] }
-- >>> pretty . strip $ parseTerm env "c d e"
-- (c, [d, e])
strip :: Term -> (Term,[Term])
strip = go [] where
  go ts (Ap s t) = go (t:ts) s
  go ts s = (s,ts)

-- |Transforms a head-spine representation back to a normal term.
--
-- >>> env = Env { cs = [(Id "c", parseTyp "a>b>a"), (Id "d", parseTyp "a"), (Id "e", parseTyp "b")], fvs = [] }
-- >>> pretty $ unstrip (parseTerm env "c", map (parseTerm env) ["d","e"])
-- c d e
unstrip :: (Term,[Term]) -> Term
unstrip (s,ss) = foldl Ap s ss

-- |Special version of 'strip' which also returns the type of the head symbol.
-- Only works if the head symbol is an atom (constant or free/bound variable).
--
-- >>> env = Env { cs = [(Id "c", parseTyp "a>b>a"), (Id "d", parseTyp "a"), (Id "e", parseTyp "b")], fvs = [] }
-- >>> pretty . stripWithType $ parseTerm env "c d e"
-- ((c, a > b > a), [d, e])
stripWithType :: Term -> ((Term,Typ),[Term])
stripWithType = go [] where
  go ts (Ap s t) = go (t:ts) s
  go ts s@(C _ a) = ((s,a),ts)
  go ts s@(FV _ a) = ((s,a),ts)
  go ts s@(DB _ a) = ((s,a),ts)
  go _ (Lam _ _) = error "stripWithType is only defined for terms of form a t_1 ... t_n"

-- |Computes the set of free variables of a term.
--
-- >>> env = Env { cs = [], fvs = [(Id "F", parseTyp "b>a>b>a"), (Id "Y", parseTyp "b")] }
-- >>> pretty . S.toList . freeVars $ parseTerm env "λx:a.F Y x Y"
-- [F, Y]
freeVars :: Term -> Set Var
freeVars (FV v _) = S.singleton v
freeVars (Ap s t) = freeVars s `S.union` freeVars t
freeVars (Lam _ s) = freeVars s
freeVars _ = S.empty

-- |Same as 'freeVars', but with additional type information.
--
-- >>> env = Env { cs = [], fvs = [(Id "F", parseTyp "b>a>b>a"), (Id "Y", parseTyp "b")] }
-- >>> pretty . M.toList . freeVarsWithType $ parseTerm env "λx:a.F Y x Y"
-- [(F, b > a > b > a), (Y, b)]
freeVarsWithType :: Term -> Map Var Typ
freeVarsWithType (FV v a) = M.singleton v a
freeVarsWithType (Ap s t) = freeVarsWithType s `M.union` freeVarsWithType t
freeVarsWithType (Lam _ s) = freeVarsWithType s
freeVarsWithType _ = M.empty

-- |Given a term, this function computes the arities of the constants it contains.
-- Here, the arity is defined as the minimum amount of arguments which the constant is applied to.
--
-- >>> env = Env { cs = [(Id "f", parseTyp "(a>a)>a>a"),(Id "c", parseTyp "a")], fvs = [] }
-- >>> arities $ parseTerm env "f (f (λx:a.x)) c"
-- fromList [(Id "c",0),(Id "f",1)]
arities :: Term -> Map Id Int
arities (C idt _) = M.singleton idt 0
arities s@(Ap u v) = case strip s of
  (C idt _,ts) -> M.unionsWith min (M.singleton idt (length ts) : map arities ts)
  _ -> M.unionWith min (arities u) (arities v)
arities (Lam _ s) = arities s
arities _ = M.empty

-- |Determines whether the head of the term is a free variable.
--
-- >>> env = Env { cs = [(Id "c", parseTyp "a")], fvs = [(Id "X", parseTyp "a>a")] }
-- >>> isHeadedByFreeVar $ parseTerm env "X c"
-- True
--
-- >>> isHeadedByFreeVar $ parseTerm env "c X"
-- False
isHeadedByFreeVar :: Term -> Bool
isHeadedByFreeVar s = case strip s of
  (FV _ _,_) -> True
  _ -> False

-- |Determines whether the term is linear (no free variable occurs more than once)
--
-- >>> env = Env { cs = [(Id "c", parseTyp "a>a>a")], fvs = [(Id "X", parseTyp "a"), (Id "Y", parseTyp "a")] }
-- >>> isLinear $ parseTerm env "c X Y"
-- True
--
-- >>> isLinear $ parseTerm env "c X X"
-- False
isLinear :: Term -> Bool
isLinear = all (\xs -> length xs < 2)  . group . sort . go where
  go (FV v _) = [v]
  go (Ap s t) = go s ++ go t
  go (Lam _ s) = go s
  go _ = []
