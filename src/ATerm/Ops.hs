-- |collection of functions for algebraic terms
module ATerm.Ops where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Utils.Type (Id(..))
import Typ.Type (Typ(..))
import Typ.Ops (applTyp,applTyps)
import Term.Type (Term(..),Var(..))
import qualified Term.Ops as TO
import ATerm.Type (ATerm(..),AT(..),EId(..))

-- $setup
--
-- Setup code for examples:
--
-- >>> :set -XOverloadedStrings
-- >>> import Prettyprinter (pretty)
-- >>> import Typ.Parse (parseTyp)
-- >>> import Term.Type (Env(..))
-- >>> import Term.Parse (parseTerm)
-- >>> a = parseTyp "a"
-- >>> aa = parseTyp "a>a"

-- |Converts a term to an algebraic term.
--
-- >>> ar = M.fromList [(Id "and",2),(Id "forall",1)]
-- >>> f = parseTyp "f"
-- >>> fff = parseTyp "f>f>f"
-- >>> tf = parseTyp "t>f"
-- >>> env = Env { cs = [(Id "and",fff),(Id "forall",tf)], fvs = [(Id "P",f),(Id "Q",tf)] }
-- >>> s = parseTerm env "and P (forall (λx:t.Q x))"
-- >>> pretty $ termToATerm ar s
-- and(P,forall(λx0:t.Q x0))
termToATerm :: Map Id Int -> Term -> ATerm
termToATerm _ (C idt a) = ATerm {term = AFun (Normal idt) [], typ = a}
termToATerm _ (FV v a) = ATerm {term = AFV v, typ = a}
termToATerm _ (DB i a) = ATerm {term = ADB i, typ = a}
termToATerm ar u@(Ap s t) = let
  recCase = ATerm {term = AAp as at, typ = recType}
  as = termToATerm ar s
  at = termToATerm ar t
  recType = case applTyp (typ as) (typ at) of
    Just b -> b
    Nothing -> error "impossible case"
  in case TO.strip u of
    (C idt a,ts) -> if ar M.! idt == length ts
      then ATerm {term = AFun (Normal idt) ats, typ = funTyp}
      else recCase where
        ats = map (termToATerm ar) ts
        funTyp = case applTyps a (map typ ats) of
          Just b -> b
          Nothing -> error "impossible case"
    _ -> recCase
termToATerm ar (Lam a s) = ATerm {term = ALam a as, typ = Ar a (typ as) } where
  as = termToATerm ar s

-- |Decomposes an algebraic term into head an spine.
strip :: ATerm -> (ATerm,[ATerm])
strip = go [] where
  go ts (ATerm {term = AAp s t}) = go (t:ts) s
  go ts s = (s,ts)

unstrip :: (ATerm,[ATerm]) -> Maybe ATerm
unstrip (s,ss) = foldl f (Just s) ss where
  f mu v = do
    u <- mu
    t <- applTyp (typ u) (typ v)
    pure $ ATerm {term = AAp u v, typ = t}

-- |Given an algebraic term s and a variable x, the term λx.s is returned.
--
-- >>> pretty $ varLam a (Named (Id "X")) (ATerm {term = ALam a (ATerm {term = AFV (Named (Id "X")), typ=a}), typ = aa})
-- λx0:a.λx1:a.x0
--
-- >>> pretty $ varLam a (Named (Id "X")) (ATerm {term = ALam a (ATerm {term = ADB 0, typ=a}), typ = aa})
-- λx0:a.λx1:a.x1
--
-- >>> pretty $ varLam a (Named (Id "X")) (ATerm {term = ALam a (ATerm {term = AFV (Named (Id "Y")), typ=a}), typ = aa})
-- λx0:a.λx1:a.Y
varLam :: Typ -> Var -> ATerm -> ATerm
varLam a v aterm = ATerm {term = ALam a (go 0 aterm), typ = Ar a (typ aterm)} where
  go i u@(ATerm {term = AFV w}) = if v == w
    then u{term = ADB i}
    else u
  go i u@(ATerm {term = ADB j}) = if j >= i
    then u{term = ADB (j+1)}
    else u
  go i u@(ATerm {term = AFun f ts}) = u{term = AFun f (map (go i) ts)}
  go i u@(ATerm {term = AAp s t}) = u{term = AAp (go i s) (go i t)}
  go i u@(ATerm {term = ALam c s}) = u{term = ALam c (go (i+1) s)}

-- |Given a de Bruijn index, a variable and a term, all corresponding de Bruijns in the term
-- are replaced with the variable.
--
-- >>> pretty $ dbToVar (Named (Id "X")) 0 (ATerm {term = ALam a (ATerm {term = ADB 1, typ=a}), typ = aa})
-- λx0:a.X
--
-- >>> pretty $ dbToVar (Named (Id "X")) 0 (ATerm {term = ALam a (ATerm {term = ADB 0, typ=a}), typ = aa})
-- λx0:a.x0
--
-- >>> s1 = ATerm {term = AFV (Named (Id "R")), typ = aa}
-- >>> s2 = ATerm {term = ADB 0, typ = a}
-- >>> s3 = ATerm {term = AAp s1 s2, typ = a}
-- >>> pretty $ dbToVar (Fresh 42) 0 (ATerm {term = AFun (Normal (Id "not")) [s3],typ = a})
-- not(R 変数42)
dbToVar :: Var -> Int -> ATerm -> ATerm
dbToVar v = go where
  go _ u@(ATerm {term = AFV _}) = u
  go i u@(ATerm {term = ADB j}) = if i == j
    then u{term = AFV v}
    else u
  go i u@(ATerm {term = AFun f ts}) = u{term = AFun f (map (go i) ts)}
  go i u@(ATerm {term = AAp s t}) = u{term = AAp (go i s) (go i t)}
  go i u@(ATerm {term = ALam c s}) = u{term = ALam c (go (i+1) s)}

-- |Checks whether a de Bruijn with the given index does not occur in the algebraic term.
--
-- >>> ctyp = parseTyp "a>(a>a)>a"
-- >>> s1 = ATerm {term = ADB 1, typ = a}
-- >>> s2 = ATerm {term = ALam a s1, typ = aa}
-- >>> s3 = ATerm {term = AFV (Named (Id "X")), typ = a}
-- >>> s = ATerm {term = AFun (Normal (Id "c")) [s3,s2], typ = a}
-- >>> notIn 0 s
-- False
notIn :: Int -> ATerm -> Bool
notIn _ (ATerm {term = AFV _}) = True
notIn i (ATerm {term = ADB j}) = i /= j
notIn i (ATerm {term = AFun _ ts}) = all (notIn i) ts
notIn i (ATerm {term = AAp s t}) = notIn i s && notIn i t
notIn i (ATerm {term = ALam _ s}) = notIn (i+1) s

-- |Checks whether a given algebraic term contains dangling de Bruijn indices,
-- i.e., indices which are not bound by any lambda.
danglingDB :: ATerm -> Bool
danglingDB = go 0 where
  go _ (ATerm {term = AFV _}) = False
  go i (ATerm {term = ADB j}) = j >= i
  go i (ATerm {term = AFun _ ts}) = any (go i) ts
  go i (ATerm {term = AAp s t}) = go i s || go i t
  go i (ATerm {term = ALam _ s}) = go (i+1) s

isFreeVar :: ATerm -> Bool
isFreeVar (ATerm {term = AFV _}) = True
isFreeVar _ = False

-- |List of subterms of a given term which are applications.
--
-- >>> env = Env { cs = [(Id "c",aa)], fvs = [(Id "F",aa),(Id "X",a)] }
-- >>> ar = M.fromList [(Id "c",1)]
-- >>> t1 = parseTerm env "c (F (F X))"
-- >>> pretty . applHeadedSubterms . termToATerm ar $ t1
-- [F (F X), F X]
--
-- >>> t2 = parseTerm env "λx:a.F X"
-- >>> pretty . applHeadedSubterms . termToATerm ar $ t2
-- [F X]
applHeadedSubterms :: ATerm -> [ATerm]
applHeadedSubterms (ATerm {term = AFun _ ts}) = concatMap applHeadedSubterms ts
applHeadedSubterms s@(ATerm {term = AAp u v}) = s : applHeadedSubterms u ++ applHeadedSubterms v
applHeadedSubterms (ATerm {term = ALam _ s}) = applHeadedSubterms s
applHeadedSubterms _ = []

-- |Checks whether the given algebraic term is nonversatile, i.e., it is either
-- * headed by a function symbol
-- * application (v w) with nonversatile v
-- * abstraction λx.sx where each application-headed subterm is nonversatile
-- * abstraction λx.s with s /= s'x such that
--   * if s is an application then it's nonversatile
--   * if s = t(uv) then t and u are nonversatile
--
-- note that we always assume terms to be beta-eta normalized.
--
-- >>> env = Env { cs = [(Id "c",a),(Id "f",aa)], fvs = [(Id "X",a),(Id "Y",aa)] }
-- >>> ar = M.fromList [(Id "c",0),(Id "f",1)]
-- >>> t1 = parseTerm env "c X"
-- >>> nonVersatile . termToATerm ar $ t1
-- True
--
-- >>> t2 = parseTerm env "λx:a.f (Y x)"
-- >>> nonVersatile . termToATerm ar $ t2
-- True
--
-- >>> t3 = parseTerm env "Y X"
-- >>> nonVersatile . termToATerm ar $ t3
-- False
--
-- >>> t4 = parseTerm env "λx:a.f(Y x) x"
-- >>> nonVersatile . termToATerm ar $ t4
-- False
--
-- >>> t5 = parseTerm env "λx:a.λy:a.Y x"
-- >>> nonVersatile . termToATerm ar $ t5
-- False
--
-- >>> t6 = parseTerm env "λx:a.λy:a.f x"
-- >>> nonVersatile . termToATerm ar $ t6
-- True
nonVersatile :: ATerm -> Bool
nonVersatile (ATerm {term = AFun {}}) = True
nonVersatile (ATerm {term = AAp t _}) = nonVersatile t
nonVersatile (ATerm {term = ALam _ s}) = case s of
  ATerm {term = AFV _} -> True
  ATerm {term = ADB _} -> True
  ATerm {term = AAp _ (ATerm {term = ADB 0})} -> all nonVersatile (applHeadedSubterms s)
  ATerm {term = AAp t (ATerm {term = AAp u _})} -> nonVersatile t && nonVersatile u
  ATerm {term = AAp t u@(ATerm {term = ALam _ _})} -> nonVersatile t && nonVersatile u
  _ -> nonVersatile s
nonVersatile _ = False

-- |convenience function which returns the negation of 'nonVersatile'
versatile :: ATerm -> Bool
versatile = not . nonVersatile

-- |Checks if a given extended identifier is a coerce symbol
isCoerce :: EId -> Bool
isCoerce (Coerce _ _) = True
isCoerce _ = False
