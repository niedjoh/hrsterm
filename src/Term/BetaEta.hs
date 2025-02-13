-- |functions related to beta/eta conversion
module Term.BetaEta (shift,subst,betaEta) where

import Term.Type (Term(..))

-- $setup
--
-- Setup code for examples:
--
-- >>> :set -XOverloadedStrings
-- >>> import Prettyprinter (pretty)
-- >>> import Utils.Type (Id(..))
-- >>> import Typ.Parse (parseTyp)
-- >>> import Term.Type (Env(..))
-- >>> import Term.Parse (parseTerm)

-- |The call @shift t i j@ adds @j@ to all De Bruijn indexes in @t@ which are greater or equal than @i@.
--
-- >>> shift (Lam (parseTyp "a") (Ap (DB 1 (parseTyp "a>a")) (DB 0 (parseTyp "a")))) 0 1
-- Lam (Base (Id "a")) (Ap (DB 2 (Ar (Base (Id "a")) (Base (Id "a")))) (DB 0 (Base (Id "a"))))
shift :: Term -> Int -> Int -> Term
shift s@(C _ _) _ _ = s
shift s@(FV _ _) _ _ = s
shift s@(DB k a) i j
  | k < i     = s
  | otherwise = DB (k+j) a
shift (Ap s t) i j = Ap (shift s i j) (shift t i j)
shift (Lam a s) i j = Lam a (shift s (i+1) j) 

-- |Substitutes a term for a De Bruijn index and decreases all indices above by 1
-- (This is needed for the beta reduction use-case.)
--
-- >>> subst (Lam (parseTyp "a") (Ap (DB 1 (parseTyp "a>a")) (DB 0 (parseTyp "a")))) 0 (C (Id "c") (parseTyp "a>a"))
-- Lam (Base (Id "a")) (Ap (C (Id "c") (Ar (Base (Id "a")) (Base (Id "a")))) (DB 0 (Base (Id "a"))))
subst :: Term -> Int -> Term -> Term
subst s@(C _ _) _ _ = s
subst s@(FV _ _) _ _ = s
subst (DB k a) i u = case compare k i of
  LT -> DB k a
  EQ -> u
  GT -> DB (k-1) a
subst (Ap s t) i u = Ap (subst s i u) (subst t i u)
subst (Lam a s) i u = Lam a (subst s (i+1) (shift u 0 1))

-- |Determines whether a given De Bruijn index occurs in the term (modulo bound indices).
notFreeIn :: Int -> Term -> Bool
notFreeIn _ (C _ _) = True
notFreeIn _ (FV _ _) = True
notFreeIn i (DB j _) = i /= j
notFreeIn i (Ap s t) = notFreeIn i s && notFreeIn i t
notFreeIn i (Lam _ s) = notFreeIn (i+1) s

-- |Rewrites all topmost beta or eta redexes
betaEta1 :: Term -> (Term,Bool)
betaEta1 (Ap (Lam _ s) t) = let
  (s',_) = betaEta1 s
  (t',_) = betaEta1 t
  in (subst s' 0 t',True)
betaEta1 (Ap s t) = let
  (s',bs) = betaEta1 s
  (t',bt) = betaEta1 t
  in (Ap s' t',bs || bt)
betaEta1 (Lam _ (Ap t (DB 0 _)))
  | 0 `notFreeIn` t = let
      (t',_) = betaEta1 $ shift t 0 (-1)
      in (t',True)
betaEta1 (Lam a s) = let
  (s',bs) = betaEta1 s
  in (Lam a s',bs)
betaEta1 s = (s,False)

-- |Reduces a term to beta-eta normal form.
--
-- >>> env = Env { cs = [(Id "c", parseTyp "a")], fvs = [(Id "F", parseTyp "a>(a>a)>a"),(Id "G", parseTyp "a>a")] }
-- >>> pretty . betaEta $ parseTerm env "(λx:a.F x (λy:a.G y)) c "
-- F c G
betaEta :: Term -> Term
betaEta s = case betaEta1 s of
  (s',True) -> betaEta s'
  _ -> s
