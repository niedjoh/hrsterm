-- |functions related to beta/eta conversion
module ATerm.BetaEta where

import ATerm.Type (ATerm(..),AT(..))

-- $setup
--
-- Setup code for examples:
--
-- >>> :set -XOverloadedStrings
-- >>> import Prettyprinter (pretty)
-- >>> import qualified Data.Map.Strict as M
-- >>> import Utils.Type (Id(..))
-- >>> import Typ.Parse (parseTyp)
-- >>> import Term.Type (Env(..))
-- >>> import Term.Parse (parseTerm)
-- >>> import ATerm.Type (EId(..))
-- >>> import ATerm.Ops (termToATerm)

-- |The call @shift t i j@ adds @j@ to all De Bruijn indexes in @t@ which are greater or equal than @i@.
--
-- >>> a = (parseTyp "a")
-- >>> aa = (parseTyp "a>a")
-- >>> db0 = ATerm {term = ADB 0, typ = a}
-- >>> db1 = ATerm {term = ADB 1, typ = aa}
-- >>> ap = ATerm {term = AAp db1 db0, typ = a}
-- >>> s = ATerm {term = ALam a ap, typ = aa}
-- >>> shift s 0 1
-- ATerm {term = ALam (Base (Id "a")) (ATerm {term = AAp (ATerm {term = ADB 2, typ = Ar (Base (Id "a")) (Base (Id "a"))}) (ATerm {term = ADB 0, typ = Base (Id "a")}), typ = Base (Id "a")}), typ = Ar (Base (Id "a")) (Base (Id "a"))}
shift :: ATerm -> Int -> Int -> ATerm
shift s@(ATerm {term = AFV _}) _ _ = s
shift s@(ATerm {term = ADB k}) i j
  | k < i     = s
  | otherwise = s{term = ADB (k+j)}
shift s@(ATerm {term = AFun f ts}) i j = s{term = AFun f (map (\u -> shift u i j) ts)}
shift s@(ATerm {term = AAp u v}) i j = s{term = AAp (shift u i j) (shift v i j)}
shift s@(ATerm {term = ALam a u}) i j = s{term = ALam a (shift u (i+1) j)}

-- |Substitutes a term for a De Bruijn index and decreases all indices above by 1
-- (This is needed for the beta reduction use-case.)
--
--  >>> a = (parseTyp "a")
-- >>> aa = (parseTyp "a>a")
-- >>> db0 = ATerm {term = ADB 0, typ = a}
-- >>> db1 = ATerm {term = ADB 1, typ = aa}
-- >>> ap = ATerm {term = AAp db1 db0, typ = a}
-- >>> s = ATerm {term = ALam a ap, typ = aa}
-- >>> u = ATerm {term = AFun (Normal (Id "c")) [], typ = aa}
-- >>> subst s 0 u
-- ATerm {term = ALam (Base (Id "a")) (ATerm {term = AAp (ATerm {term = AFun (Normal (Id "c")) [], typ = Ar (Base (Id "a")) (Base (Id "a"))}) (ATerm {term = ADB 0, typ = Base (Id "a")}), typ = Base (Id "a")}), typ = Ar (Base (Id "a")) (Base (Id "a"))}
subst :: ATerm -> Int -> ATerm -> ATerm
subst s@(ATerm {term = AFV _}) _ _ = s
subst s@(ATerm {term = ADB k}) i u = case compare k i of
  LT -> s{term = ADB k}
  EQ -> u
  GT -> s{term = ADB (k-1)}
subst s@(ATerm {term = AFun f ts}) i u = s{term = AFun f (map (\v -> subst v i u) ts)}
subst s@(ATerm {term = AAp t w}) i u = s{term = AAp (subst t i u) (subst w i u)}
subst s@(ATerm {term = ALam a v}) i u = s{term = ALam a (subst v (i+1) (shift u 0 1))}

-- |Determines whether a given De Bruijn index occurs in the term (modulo bound indices).
notFreeIn :: Int -> ATerm -> Bool
notFreeIn _ (ATerm {term = AFV _}) = True
notFreeIn i (ATerm {term = ADB j}) = i /= j
notFreeIn i (ATerm {term = AFun _ ts}) = all (notFreeIn i) ts
notFreeIn i (ATerm {term = AAp s t}) = notFreeIn i s && notFreeIn i t
notFreeIn i (ATerm {term = ALam _ s}) = notFreeIn (i+1) s

-- |Rewrites all topmost beta or eta redexes
betaEta1 :: ATerm -> (ATerm,Bool)
betaEta1 (ATerm {term = AAp (ATerm {term = ALam _ s}) t}) = let
  (s',_) = betaEta1 s
  (t',_) = betaEta1 t
  in (subst s' 0 t',True)
betaEta1 s@(ATerm {term = AAp u v}) = let
  (u',bu) = betaEta1 u
  (v',bv) = betaEta1 v
  in (s{term = AAp u' v'}, bu || bv)
betaEta1 (ATerm {term = ALam _ (ATerm {term = AAp t (ATerm {term = ADB 0})})})
  | 0 `notFreeIn` t = let
      (t',_) = betaEta1 $ shift t 0 (-1)
      in (t',True)
betaEta1 s@(ATerm {term = ALam a u}) = let
  (u',bu) = betaEta1 u
  in (s{term = ALam a u'},bu)
betaEta1 s@(ATerm {term = AFun f ts}) = let
  (ts',bts) = unzip $ map betaEta1 ts
  in (s{term = AFun f ts'},or bts)
betaEta1 s = (s,False)

-- |Reduces a term to beta-eta normal form.
--
-- >>> env = Env { cs = [(Id "c", parseTyp "a")], fvs = [(Id "F", parseTyp "a>(a>a)>a"),(Id "G", parseTyp "a>a")] }
-- >>> ar = M.fromList [(Id "c",0)]
-- >>> pretty . betaEta .termToATerm ar $ parseTerm env "(λx:a.F x (λy:a.G y)) c "
-- F c G
betaEta :: ATerm -> ATerm
betaEta s = case betaEta1 s of
  (s',True) -> betaEta s'
  _ -> s
