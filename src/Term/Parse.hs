{-# LANGUAGE OverloadedStrings #-}

-- |parsing functionality for simply-typed terms
module Term.Parse (Ctx,parseTerm, pTerm) where

import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT,ask)
import Data.Text (Text)
import Fmt ((|+),(+|))
import Text.Megaparsec ((<|>),parse,errorBundlePretty,choice,some)
import Text.Megaparsec.Char (space)

import Utils.Parse (Parser,EnvT,parens,name,symbol)
import Utils.Type (Id(..))
import Typ.Type (Typ)
import Typ.Parse (pTyp)
import Term.Type (Env(..), Term(..),Var(..))

-- | context for bound variables during parsing
type Ctx = [(Text,Typ)]

-- $setup
--
-- Setup code for examples:
--
-- >>> :set -XOverloadedStrings
-- >>> import Prettyprinter (pretty)
-- >>> import Typ.Parse (parseTyp)

-- |Parses a term with respect to an environment defining constants and free variables.
--Does not perform type checking.
--
-- >>> env = Env { cs = [(Id "c", parseTyp "a>a")], fvs = [(Id "F", parseTyp "a>a")] }
-- >>> pretty $ parseTerm env "λx:a. c (F x)"
-- λx0:a.c (F x0)
parseTerm :: Env -> Text -> Term
parseTerm env s = case parse (runReaderT (space >> pTerm []) env) "" s of
  Right t -> t
  Left bundle -> error $ errorBundlePretty bundle

-- |A parser for simply-typed lambda terms built with MegaParsec. Does not perform type checking.
pTerm :: Ctx -> EnvT Parser Term
pTerm ctx = choice
  [ pLam ctx
  , pAp ctx
  ]

pParensTerm :: Ctx -> EnvT Parser Term
pParensTerm ctx = choice
  [ parens $ pLam ctx <|> pAp ctx
  , pVarConst ctx 
  ]

pAp :: Ctx -> EnvT Parser Term
pAp ctx = do
  ts <- some (pParensTerm ctx)
  return $ foldl1 Ap ts

pLam :: Ctx -> EnvT Parser Term
pLam ctx = do
  void $ symbol "\\" <|> symbol "λ"
  x <- lift name
  void $ symbol ":"
  a <- lift pTyp
  void $ symbol "."
  t <- pTerm ((x,a):ctx)
  return $ Lam a t

pVarConst :: Ctx -> EnvT Parser Term
pVarConst ctx = do
  s <- lift name
  env <- ask
  case lookup s (zipWith (\(k,v) i -> (k,(v,i))) ctx [0..]) of
    Just (a,i) -> return $ DB i a
    Nothing -> let
        idt = Id s
      in case lookup idt (cs env) of
        Just a -> return $ C idt a
        Nothing -> case lookup idt (fvs env) of
          Just a -> return $ FV (Named idt) a
          Nothing -> fail $ "'"+|s|+"' is neither a constant nor a free variable"
