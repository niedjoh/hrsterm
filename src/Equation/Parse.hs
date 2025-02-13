{-# LANGUAGE OverloadedStrings #-}

-- |parsing functionality for equations
module Equation.Parse (parseEquation,pEquation) where

import Control.Monad.Trans.Reader (runReaderT)
import Data.Text (Text)

import Text.Megaparsec ((<|>), parse, errorBundlePretty)
import Text.Megaparsec.Char (space)

import Utils.Parse (EnvT,Parser,symbol)

import Term.Type (Env(..))
import Term.Parse (pTerm)
import Equation.Type

-- $setup
--
-- Setup code for examples:
--
-- >>> :set -XOverloadedStrings
-- >>> import Prettyprinter (pretty)
-- >>> import Utils.Type (Id(..))
-- >>> import Typ.Parse (parseTyp)

-- |Parses an equation with respect to an environment defining constants and free variables.
--Does not perform type checking.
--
-- >>> env = Env { cs = [(Id "c", parseTyp "a>a")], fvs = [(Id "F", parseTyp "a>a")] }
-- >>> pretty $ parseEquation env "λx:a. c (F x) ≈ λx:a.(λx:a. c x) (F x)"
-- λx0:a.c (F x0) ≈ λx0:a.(λx1:a.c x1) (F x0)
parseEquation :: Env -> Text -> Equation
parseEquation env s = case parse (runReaderT (space >> pEquation) env) "" s of
  Right e -> e
  Left bundle -> error $ errorBundlePretty bundle

-- |A parser for equations of simply-typed lambda terms built with MegaParsec. Does not perform type checking.
pEquation :: EnvT Parser Equation
pEquation = do
  s <- pTerm []
  rule <- True <$ (symbol "→" <|> symbol "->")
      <|> False <$ (symbol "≈" <|> symbol "==")
  t <- pTerm []
  return $ Equation {lhs = s, rhs = t, isRule = rule}

