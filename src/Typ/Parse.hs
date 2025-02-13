{-# LANGUAGE OverloadedStrings #-}

-- |parsing functionality for simple types
module Typ.Parse(parseTyp,pTyp) where

import Control.Monad (void)
import Data.Text (Text)
import Text.Megaparsec (parse, errorBundlePretty,choice,sepBy1)
import Text.Megaparsec.Char (space)

import Utils.Parse (Parser,parens,name,symbol)
import Utils.Type (Id(..))
import Typ.Type (Typ(..))

-- $setup
--
-- Setup code for examples:
--
-- >>> :set -XOverloadedStrings
-- >>> import Prettyprinter (pretty)

-- |Parses a simple type.
--
-- >>> pretty $ parseTyp "a > (b > c) > d"
-- a > (b > c) > d
parseTyp :: Text -> Typ
parseTyp s = case parse (space >> pTyp) "" s of
  Right a -> a
  Left bundle -> error $ errorBundlePretty bundle

-- |A parser for simple types built with MegaParsec.
pTyp :: Parser Typ
pTyp = do
  ts <- sepBy1 pParensTyp pArrow
  return $ foldr1 Ar ts

pParensTyp :: Parser Typ
pParensTyp = choice
  [ pBase
  , parens pTyp
  ]

pArrow :: Parser ()
pArrow = void $ symbol ">"

pBase :: Parser Typ
pBase = Base . Id <$> name
