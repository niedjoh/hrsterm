{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |utility functions for parsing. By convention, each parser automatically consumes trailing space.
module Utils.Parse where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (isDigit,isAlphaNum)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec,Parsec,Token,Tokens,ParseErrorBundle,between,satisfy,takeWhileP)
import Text.Megaparsec.Char (space1,lowerChar,upperChar,letterChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Prettyprinter (Pretty,pretty,(<+>))

import Utils.Pretty (parensIf)
import Utils.Type (Id(..))
import Typ.Type (Typ(..))
import Term.Type (Env,Var(..))

-- |MegaParsec parser using 'Text'
type Parser = Parsec Void Text

-- |monad transformer for reading environments while parsing
type EnvT = ReaderT Env

-- |type synonym to store positions in the input
type Pos = Int

-- |intermediate term representation which stores positions while parsing
data ITerm =
    IC Pos Id Typ
  | IFV Pos Var Typ
  | IDB Pos Id Int Typ
  | IAp Pos Pos ITerm ITerm
  | ILam Pos Id Typ ITerm deriving (Eq,Show)

instance Pretty ITerm where
  pretty = go (0 :: Int) where
    go _ (IC _ idt _) = pretty idt
    go _ (IFV _ idt _) = pretty idt
    go _ (IDB _ idt _ _) = pretty idt
    go p (IAp _ _ s t) =
      let p' = 6
      in parensIf (p > p') $ go p' s <+> go (p'+1) t
    go p (ILam _ idt a s) =
      let p' = 5
      in parensIf (p > p') $ "λ" <> pretty idt <> ":" <> pretty a <> "." <> go p' s

-- |intermediate equation representation which stores positions while parsing
data IEquation = IEquation { ilhs :: ITerm, irhs :: ITerm, iisRule :: Bool, iposl :: Int, iposr :: Int }

-- |ESs of intermediate equations
type IES = [IEquation]

-- |type of a general problem parsing function
type ProblemParser = FilePath -> Text -> Either (ParseErrorBundle Text Void) (Map Text Typ, IES, IES)

-- |Checks whether a given 'Char' is alphanumeric or an underscore.
isAlphaNumU :: Char -> Bool
isAlphaNumU c = isAlphaNum c || c == '_' 

-- |consumes trailing space and ignores comments
sc :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m  ()
sc = L.space space1 (L.skipLineComment "%") (L.skipBlockComment "/*" "*/")

-- |A parser for names which are numbers or identifiers starting with a letter.
name :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m Text
name = lexeme $ identifier <|> number

-- |A parser for strings containing letters, underscores and digits starting with a letter.
identifier :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m Text
identifier = lexeme $ T.cons <$> letterChar <*> takeWhileP (Just "alpha, num or underscore char") isAlphaNumU

-- |A parser for names starting with lowercase letters which are numbers or
-- identifiers starting with lowercase letters.
nameStartingLow :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m Text
nameStartingLow = lexeme (identifierStartingLow <|> number)

-- |A parser for strings containing letters, underscores and digits starting with a lowercase letter.
identifierStartingLow :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m Text
identifierStartingLow =
  lexeme (T.cons <$> lowerChar <*> takeWhileP (Just "alpha, num or underscore char") isAlphaNumU)

-- |A parser for names starting with lowercase letters which are numbers or
-- identifiers starting with uppercase letters.
nameStartingUp ::(MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m Text
nameStartingUp = lexeme (identifierStartingUp <|> number)

-- |A parser for strings containing letters, underscores and digits starting with an uppercase letter.
identifierStartingUp :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m Text
identifierStartingUp =
  lexeme (T.cons <$> upperChar <*> takeWhileP (Just "alpha, num or underscore char") isAlphaNumU)

-- |A parser for numbers with no leading zeroes.
number :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m Text
number = lexeme (T.cons <$> nonZeroNumber <*> takeWhileP (Just "num") isDigit) where
  nonZeroNumber = satisfy (\c -> isDigit c && c /= '0')

-- |A parser for the dot symbol.
dot :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m Text
dot = symbol "."

-- |A parser for the comma symbol.
comma :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m Text
comma = symbol ","

-- |Modifies a parser to operate between parentheses.
parens :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m a -> m a
parens = lexeme . between (symbol "(") (symbol ")")

-- |Modifies a given parser to also consume trailing space.
lexeme :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m a -> m a
lexeme = L.lexeme sc

-- |A parser for a given symbol which consumes trailing space.
symbol :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => Text -> m Text
symbol = L.symbol sc
