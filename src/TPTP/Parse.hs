{-# LANGUAGE OverloadedStrings #-}

-- |Parser for equational THF fragment.
module TPTP.Parse (parseProblem) where

import Control.Applicative (many,(<|>))
import Control.Monad (void)
import Control.Monad.Trans.State (StateT,runStateT,get,put)
import Data.Either (partitionEithers)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Fmt ((|+),(+|))
import Text.Megaparsec (parse,chunk,choice,sepBy1,setOffset,getOffset)

import Utils.Parse
    ( sc,
      comma,
      dot,
      identifierStartingLow,
      identifierStartingUp,
      lexeme,
      name,
      parens,
      symbol,
      IEquation(..),
      ITerm(..),
      Parser,
      ProblemParser )
import Utils.Type (Id(..))
import Typ.Type (Typ(..))
import Term.Type (Var(..))

import Data.Text (Text)

data Role = Type | Axiom | Conjecture
type Ctx = [(Text,Typ)]
type TPTPEnv = Map Text Typ
type TPTPParser = StateT TPTPEnv Parser

-- |Parser for a fairly simple fragment of TPTP: simply-typed constant declarations together
-- with axioms/conjectures of the form ∀...∀ l = r
parseProblem :: ProblemParser
parseProblem file input = case parse (runStateT (sc >> problem) M.empty) file input of
  Right (mees,env) -> Right (env,axs,conjs) where
    (axs,conjs) = partitionEithers . catMaybes $ mees
  Left bundle -> Left bundle

problem :: TPTPParser [Maybe (Either IEquation IEquation)]
problem = many annotatedTHFFormula

annotatedTHFFormula :: TPTPParser (Maybe (Either IEquation IEquation))
annotatedTHFFormula = do
  void $ symbol "thf"
  void $ symbol "("
  void name
  void comma
  r <- role
  void comma
  mee <- case r of
    Type -> updateState "constant" typeDecl $> Nothing
    Axiom -> Just . Left <$> equation
    Conjecture -> Just . Right <$> equation
  void $ symbol ")"
  void dot
  return mee

role :: TPTPParser Role
role = fromText <$> lexeme (chunk "axiom" <|> chunk "type" <|> chunk "conjecture") where
  fromText "type" = Type
  fromText "axiom" = Axiom
  fromText "conjecture" = Conjecture
  fromText _ = error "impossible case"

typeDecl :: TPTPParser (Text,Typ)
typeDecl = do
  c <- identifierStartingLow
  void $ symbol ":"
  a <- (Base . Id <$> symbol "$tType") <|> typ
  return (c,a)

updateState :: Text -> TPTPParser (Text,Typ) -> TPTPParser (Text,Typ)
updateState entity parser = do
  p <- getOffset
  (idt,a) <- parser
  m <- get
  case m M.!? idt of
      Just _ -> setOffset p >> fail ("re-definition of "+|entity|+" '"+|idt|+"'")
      Nothing -> put $ M.insert idt a m
  return (idt,a)

typ :: TPTPParser Typ
typ = foldr1 Ar <$> sepBy1 parensTyp (symbol ">")

parensTyp :: TPTPParser Typ
parensTyp = choice
  [ base
  , parens typ
  ]

base :: TPTPParser Typ
base = do
  a <- identifierStartingLow
  env <- get
  case env M.!? a of
    Just (Base (Id "$tType")) -> return . Base . Id $ a
    _ -> fail $ "'"+|a|+"' is not a declared base type"

equation :: TPTPParser IEquation
equation = do
  env <- get  
  e <- choice
    [ quantifierPrefix >> (parens termPair <|> termPair)
    , parens termPair
    , termPair
    ]
  put env -- modifications to environment are local to equation
  return e

quantifierPrefix :: TPTPParser ()
quantifierPrefix = do
  void $ symbol "!"
  void $ symbol "["
  void $ sepBy1 (updateState "variable" varDecl) (symbol ",")
  void $ symbol "]"
  void $ symbol ":"

varDecl :: TPTPParser (Text,Typ)
varDecl = do
  x <- identifierStartingUp
  void $ symbol ":"
  a <- typ
  return (x,a)

termPair :: TPTPParser IEquation
termPair = do
  pl <- getOffset
  l <- term []
  void $ symbol "="
  pr <- getOffset
  r <- term []
  return $ IEquation { ilhs = l, irhs = r, iisRule = False, iposl = pl, iposr = pr }
  
term :: Ctx -> TPTPParser ITerm
term ctx = do
  fst . foldl1 (\(is,p) (it,p') -> (IAp p p' is it,p)) <$> sepBy1 (parensTerm ctx) (symbol "@")

parensTerm :: Ctx -> TPTPParser (ITerm,Int)
parensTerm ctx = do
  p <- getOffset
  it <- choice
    [ lam ctx
    , var ctx
    , con
    , parens $ term ctx
    ]
  return (it,p)

lam :: Ctx -> TPTPParser ITerm
lam ctx = do
  p <- getOffset
  void $ symbol "^"
  void $ symbol "["
  ps <- sepBy1 varDecl (symbol ",")
  void $ symbol "]"
  void $ symbol ":"
  s <- term (reverse ps ++ ctx)
  return $ foldr (\(x,a) -> ILam p (Id x) a) s ps

var :: Ctx -> TPTPParser ITerm
var ctx = do
  p <- getOffset
  x <- identifierStartingUp
  env <- get
  case lookup x (zipWith (\(k,v) i -> (k,(v,i))) ctx [0..]) of
    Just (a,i) -> return $ IDB p (Id x) i a
    Nothing -> case env M.!? x of
      Just a -> return $ IFV p (Named (Id x)) a
      Nothing -> fail $ "'"+|x|+"' is not a declared variable"

con :: TPTPParser ITerm
con = do
  p <- getOffset
  c <- identifierStartingLow
  env <- get
  case env M.!? c of
    Just a -> return $ IC p (Id c) a
    Nothing -> fail $ "'"+|c|+"' is not a declared constant"
