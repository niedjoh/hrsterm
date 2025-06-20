{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless,when)
import Data.Char (toLower)
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime,diffUTCTime)
import Fmt ((+|),(|+),fixedF,fmtLn)
import Options.Applicative
    ( (<**>),
      argument,
      eitherReader,
      fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      option,
      progDesc,
      short,
      showDefault,
      str,
      switch,
      value,
      execParser,
      helper,
      Parser,
      ParserInfo )
import Prettyprinter (pretty,vsep)
import Prettyprinter.Render.Text (putDoc)
import System.Exit (exitSuccess,exitFailure)

import Utils.Parse (ProblemParser)
import Utils.SMT (SMTSolver,z3,cvc5,yices)
import Utils.InputProcessing (InputType(..),processInput)
import Typ.Type (BaseTypes)
import Term.Type (ConstTypeMap)
import Equation.Type (ES)
import qualified Termination.NHORPO.Solver as NHORPO
import qualified Termination.NCPO.Solver as NCPO
import qualified TPTP.Parse

data TermMethod = NHORPO | NHORPONeutralized | NCPO | NCPOBetaEtaLong deriving Show

data Args = Args
  { inputFile :: String
  , termMethod :: TermMethod
  , smtSolver :: SMTSolver
  , verbose :: Bool
  , debug :: Bool
  }

main :: IO ()
main = do
  start <- getCurrentTime
  args <- execParser opts
  input <- TIO.readFile (inputFile args)
  case processInput (inputFile args)
                    input
                    TPTP.Parse.parseProblem of
    Left e -> do
      putStrLn "ERROR"
      putStr e
      exitFailure
    Right (bts,cs,axs,_) -> termination (termMethod args)
                                        (smtSolver args)
                                        (verbose args)
                                        (debug args)
                                        bts cs axs
  stop <- getCurrentTime
  fmtLn $ "time: " +|fixedF 3  (diffUTCTime stop start)|+"s"

termination :: TermMethod -> SMTSolver -> Bool -> Bool -> BaseTypes -> ConstTypeMap -> ES ->  IO ()
termination NHORPO s v d bts cTyM hrs = do
  mres <- NHORPO.checkTermination s d False bts cTyM hrs
  case mres of
    Just res -> do
      if NHORPO.status res
        then putStrLn "YES"
        else putStrLn "MAYBE"
      when v . putDoc $ NHORPO.resultDoc res hrs
    Nothing -> putStrLn "SMT Pipe Error"
termination NHORPONeutralized s v d bts cTyM hrs = do
  mres <- NHORPO.checkTermination s d True bts cTyM hrs
  case mres of
    Just res -> do
      if NHORPO.status res
        then putStrLn "YES"
        else putStrLn "MAYBE"
      when v . putDoc $ NHORPO.resultDoc res hrs
    Nothing -> putStrLn "SMT Pipe ERROR"
termination NCPO s v d bts cTyM hrs = do
  res <- NCPO.checkTermination s d False bts cTyM hrs
  if NCPO.status res
    then putStrLn "YES"
    else putStrLn "MAYBE"
  when v . putDoc $ NCPO.resultDoc False res hrs
termination NCPOBetaEtaLong s v d bts cTyM hrs = do
  res <- NCPO.checkTermination s d True bts cTyM hrs
  if NCPO.status res
    then putStrLn "YES"
    else putStrLn "MAYBE"
  when v . putDoc $ NCPO.resultDoc True res hrs

printES :: String -> ES -> IO ()
printES s es = do
  putStrLn s
  if null es
    then putStrLn "none"
    else do
      putStrLn ""
      putDoc . vsep $ map pretty es
      putStrLn ""
      
opts :: ParserInfo Args
opts = info (argsParser <**> helper)
  ( fullDesc
  <> progDesc ("reads a file containing an (equational) HRS a la Nipkow " ++
               "and performs ermination analysis")
  <> header "hrsterm" )

argsParser :: Parser Args
argsParser = Args
  <$> argument str
      ( metavar "FILE"
     <> help "input file" )
  <*> option (eitherReader $ termMethodFromString . map toLower)
      ( long "term-method"
     <> short 't'
     <> showDefault
     <> value NCPO
     <> metavar "NHORPO | NHORPO_n | NCPO | NCPO_lnf"
     <> help "employed termination method" )
  <*> option (eitherReader $ smtSolverFromString . map toLower)
      ( long "smt-solver"
     <> short 's'
     <> showDefault
     <> value z3
     <> metavar "cvc5 | yices | z3"
     <> help "SMT solver" )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "more output information" )
  <*> switch
      ( long "debug"
     <> short 'd'
     <> help "output debug information" )

termMethodFromString :: String -> Either String TermMethod
termMethodFromString "nhorpo" = Right NHORPO
termMethodFromString "nhorpo_n" = Right NHORPONeutralized
termMethodFromString "ncpo" = Right NCPO
termMethodFromString "ncpo_lnf" = Right NCPOBetaEtaLong
termMethodFromString _ = Left "supported termination methods are 'nhorpo', 'nhorpo_n', 'ncpo' and 'ncpo_lnf'"

smtSolverFromString :: String -> Either String SMTSolver
smtSolverFromString "z3" = Right z3
smtSolverFromString "cvc5" = Right cvc5
smtSolverFromString "yices" = Right yices
smtSolverFromString _ = Left "supported SMT solvers are 'z3', 'cvc5' and 'yices'"
