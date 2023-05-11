module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad

import AbsGramatyka   ()
import LexGramatyka   ( Token, mkPosToken )
import ParGramatyka   ( pProgram, myLexer )
import PrintGramatyka ( Print, printTree )
import SkelGramatyka  ()

import TypeChecker
import Eval

type Err        = Either String
type ParseFun a = [Token] -> Err a

runFile :: (Print a, Show a) => ParseFun a -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

run :: (Print a, Show a) => ParseFun a -> String -> IO ()
run p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrLn "Tokens:"
      mapM_ (putStrLn . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!" -- todo: remove
      case checkTypes tree of
        Left err -> putStrLn err
        Right _ -> do
          putStrLn "Type checking successful!" -- todo: remove
          eval tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin."
    , "  file            Parse content of file."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run pProgram
    fs         -> mapM_ (runFile pProgram) fs

