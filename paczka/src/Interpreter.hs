module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn, putStr
  , FilePath
  , getContents, readFile
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad

import AbsGramatyka   ( Program )
import LexGramatyka   ( Token, mkPosToken )
import ParGramatyka   ( pProgram, myLexer )
import PrintGramatyka ( Print, printTree )
import SkelGramatyka  ()

import TypeChecker
import Eval

type Err      = Either String
type ParseFun = [Token] -> Err AbsGramatyka.Program

-- todo: annotate with type

runFile :: ParseFun -> String -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

run :: ParseFun -> String -> IO ()
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
        Left err -> do
          putStrLn "Type checking failed!"
          putStrLn err
          showTree tree
        Right r -> do
          putStrLn $ "Type checking successful!" -- todo: remove
          case eval tree of
            Left (err, output) -> do
              putStrLn output
              putStrLn "Evaluation failed!"
              putStrLn err
              showTree tree
            Right (_, _, _, _, _, v, output) -> do
              putStrLn $ "Evaluation successful!" -- todo: remove
              putStrLn $ "Main exit code: " ++ show v -- todo: remove
              putStr $ output
              putStr $ "\n" -- todo: remove
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree tree = do
  putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

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

