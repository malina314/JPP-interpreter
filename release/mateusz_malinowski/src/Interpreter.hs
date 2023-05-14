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
import System.IO          ( hPutStrLn, stderr )
import Control.Monad

import AbsGramatyka   ( Program )
import LexGramatyka   ( Token, mkPosToken )
import ParGramatyka   ( pProgram, myLexer )
import PrintGramatyka ( Print, printTree )
import SkelGramatyka  ()

import TypeChecker    ( checkTypes )
import Eval           ( eval, Value(..) )

type Err       = Either String
type ParseFun  = [Token] -> Err AbsGramatyka.Program
type Verbosity = Int

ePutStrLn :: String -> IO ()
ePutStrLn s = hPutStrLn stderr s

ePutStrV :: Verbosity -> String -> IO ()
ePutStrV v s = when (v > 0) $ ePutStrLn s

runFile :: Verbosity -> ParseFun -> String -> IO ()
runFile v p f = ePutStrV v f >> readFile f >>= run v p

run :: Verbosity -> ParseFun -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      ePutStrLn "Parse Failed!\n"
      ePutStrLn "Tokens:"
      mapM_ (ePutStrLn . showPosToken . mkPosToken) ts
      ePutStrLn err
    Right tree -> do
      ePutStrV v "\nParse Successful!"
      case checkTypes tree of
        Left err -> do
          ePutStrLn "Type checking failed!"
          ePutStrLn err
          showTree v tree
        Right r -> do
          ePutStrV v "Type checking successful!"
          case eval tree of
            Left (err, output) -> do
              putStr output
              ePutStrLn "Evaluation failed!"
              ePutStrLn err
              showTree v tree
            Right (_, _, _, _, _, I mainRet, output) -> do
              ePutStrV v "Evaluation successful!"
              ePutStrV v $ "Main returned: " ++ show mainRet
              putStr output
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: (Show a, Print a) => Verbosity -> a -> IO ()
showTree v tree = do
  ePutStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  ePutStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

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
    []         -> getContents >>= run 0 pProgram
    "-v":fs    -> mapM_ (runFile 1 pProgram) fs
    fs         -> mapM_ (runFile 0 pProgram) fs

