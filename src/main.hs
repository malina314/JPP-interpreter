{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
-- import Text.Parsec
-- import Text.Parsec.String
-- import Text.PrettyPrint
-- import Text.PrettyPrint.HughesPJClass
import System.IO
import System.Environment
import Mock

-- run :: FilePath -> IO ()
-- run = parseFromFile parseProgram >=> either (hPrint stderr) checkTypes

-- checkTypes :: Program -> IO ()
-- checkTypes (Program prog) =  >>= \case
--     Left err -> hPrint stderr err
--     Right _ -> eval $ Program prog

-- eval :: Program -> IO ()
-- eval (Program prog) =  >>= \case
--     Left err -> hPrint stderr err
--     _ -> pure ()

main :: IO ()
main = getArgs >>= \case
    [file] -> run file
    _ -> hPutStrLn stderr "Usage: interpreter filename"