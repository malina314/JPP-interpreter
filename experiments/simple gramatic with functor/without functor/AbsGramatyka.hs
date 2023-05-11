-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language gramatyka.

module AbsGramatyka where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Program = Program [TopDef]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TopDef = VarDef Type Item
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Arg = Arg Type Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stmt = Empty | Decl Type Item | Ass Ident Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Item = NoInit Ident | Init Ident Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type = Int | Bool
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Expr = EVar Ident | ELitInt Integer | ELitTrue | ELitFalse
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data AddOp = Plus
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)
