-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SkelGramatyka where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsGramatyka

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: AbsGramatyka.Ident -> Result
transIdent x = case x of
  AbsGramatyka.Ident string -> failure x

transProgram :: AbsGramatyka.Program -> Result
transProgram x = case x of
  AbsGramatyka.Program topdefs -> failure x

transTopDef :: AbsGramatyka.TopDef -> Result
transTopDef x = case x of
  AbsGramatyka.VarDef type_ item -> failure x
  AbsGramatyka.FnDef type_ ident args block -> failure x

transArg :: AbsGramatyka.Arg -> Result
transArg x = case x of
  AbsGramatyka.Arg type_ ident -> failure x
  AbsGramatyka.ArgVar type_ ident -> failure x

transBlock :: AbsGramatyka.Block -> Result
transBlock x = case x of
  AbsGramatyka.Block stmts -> failure x

transStmt :: AbsGramatyka.Stmt -> Result
transStmt x = case x of
  AbsGramatyka.Empty -> failure x
  AbsGramatyka.BStmt block -> failure x
  AbsGramatyka.Decl type_ item -> failure x
  AbsGramatyka.Ass ident expr -> failure x
  AbsGramatyka.Ret expr -> failure x
  AbsGramatyka.Cond expr stmt -> failure x
  AbsGramatyka.CondElse expr stmt1 stmt2 -> failure x
  AbsGramatyka.While expr stmt -> failure x
  AbsGramatyka.SExp expr -> failure x

transItem :: AbsGramatyka.Item -> Result
transItem x = case x of
  AbsGramatyka.NoInit ident -> failure x
  AbsGramatyka.Init ident expr -> failure x

transType :: AbsGramatyka.Type -> Result
transType x = case x of
  AbsGramatyka.Int -> failure x
  AbsGramatyka.Str -> failure x
  AbsGramatyka.Bool -> failure x

transExpr :: AbsGramatyka.Expr -> Result
transExpr x = case x of
  AbsGramatyka.EVar ident -> failure x
  AbsGramatyka.ELitInt integer -> failure x
  AbsGramatyka.ELitTrue -> failure x
  AbsGramatyka.ELitFalse -> failure x
  AbsGramatyka.EApp ident exprs -> failure x
  AbsGramatyka.EString string -> failure x
  AbsGramatyka.Neg expr -> failure x
  AbsGramatyka.Not expr -> failure x
  AbsGramatyka.EMul expr1 mulop expr2 -> failure x
  AbsGramatyka.EAdd expr1 addop expr2 -> failure x
  AbsGramatyka.ERel expr1 relop expr2 -> failure x
  AbsGramatyka.EAnd expr1 expr2 -> failure x
  AbsGramatyka.EOr expr1 expr2 -> failure x

transAddOp :: AbsGramatyka.AddOp -> Result
transAddOp x = case x of
  AbsGramatyka.Plus -> failure x
  AbsGramatyka.Minus -> failure x

transMulOp :: AbsGramatyka.MulOp -> Result
transMulOp x = case x of
  AbsGramatyka.Times -> failure x
  AbsGramatyka.Div -> failure x
  AbsGramatyka.Mod -> failure x

transRelOp :: AbsGramatyka.RelOp -> Result
transRelOp x = case x of
  AbsGramatyka.LTH -> failure x
  AbsGramatyka.LE -> failure x
  AbsGramatyka.GTH -> failure x
  AbsGramatyka.GE -> failure x
  AbsGramatyka.EQU -> failure x
  AbsGramatyka.NE -> failure x
