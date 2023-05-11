module TypeChecker (checkTypes) where

import qualified AbsGramatyka

data Type = Int | Str | Bool | Void
  deriving (Eq, Show)

type ErrT = String
type OkT = Type
type Result = Either ErrT OkT

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

checkProgram :: AbsGramatyka.Program -> Result
checkProgram x = case x of
  AbsGramatyka.Program topdefs -> failure x -- TODO przejść po topdefs i
  -- w jakimś stanie trzymać typy zmiennych i funkcji, a potem 
  -- sprawdzić typy w każdej funkcji

checkIdent :: AbsGramatyka.Ident -> Result
checkIdent x = case x of
  AbsGramatyka.Ident string -> failure x

-- returns type of expression
preCheckTopDef :: AbsGramatyka.TopDef -> Result
preCheckTopDef x = case x of
  AbsGramatyka.VarDef type_ item -> failure x
  AbsGramatyka.FnDef type_ ident args block -> failure x

checkTopDef :: AbsGramatyka.TopDef -> Result
checkTopDef x = case x of
  AbsGramatyka.VarDef type_ item -> failure x
  AbsGramatyka.FnDef type_ ident args block -> failure x

checkArg :: AbsGramatyka.Arg -> Result
checkArg x = case x of
  AbsGramatyka.Arg type_ ident -> failure x
  AbsGramatyka.ArgVar type_ ident -> failure x

checkBlock :: AbsGramatyka.Block -> Result
checkBlock x = case x of
  AbsGramatyka.Block stmts -> failure x

checkStmt :: AbsGramatyka.Stmt -> Result
checkStmt x = case x of
  AbsGramatyka.Empty -> Right Void
  AbsGramatyka.BStmt block -> Right Void
  AbsGramatyka.Decl type_ item -> case checkItem item of
    Left err -> Left err
    Right Void -> Right Void
    Right t -> if t == mapType type_ then Right Void else Left "Type mismatch"
  AbsGramatyka.Ass ident expr -> Right Void
  AbsGramatyka.Ret expr -> failure x -- todo: sprawdzić czy return jest zgodny z typem funckji
  AbsGramatyka.Cond expr stmt -> Right Void
  AbsGramatyka.CondElse expr stmt1 stmt2 -> Right Void
  AbsGramatyka.While expr stmt -> Right Void
  AbsGramatyka.SExp expr -> Right Void

checkItem :: AbsGramatyka.Item -> Result
checkItem x = case x of
  AbsGramatyka.NoInit ident -> Right Void
  AbsGramatyka.Init ident expr -> checkExpr expr

mapType :: AbsGramatyka.Type -> Type
mapType x = case x of
  AbsGramatyka.Int -> Int
  AbsGramatyka.Str -> Str
  AbsGramatyka.Bool -> Bool

checkExpr :: AbsGramatyka.Expr -> Result
checkExpr x = case x of
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

checkAddOp :: AbsGramatyka.AddOp -> Result
checkAddOp x = case x of
  AbsGramatyka.Plus -> failure x
  AbsGramatyka.Minus -> failure x

checkMulOp :: AbsGramatyka.MulOp -> Result
checkMulOp x = case x of
  AbsGramatyka.Times -> failure x
  AbsGramatyka.Div -> failure x
  AbsGramatyka.Mod -> failure x

checkRelOp :: AbsGramatyka.RelOp -> Result
checkRelOp x = case x of
  AbsGramatyka.LTH -> failure x
  AbsGramatyka.LE -> failure x
  AbsGramatyka.GTH -> failure x
  AbsGramatyka.GE -> failure x
  AbsGramatyka.EQU -> failure x
  AbsGramatyka.NE -> failure x

checkTypes :: AbsGramatyka.Program -> Result
checkTypes = checkProgram