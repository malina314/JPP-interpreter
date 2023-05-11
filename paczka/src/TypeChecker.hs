module TypeChecker (checkTypes) where

import qualified AbsGramatyka

data Type = Int | Str | Bool | Void
  deriving (Eq, Show)

type ErrT = String
type OkT = Type
type Result = Either ErrT OkT

checkTypes :: AbsGramatyka.Program -> Result
checkTypes = checkProgram

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

checkIdent :: AbsGramatyka.Ident -> Result
checkIdent x = case x of
  AbsGramatyka.Ident string -> failure x

checkProgram :: Show a => AbsGramatyka.Program' a -> Result
checkProgram x = case x of
  AbsGramatyka.Program _ topdefs -> failure x -- TODO przejść po topdefs i
  -- w jakimś stanie trzymać typy zmiennych i funkcji, a potem 
  -- sprawdzić typy w każdej funkcji

checkTopDef :: Show a => AbsGramatyka.TopDef' a -> Result
checkTopDef x = case x of
  AbsGramatyka.VarDef _ type_ item -> failure x
  AbsGramatyka.FnDef _ type_ ident args block -> failure x

checkArg :: Show a => AbsGramatyka.Arg' a -> Result
checkArg x = case x of
  AbsGramatyka.Arg _ type_ ident -> failure x
  AbsGramatyka.ArgVar _ type_ ident -> failure x

checkBlock :: Show a => AbsGramatyka.Block' a -> Result
checkBlock x = case x of
  AbsGramatyka.Block _ stmts -> failure x

checkStmt :: Show a => AbsGramatyka.Stmt' a -> Result
checkStmt x = case x of
  AbsGramatyka.Empty _ -> Right Void
  AbsGramatyka.BStmt _ block -> Right Void
  AbsGramatyka.Decl pos type_ item -> case checkItem item of
    Left err -> Left err
    Right Void -> Right Void
    Right t -> if t == mapType type_ then Right Void else Left $ "Type mismatch at " ++ show pos
  AbsGramatyka.Ass _ ident expr -> Right Void
  AbsGramatyka.Ret _ expr -> Right Void -- todo: sprawdzić czy return jest zgodny z typem funckji
  AbsGramatyka.Cond _ expr stmt -> Right Void
  AbsGramatyka.CondElse _ expr stmt1 stmt2 -> Right Void
  AbsGramatyka.While _ expr stmt -> Right Void
  AbsGramatyka.SExp _ expr -> Right Void

checkItem :: Show a => AbsGramatyka.Item' a -> Result
checkItem x = case x of
  AbsGramatyka.NoInit _ ident -> Right Void
  AbsGramatyka.Init _ ident expr -> checkExpr expr

mapType :: Show a => AbsGramatyka.Type' a -> Type
mapType x = case x of
  AbsGramatyka.Int _ -> Int
  AbsGramatyka.Str _ -> Str
  AbsGramatyka.Bool _ -> Bool

checkExpr :: Show a => AbsGramatyka.Expr' a -> Result
checkExpr x = case x of
  AbsGramatyka.EVar _ ident -> failure x
  AbsGramatyka.ELitInt _ integer -> failure x
  AbsGramatyka.ELitTrue _ -> failure x
  AbsGramatyka.ELitFalse _ -> failure x
  AbsGramatyka.EApp _ ident exprs -> failure x
  AbsGramatyka.EString _ string -> failure x
  AbsGramatyka.Neg _ expr -> failure x
  AbsGramatyka.Not _ expr -> failure x
  AbsGramatyka.EMul _ expr1 mulop expr2 -> failure x
  AbsGramatyka.EAdd _ expr1 addop expr2 -> failure x
  AbsGramatyka.ERel _ expr1 relop expr2 -> failure x
  AbsGramatyka.EAnd _ expr1 expr2 -> failure x
  AbsGramatyka.EOr _ expr1 expr2 -> failure x

checkAddOp :: Show a => AbsGramatyka.AddOp' a -> Result
checkAddOp x = case x of
  AbsGramatyka.Plus _ -> failure x
  AbsGramatyka.Minus _ -> failure x

checkMulOp :: Show a => AbsGramatyka.MulOp' a -> Result
checkMulOp x = case x of
  AbsGramatyka.Times _ -> failure x
  AbsGramatyka.Div _ -> failure x
  AbsGramatyka.Mod _ -> failure x

checkRelOp :: Show a => AbsGramatyka.RelOp' a -> Result
checkRelOp x = case x of
  AbsGramatyka.LTH _ -> failure x
  AbsGramatyka.LE _ -> failure x
  AbsGramatyka.GTH _ -> failure x
  AbsGramatyka.GE _ -> failure x
  AbsGramatyka.EQU _ -> failure x
  AbsGramatyka.NE _ -> failure x








