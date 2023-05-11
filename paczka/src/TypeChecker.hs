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
  AbsGramatyka.Ident string -> failure x -- todo: zapisać ident w stanie i sprawdzić czy nie jest już zadeklarowany

checkProgram :: Show a => AbsGramatyka.Program' a -> Result
checkProgram x = case x of
  AbsGramatyka.Program _ topdefs -> failure x -- TODO przejść po topdefs i
                                              -- w jakimś stanie trzymać typy zmiennych i funkcji, a potem 
                                              -- sprawdzić typy w każdej funkcji

checkTopDef :: Show a => AbsGramatyka.TopDef' a -> Result
checkTopDef x = case x of
  AbsGramatyka.VarDef pos type_ item -> checkItemWithType pos type_ item -- todo: potencjalnie ident trzeba zapisać w stanie
  AbsGramatyka.FnDef _ type_ ident args block -> failure x -- todo: potencjalnie ident trzeba zapisać w stanie

checkArg :: Show a => AbsGramatyka.Arg' a -> Result
checkArg x = case x of
  AbsGramatyka.Arg _ type_ ident -> Right $ mapType type_ -- todo: potencjalnie ident trzeba zapisać w stanie
  AbsGramatyka.ArgVar _ type_ ident -> Right $ mapType type_ -- todo: potencjalnie ident trzeba zapisać w stanie

checkBlock :: Show a => AbsGramatyka.Block' a -> Result
checkBlock x = case x of
  AbsGramatyka.Block _ stmts -> Right Void

checkStmt :: Show a => AbsGramatyka.Stmt' a -> Result
checkStmt x = case x of
  AbsGramatyka.Empty _ -> Right Void
  AbsGramatyka.BStmt _ block -> Right Void
  AbsGramatyka.Decl pos type_ item -> checkItemWithType pos type_ item
  AbsGramatyka.Ass _ ident expr -> Right Void
  AbsGramatyka.Ret _ expr -> failure x -- todo: sprawdzić czy return jest zgodny z typem funckji
  AbsGramatyka.Cond _ expr stmt -> Right Void
  AbsGramatyka.CondElse _ expr stmt1 stmt2 -> Right Void
  AbsGramatyka.While _ expr stmt -> Right Void
  AbsGramatyka.SExp _ expr -> Right Void

checkItem :: Show a => AbsGramatyka.Item' a -> Result
checkItem x = case x of
  AbsGramatyka.NoInit _ ident -> Right Void
  AbsGramatyka.Init _ ident expr -> checkExpr expr

checkItemWithType :: Show a => a -> AbsGramatyka.Type' a -> AbsGramatyka.Item' a -> Result
checkItemWithType pos type_ item = case checkItem item of
  Left err -> Left err
  Right Void -> Right Void
  Right t -> if t == mapType type_ then Right Void else Left $ "Type mismatch at " ++ show pos ++ ". Expected " ++ show type_ ++ ", got " ++ show t

mapType :: Show a => AbsGramatyka.Type' a -> Type
mapType x = case x of
  AbsGramatyka.Int _ -> Int
  AbsGramatyka.Str _ -> Str
  AbsGramatyka.Bool _ -> Bool

checkExpr :: Show a => AbsGramatyka.Expr' a -> Result
checkExpr x = case x of
  AbsGramatyka.EVar _ ident -> failure x -- todo: odczytać ze stanu
  AbsGramatyka.ELitInt _ integer -> Right Int
  AbsGramatyka.ELitTrue _ -> Right Bool
  AbsGramatyka.ELitFalse _ -> Right Bool
  AbsGramatyka.EApp _ ident exprs -> failure x -- todo: odczytać ze stanu funkcji i sprawdzić typy argumentów
  AbsGramatyka.EString _ string -> Right Str
  AbsGramatyka.Neg _ expr -> Right Int
  AbsGramatyka.Not _ expr -> Right Bool
  AbsGramatyka.EMul pos expr1 mulop expr2 -> checkBinaryIntOp pos expr1 expr2
  AbsGramatyka.EAdd pos expr1 addop expr2 -> checkBinaryIntOp pos expr1 expr2
  AbsGramatyka.ERel pos expr1 relop expr2 -> checkBinaryIntOp pos expr1 expr2
  AbsGramatyka.EAnd pos expr1 expr2 -> checkBinaryBoolOp pos expr1 expr2
  AbsGramatyka.EOr pos expr1 expr2 -> checkBinaryBoolOp pos expr1 expr2

checkBinaryIntOp :: Show a => a -> AbsGramatyka.Expr' a -> AbsGramatyka.Expr' a -> Result
checkBinaryIntOp pos expr1 expr2 = case checkExpr expr1 of
    Left err -> Left err
    Right t -> if t == Int then
      case checkExpr expr2 of
        Left err -> Left err
        Right t -> if t == Int then Right Int else Left $ "Type mismatch at " ++ show pos ++ " expected Int"
    else Left $ "Type mismatch at " ++ show pos ++ " expected Int"

checkBinaryBoolOp :: Show a => a -> AbsGramatyka.Expr' a -> AbsGramatyka.Expr' a -> Result
checkBinaryBoolOp pos expr1 expr2 = case checkExpr expr1 of
    Left err -> Left err
    Right t -> if t == Bool then
      case checkExpr expr2 of
        Left err -> Left err
        Right t -> if t == Bool then Right Bool else Left $ "Type mismatch at " ++ show pos ++ " expected Bool"
    else Left $ "Type mismatch at " ++ show pos ++ " expected Bool"
