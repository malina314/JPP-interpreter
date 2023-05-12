module TypeChecker (checkTypes) where

import qualified AbsGramatyka
import qualified Data.Map as Map

data Type = Int | Str | Bool | Void | Func (Type, [Type])
  deriving (Eq, Show)

type ErrT = String
type OkT = Type
type Result = Either ErrT OkT
-- type Loc = Int
-- data Val = I Int | S String | B Bool
--   deriving (Eq, Show)
-- type Store = Map.Map Loc Val
-- type Env = Map.Map AbsGramatyka.Ident Loc
-- type Funcs = Map.Map AbsGramatyka.Ident (Type, [Type], Env, AbsGramatyka.Block)
type Loc = Int
-- type Store = Map.Map Loc Type
-- type Env = Map.Map AbsGramatyka.Ident Loc
type Env = Map.Map AbsGramatyka.Ident Type
type Funcs = Map.Map AbsGramatyka.Ident (Type, [Type])

checkTypes :: AbsGramatyka.Program -> Result
checkTypes = checkProgram
-- checkTypes = let
--   store = Map.empty
--   env = Map.empty
--   funcs = Map.empty
--   in checkProgram store env funcs

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

foldResults :: [Result] -> Result
foldResults [] = Right Void
foldResults (x:xs) = case x of
  Left err -> Left err
  Right Void -> foldResults xs

-- checkProgram :: Show a => Store -> Env -> Funcs -> AbsGramatyka.Program' a -> Result
checkProgram :: Show a => AbsGramatyka.Program' a -> Result
checkProgram x = case x of
   AbsGramatyka.Program _ topdefs -> let
    env = typeGlobalVars topdefs Map.empty
    funcs = typeFuncs topdefs
    funcs' = Map.union funcs (Map.fromList [(AbsGramatyka.Ident "printInt", (Int, [Int])), (AbsGramatyka.Ident "printString", (Int, [Str])), (AbsGramatyka.Ident "printBool", (Bool, [Bool]))]) -- todo: redeklaracje nie są dozwolone
    in foldResults $ map (checkTopDef env funcs') topdefs

typeGlobalVars :: Show a => [AbsGramatyka.TopDef' a] -> Env -> Env
typeGlobalVars [] env = env
typeGlobalVars (x:xs) env = typeGlobalVars xs $ typeVar x env

typeVar :: Show a => AbsGramatyka.TopDef' a -> Env -> Env
typeVar x env = case x of
  AbsGramatyka.VarDef pos type_ item -> Map.insert (getIdent item) (mapType type_) env
  AbsGramatyka.FnDef _ type_ ident args block -> env

typeFunc :: Show a => AbsGramatyka.TopDef' a -> Funcs
typeFunc x = case x of
  AbsGramatyka.VarDef pos type_ item -> Map.empty
  AbsGramatyka.FnDef _ type_ ident args block -> Map.insert ident (getFuncRetType x, getFuncArgsTypes x) Map.empty

typeFuncs :: Show a => [AbsGramatyka.TopDef' a] -> Funcs
typeFuncs [] = Map.empty
typeFuncs (x:xs) = let
  funcs = typeFunc x
  funcs' = typeFuncs xs
  in Map.union funcs funcs' -- todo: redeklaracje nie są dozwolone

getIdent :: Show a => AbsGramatyka.Item' a -> AbsGramatyka.Ident
getIdent x = case x of
  AbsGramatyka.NoInit _ ident -> ident
  AbsGramatyka.Init _ ident _ -> ident

getFuncRetType :: Show a => AbsGramatyka.TopDef' a -> Type
getFuncRetType x = case x of
  AbsGramatyka.FnDef _ type_ ident args block -> mapType type_

getFuncArgsTypes :: Show a => AbsGramatyka.TopDef' a -> [Type]
getFuncArgsTypes x = case x of
  AbsGramatyka.FnDef _ type_ ident args block -> map getArgType args

getArgType :: Show a => AbsGramatyka.Arg' a -> Type
getArgType x = case x of
  AbsGramatyka.Arg _ type_ ident -> mapType type_
  AbsGramatyka.ArgVar _ type_ ident -> mapType type_

typeArg :: Show a => AbsGramatyka.Arg' a -> Env -> Env
typeArg x env = case x of
  AbsGramatyka.Arg _ type_ ident -> Map.insert ident (mapType type_) env
  AbsGramatyka.ArgVar _ type_ ident -> Map.insert ident (mapType type_) env

typeArgs :: Show a => [AbsGramatyka.Arg' a] -> Env -> Env
typeArgs [] env = env
typeArgs (x:xs) env = typeArgs xs $ typeArg x env

checkTopDef :: Show a => Env -> Funcs -> AbsGramatyka.TopDef' a -> Result
checkTopDef env funcs x = case x of
  AbsGramatyka.VarDef pos type_ item -> Right Void
  AbsGramatyka.FnDef _ type_ ident args block -> let
    env' = typeArgs args env
    in checkBlock env' funcs block

checkBlock :: Show a => Env -> Funcs -> AbsGramatyka.Block' a -> Result
checkBlock env funcs x = case x of
  AbsGramatyka.Block _ stmts -> snd $ checkStmts env funcs stmts

checkStmts :: Show a => Env -> Funcs -> [AbsGramatyka.Stmt' a] -> (Env, Result)
checkStmts env funcs [] = (env, Right Void)
checkStmts env funcs (x:xs) = let
  (env', res) = checkStmt env funcs x
  in case res of
  Left err -> (env', Left err)
  Right _ -> checkStmts env' funcs xs

checkStmt :: Show a => Env -> Funcs -> AbsGramatyka.Stmt' a -> (Env, Result)
checkStmt env funcs x = case x of
  AbsGramatyka.Empty _ -> (env, Right Void)
  AbsGramatyka.BStmt _ block -> (env, checkBlock env funcs block)
  AbsGramatyka.Decl pos type_ item -> case checkItemWithType pos type_ item of
    Left err -> (env, Left err)
    Right t -> (Map.insert (getIdent item) t env, Right Void)
  AbsGramatyka.Ass pos ident expr -> case checkExpr expr of
    Left err -> (env, Left err)
    Right t -> (env, checkIdentWithType pos ident t)
  AbsGramatyka.Ret _ expr -> (env, failure x) -- todo: sprawdzić czy return jest zgodny z typem funckji
  AbsGramatyka.Cond pos expr stmt -> case checkExpr expr of
    Left err -> (env, Left err)
    Right t -> (env, if t == Bool then snd $ checkStmt env funcs stmt else Left $ "Type mismatch at " ++ show pos ++ ". Expected Bool" ++ ", got " ++ show t)
  AbsGramatyka.CondElse pos expr stmt1 stmt2 -> case checkExpr expr of
    Left err -> (env, Left err)
    Right t -> (env, if t == Bool then foldResults $ map snd [checkStmt env funcs stmt1, checkStmt env funcs stmt2] else Left $ "Type mismatch at " ++ show pos ++ ". Expected Bool" ++ ", got " ++ show t)
  AbsGramatyka.While pos expr stmt -> case checkExpr expr of
    Left err -> (env, Left err)
    Right t -> (env, if t == Bool then snd $ checkStmt env funcs stmt else Left $ "Type mismatch at " ++ show pos ++ ". Expected Bool" ++ ", got " ++ show t)
  AbsGramatyka.SExp _ expr -> (env, checkExpr expr)

checkItem :: Show a => AbsGramatyka.Item' a -> Result
checkItem x = case x of
  AbsGramatyka.NoInit _ ident -> Right Void
  AbsGramatyka.Init _ ident expr -> checkExpr expr

checkItemWithType :: Show a => a -> AbsGramatyka.Type' a -> AbsGramatyka.Item' a -> Result
checkItemWithType pos type_ item = case checkItem item of
  Left err -> Left err
  Right Void -> Right Void
  Right t -> if t == mapType type_ then Right Void else Left $ "Type mismatch at " ++ show pos ++ ". Expected " ++ show type_ ++ ", got " ++ show t

checkIdentWithType :: a -> AbsGramatyka.Ident -> Type -> Result
checkIdentWithType pos x type_ = case x of
  AbsGramatyka.Ident string -> failure x -- todo: odczytać typ ze store

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
