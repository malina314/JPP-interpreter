module TypeChecker (checkTypes) where

import qualified AbsGramatyka
import qualified Data.Map as Map

data Type = Int | Str | Bool | Void | Func (Type, [Type])
  deriving (Eq, Show)

type ErrT = String
type OkT = Type
type Result = Either ErrT OkT
type Env = Map.Map AbsGramatyka.Ident Type
type Funcs = Map.Map AbsGramatyka.Ident (Type, [Type])

checkTypes :: AbsGramatyka.Program -> Result
checkTypes = checkProgram

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

foldResults :: [Result] -> Result
foldResults [] = Right Void
foldResults (x:xs) = case x of
  Left err -> Left err
  Right _ -> foldResults xs

checkProgram :: Show a => AbsGramatyka.Program' a -> Result
checkProgram x = case x of
   AbsGramatyka.Program _ topdefs -> let
    env = typeGlobalVars topdefs Map.empty
    funcs = typeFuncs topdefs
    funcs' = addBuiltIn funcs
    in checkMain funcs' >>= \_ -> foldResults $ map (checkTopDef env funcs') topdefs

addBuiltIn :: Funcs -> Funcs
addBuiltIn funcs = let
  names = [("printInt", Int), ("printString", Str), ("printBool", Bool), ("printLnInt", Int), ("printLnString", Str), ("printLnBool", Bool)]
  in Map.union funcs (Map.fromList $ map (\(n, t) -> (AbsGramatyka.Ident n, (Int, [t]))) names)

checkMain :: Funcs -> Result
checkMain funcs = case Map.lookup (AbsGramatyka.Ident "main") funcs of
  Nothing -> Left "No main function"
  Just (type_, args) -> if type_ == Int && args == [] then Right Void else Left "Wrong main function type"

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
    env'' = Map.insert (AbsGramatyka.Ident "__return_type__") (mapType type_) env'
    in checkBlock env'' funcs block

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
  AbsGramatyka.Decl pos type_ item -> case checkItemWithType env funcs pos type_ item of
    Left err -> (env, Left err)
    Right t -> (Map.insert (getIdent item) t env, Right Void)
  AbsGramatyka.Ass pos ident expr -> case checkExpr env funcs expr of
    Left err -> (env, Left err)
    Right t -> (env, checkIdentWithType pos ident t env)
  AbsGramatyka.Ret pos expr -> case Map.lookup (AbsGramatyka.Ident "__return_type__") env of
    Nothing -> (env, Left $ "Unexpected problem occured at " ++ show pos) -- this should never happen
    Just t -> (env, checkExprWithType env funcs pos t expr)
  AbsGramatyka.Cond pos expr stmt -> case checkExpr env funcs expr of
    Left err -> (env, Left err)
    Right t -> (env, if t == Bool then snd $ checkStmt env funcs stmt else Left $ "Type mismatch at " ++ show pos ++ ". Expected Bool" ++ ", got " ++ show t ++ " - [checkStmt@1]")
  AbsGramatyka.CondElse pos expr stmt1 stmt2 -> case checkExpr env funcs expr of
    Left err -> (env, Left err)
    Right t -> (env, if t == Bool then foldResults $ map snd [checkStmt env funcs stmt1, checkStmt env funcs stmt2] else Left $ "Type mismatch at " ++ show pos ++ ". Expected Bool" ++ ", got " ++ show t ++ " - [checkStmt@2]")
  AbsGramatyka.While pos expr stmt -> case checkExpr env funcs expr of
    Left err -> (env, Left err)
    Right t -> (env, if t == Bool then snd $ checkStmt env funcs stmt else Left $ "Type mismatch at " ++ show pos ++ ". Expected Bool" ++ ", got " ++ show t ++ " - [checkStmt@3]")
  AbsGramatyka.SExp _ expr -> (env, checkExpr env funcs expr)

checkItem :: Show a => Env -> Funcs -> AbsGramatyka.Item' a -> Result
checkItem env funcs x = case x of
  AbsGramatyka.NoInit _ ident -> Right Void
  AbsGramatyka.Init _ ident expr -> checkExpr env funcs expr

checkItemWithType :: Show a => Env -> Funcs -> a -> AbsGramatyka.Type' a -> AbsGramatyka.Item' a -> Result
checkItemWithType env funcs pos type_ item = let
  t = mapType type_
  in case checkItem env funcs item of
  Left err -> Left err
  Right Void -> Right t
  Right t' -> if t' == t then Right t else Left $ "Type mismatch at " ++ show pos ++ ". Expected " ++ show type_ ++ ", got " ++ show t ++ " - [checkItemWithType]"

checkIdentWithType :: Show a => a -> AbsGramatyka.Ident -> Type -> Env -> Result
checkIdentWithType pos x type_ env = case Map.lookup x env of
    Nothing -> Left $ "Undeclared variable " ++ show x ++ " at " ++ show pos ++ " - [checkIdentWithType@1]"
    Just t -> if t == type_ then Right Void else Left $ "Type mismatch at " ++ show pos ++ ". Expected " ++ show t ++ ", got " ++ show type_ ++ " - [checkIdentWithType@2]"

mapType :: Show a => AbsGramatyka.Type' a -> Type
mapType x = case x of
  AbsGramatyka.Int _ -> Int
  AbsGramatyka.Str _ -> Str
  AbsGramatyka.Bool _ -> Bool

checkExpr :: Show a => Env -> Funcs -> AbsGramatyka.Expr' a -> Result
checkExpr env funcs x = case x of
  AbsGramatyka.EVar _ ident -> case Map.lookup ident env of
    Nothing -> Left $ "Undeclared variable " ++ show ident ++ " at " ++ show x ++ " - [checkExpr]"
    Just t -> Right t
  AbsGramatyka.ELitInt _ integer -> Right Int
  AbsGramatyka.ELitTrue _ -> Right Bool
  AbsGramatyka.ELitFalse _ -> Right Bool
  AbsGramatyka.EApp pos ident exprs -> checkApp env funcs pos ident exprs
  AbsGramatyka.EString _ string -> Right Str
  AbsGramatyka.Neg pos expr -> checkExprWithType env funcs pos Int expr
  AbsGramatyka.Not pos expr -> checkExprWithType env funcs pos Bool expr
  AbsGramatyka.EMul pos expr1 mulop expr2 -> checkBinaryIntOp env funcs pos expr1 expr2
  AbsGramatyka.EAdd pos expr1 addop expr2 -> checkBinaryIntOp env funcs pos expr1 expr2
  AbsGramatyka.ERel pos expr1 relop expr2 -> checkBinaryRelOp env funcs pos expr1 expr2
  AbsGramatyka.EAnd pos expr1 expr2 -> checkBinaryBoolOp env funcs pos expr1 expr2
  AbsGramatyka.EOr pos expr1 expr2 -> checkBinaryBoolOp env funcs pos expr1 expr2

checkApp :: Show a => Env -> Funcs -> a -> AbsGramatyka.Ident -> [AbsGramatyka.Expr' a] -> Result
checkApp env funcs pos ident exprs = case Map.lookup ident funcs of
  Nothing -> Left $ "Undeclared function " ++ show ident ++ " at " ++ show pos
  Just (type_, args) -> if length args == length exprs then
    case foldResults $ map (\(arg, expr) -> checkExprWithType env funcs pos arg expr) $ zip args exprs of
      Left err -> Left err
      Right _ -> Right type_
  else Left $ "Wrong number of arguments at " ++ show pos ++ ". Expected " ++ show (length args) ++ ", got " ++ show (length exprs) ++ " - [checkApp]"

checkExprWithType :: Show a => Env -> Funcs -> a -> Type -> AbsGramatyka.Expr' a -> Result
checkExprWithType env funcs pos type_ expr = case checkExpr env funcs expr of
  Left err -> Left err
  Right t -> if t == type_ then Right t else Left $ "Type mismatch at " ++ show pos ++ ". Expected " ++ show type_ ++ ", got " ++ show t ++ " - [checkExprWithType]"

checkBinaryIntOp :: Show a => Env -> Funcs -> a -> AbsGramatyka.Expr' a -> AbsGramatyka.Expr' a -> Result
checkBinaryIntOp env funcs pos expr1 expr2 = case checkExpr env funcs expr1 of
    Left err -> Left err
    Right t -> if t == Int then
      case checkExpr env funcs expr2 of
        Left err -> Left err
        Right t -> if t == Int then Right Int else Left $ "Type mismatch at " ++ show pos ++ " expected Int, got " ++ show t ++ " - [checkBinaryIntOp@1]"
    else Left $ "Type mismatch at " ++ show pos ++ " expected Int, got " ++ show t ++ " - [checkBinaryIntOp@2]"

checkBinaryRelOp :: Show a => Env -> Funcs -> a -> AbsGramatyka.Expr' a -> AbsGramatyka.Expr' a -> Result
checkBinaryRelOp env funcs pos expr1 expr2 = case checkExpr env funcs expr1 of
    Left err -> Left err
    Right t -> if t == Int then
      case checkExpr env funcs expr2 of
        Left err -> Left err
        Right t -> if t == Int then Right Bool else Left $ "Type mismatch at " ++ show pos ++ " expected Int, got " ++ show t ++ " - [checkBinaryRelOp@1]"
    else Left $ "Type mismatch at " ++ show pos ++ " expected Int, got " ++ show t ++ " - [checkBinaryRelOp@2]"

checkBinaryBoolOp :: Show a => Env -> Funcs -> a -> AbsGramatyka.Expr' a -> AbsGramatyka.Expr' a -> Result
checkBinaryBoolOp env funcs pos expr1 expr2 = case checkExpr env funcs expr1 of
    Left err -> Left err
    Right t -> if t == Bool then
      case checkExpr env funcs expr2 of
        Left err -> Left err
        Right t -> if t == Bool then Right Bool else Left $ "Type mismatch at " ++ show pos ++ " expected Bool, got " ++ show t ++ " - [checkBinaryBoolOp@1]"
    else Left $ "Type mismatch at " ++ show pos ++ " expected Bool, got " ++ show t ++ " - [checkBinaryBoolOp@2]"
