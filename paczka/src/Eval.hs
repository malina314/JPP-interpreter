module Eval (eval) where

import qualified AbsGramatyka
import qualified Data.Map as Map

data Value = I Int | S String | B Bool | Void
  deriving (Eq, Show)

data FuncBody = Code [AbsGramatyka.Arg] AbsGramatyka.Block | BuiltIn String
  deriving (Eq, Show)

type ErrorMsg = String
type OutputMsg = String

type Ident = String
type Loc = Int
type Store = Map.Map Loc Value
type Env = Map.Map Ident Loc
type Funcs = Map.Map Ident FuncBody

type Memory = (Store, Env, Env, Funcs, Loc, Value, OutputMsg) -- sotre, env, localEnv, funcs, newloc, last_return_value, output

type Result = Either (ErrorMsg, OutputMsg) Memory

-- exported function

eval :: AbsGramatyka.Program -> Result
eval = evalProgram


-- utility functions

failure :: Show a => a -> Result
failure x = Left $ ("Undefined case: " ++ show x, "")

getIdent :: AbsGramatyka.Item -> AbsGramatyka.Ident
getIdent x = case x of
  AbsGramatyka.NoInit _ ident -> ident
  AbsGramatyka.Init _ ident _ -> ident

getIdentName :: AbsGramatyka.Ident -> Ident
getIdentName (AbsGramatyka.Ident name) = name

getItemName :: AbsGramatyka.Item -> Ident
getItemName = getIdentName . getIdent

getArgName :: AbsGramatyka.Arg -> Ident
getArgName (AbsGramatyka.Arg _ _ ident) = getIdentName ident

getIdentLoc :: AbsGramatyka.Ident -> Env -> Env -> Loc
getIdentLoc = getVarLoc . getIdentName

getItemLoc :: AbsGramatyka.Item -> Env -> Env -> Loc
getItemLoc = getVarLoc . getItemName

getVarLoc :: Ident -> Env -> Env -> Loc
getVarLoc ident env localEnv = case Map.lookup ident localEnv of
  Just loc -> loc
  Nothing -> case Map.lookup ident env of
    Just loc -> loc

defaultVal :: AbsGramatyka.Type -> Value
defaultVal x = case x of
  AbsGramatyka.Int _ -> I 0
  AbsGramatyka.Str _ -> S ""
  AbsGramatyka.Bool _ -> B False

str :: Value -> String
str (I x) = show x
str (S x) = x
str (B x) = show x
str Void = ""

isArg :: AbsGramatyka.Arg -> Bool
isArg x = case x of
  AbsGramatyka.Arg _ _ _ -> True
  AbsGramatyka.ArgVar _ _ _ -> False

-- (>>>>>>) :: Result -> (Memory -> Result) -> Result
-- (>>>>>>) (Left err) _ = Left err
-- (>>>>>>) (Right r) lambda = lambda r

-- auxiliary functions

addBuiltIn :: Funcs -> Funcs
addBuiltIn funcs = let
  names = ["printInt", "printString", "printBool", "printLnInt", "printLnString", "printLnBool"]
  in Map.union funcs (Map.fromList $ map (\n -> (n, BuiltIn n)) names)

mapVarsToLocs :: [AbsGramatyka.TopDef] -> Env -> Loc -> (Env, Loc)
mapVarsToLocs [] env newloc = (env, newloc)
mapVarsToLocs (x:xs) env newloc = let
  (env', newloc') = mapVarToLoc x env newloc
  in mapVarsToLocs xs env' newloc'

mapVarToLoc :: AbsGramatyka.TopDef -> Env -> Loc -> (Env, Loc)
mapVarToLoc x env newloc = case x of
  AbsGramatyka.VarDef pos type_ item -> (Map.insert (getItemName item) newloc env, newloc + 1)
  AbsGramatyka.FnDef _ _ _ _ _ -> (env, newloc)

mapFuncsToCode :: [AbsGramatyka.TopDef] -> Funcs -> Funcs
mapFuncsToCode [] funcs = funcs
mapFuncsToCode (x:xs) funcs = mapFuncsToCode xs $ mapFuncToCode x funcs

mapFuncToCode :: AbsGramatyka.TopDef -> Funcs -> Funcs
mapFuncToCode x funcs = case x of
  AbsGramatyka.VarDef _ _ _ -> funcs
  AbsGramatyka.FnDef _ type_ ident args block -> Map.insert (getIdentName ident) (Code args block) funcs

initVars :: Memory -> [AbsGramatyka.TopDef] -> Result
initVars mem [] = Right mem
initVars mem (x:xs) = initVar mem x >>= \mem -> initVars mem xs

initVar :: Memory -> AbsGramatyka.TopDef -> Result
initVar mem x = case x of
  AbsGramatyka.VarDef _ type_ item -> initItem mem (defaultVal type_) item
  AbsGramatyka.FnDef _ _ _ _ _ -> Right mem

initItem :: Memory -> Value -> AbsGramatyka.Item -> Result
initItem mem default_ x = case x of
  AbsGramatyka.NoInit _ ident -> let 
    (store, env, localEnv, funcs, newloc, v, output) = mem
    in Right (Map.insert (getItemLoc x env localEnv) default_ store, env, localEnv, funcs, newloc, v, output)
  AbsGramatyka.Init _ ident expr -> evalExpr mem expr >>= \(store, env, localEnv, funcs, newloc, v, output) ->
    Right (Map.insert (getItemLoc x env localEnv) v store, env, localEnv, funcs, newloc, v, output)

makeLocalEnv :: Memory -> [AbsGramatyka.Arg] -> [Value] -> Memory
makeLocalEnv (store, env, _, funcs, newloc, _, output) args argsVals = let
  (args', argsVals') = unzip $ map (\(a, v) -> (getArgName a, v)) $ filter (\(a, _) -> isArg a) $ zip args argsVals
  n = length args'
  store' = Map.union store $ Map.fromList $ zip [newloc..] argsVals'
  localEnv' = Map.fromList $ zip args' [newloc..]
  newloc' = newloc + n
  in (store', env, localEnv', funcs, newloc', Void, output)


-- eval functions

evalProgram :: AbsGramatyka.Program -> Result
evalProgram x = case x of
  AbsGramatyka.Program _ topdefs -> let
    (env, newloc) = mapVarsToLocs topdefs Map.empty 0
    funcs = mapFuncsToCode topdefs Map.empty
    funcs' = addBuiltIn funcs
    res = initVars (Map.empty, env, Map.empty, funcs', newloc, Void, "") topdefs
    in res >>= evalMain

evalMain :: Memory -> Result
evalMain mem = evalFunc mem "main" []

evalFunc :: Memory -> Ident -> [Value] -> Result
evalFunc mem ident argsVals = let
  (_, _, _, funcs, _, _, _) = mem
  in case Map.lookup ident funcs of
    Just (Code args block) -> evalBlock (makeLocalEnv mem args argsVals) block
    Just (BuiltIn name) -> evalPrint mem name $ head argsVals
    Nothing -> failure "evalFunc" -- this should never happen

evalPrint :: Memory -> Ident -> Value -> Result
evalPrint (store, env, localEnv, funcs, newloc, v, output) name val = case name of
  "printInt" -> Right (store, env, localEnv, funcs, newloc, I 0, output ++ str val)
  "printString" -> Right (store, env, localEnv, funcs, newloc, I 0, output ++ str val)
  "printBool" -> Right (store, env, localEnv, funcs, newloc, I 0, output ++ str val)
  "printLnInt" -> Right (store, env, localEnv, funcs, newloc, I 0, output ++ str val ++ "\n")
  "printLnString" -> Right (store, env, localEnv, funcs, newloc, I 0, output ++ str val ++ "\n")
  "printLnBool" -> Right (store, env, localEnv, funcs, newloc, I 0, output ++ str val ++ "\n")

evalBlock :: Memory -> AbsGramatyka.Block -> Result
evalBlock (store, env, localEnv, funcs, newloc, v, output) x = case x of
  AbsGramatyka.Block _ stmts -> evalStmts (store, env, localEnv, funcs, newloc, v, output) stmts >>= \(_, _, _, _, _, _, output') -> Right (store, env, localEnv, funcs, newloc, v, output')

evalStmts :: Memory -> [AbsGramatyka.Stmt] -> Result
evalStmts mem [] = Right mem
evalStmts mem (x:xs) = evalStmt mem x >>= \mem -> evalStmts mem xs

evalStmt :: Memory -> AbsGramatyka.Stmt -> Result
evalStmt mem x = case x of
  AbsGramatyka.Empty _ -> Right mem
  AbsGramatyka.BStmt _ block -> evalBlock mem block
  AbsGramatyka.Decl _ type_ item -> evalItem mem (defaultVal type_) item
  AbsGramatyka.Ass _ ident expr -> evalExpr mem expr >>= \mem' -> let
    (store, env, localEnv, funcs, newloc, v, output) = mem'
    store' = Map.insert (getIdentLoc ident env localEnv) v store
    in Right (store', env, localEnv, funcs, newloc, Void, output)
  AbsGramatyka.Ret _ expr -> failure x
  AbsGramatyka.Cond _ expr stmt -> evalExpr mem expr >>= \mem' -> let (_, _, _, _, _, B b, _) = mem' in
    if b then evalStmt mem' stmt else Right mem'
  AbsGramatyka.CondElse _ expr stmt1 stmt2 -> evalExpr mem expr >>= \mem' -> let (_, _, _, _, _, B b, _) = mem' in
    if b then evalStmt mem' stmt1 else evalStmt mem' stmt2
  AbsGramatyka.While pos expr stmt -> evalExpr mem expr >>= \mem' -> let (_, _, _, _, _, B b, _) = mem' in
    if b then evalStmts mem' [stmt, AbsGramatyka.While pos expr stmt] else Right mem'
  AbsGramatyka.SExp _ expr -> evalExpr mem expr

evalItem :: Memory -> Value -> AbsGramatyka.Item -> Result
evalItem mem default_ x = case x of
  AbsGramatyka.NoInit _ ident -> let
    (store, env, localEnv, funcs, newloc, v, output) = mem
    localEnv' = Map.insert (getIdentName ident) newloc localEnv
    store' = Map.insert newloc default_ store
    newloc' = newloc + 1
    in Right (store', env, localEnv', funcs, newloc', Void, output)
  AbsGramatyka.Init _ ident expr -> evalExpr mem expr >>= \mem' -> let
    (store, env, localEnv, funcs, newloc, v, output) = mem'
    localEnv' = Map.insert (getIdentName ident) newloc localEnv
    store' = Map.insert newloc v store
    newloc' = newloc + 1
    in Right (store', env, localEnv', funcs, newloc', Void, output)

evalExpr :: Memory -> AbsGramatyka.Expr -> Result
evalExpr = undefined

------------------------------------------------------------------------------------------------------------------------



-- -- evalItem :: Env -> Funcs -> AbsGramatyka.Item -> Result
-- -- evalItem env funcs x = case x of
-- --   AbsGramatyka.NoInit _ ident -> Right Void
-- --   AbsGramatyka.Init _ ident expr -> evalExpr env funcs expr

-- -- evalItemWithType :: Env -> Funcs -> a -> AbsGramatyka.Type -> AbsGramatyka.Item -> Result
-- -- evalItemWithType env funcs pos type_ item = let
-- --   t = mapType type_
-- --   in case evalItem env funcs item of
-- --   Left err -> Left err
-- --   Right Void -> Right t
-- --   Right t' -> if t' == t then Right t else Left $ "Type mismatch at " ++ show pos ++ ". Expected " ++ show type_ ++ ", got " ++ show t ++ " - [evalItemWithType]"

-- -- evalIdentWithType :: a -> AbsGramatyka.Ident -> Type -> Env -> Result
-- -- evalIdentWithType pos x type_ env = case Map.lookup x env of
-- --     Nothing -> Left $ "Undeclared variable " ++ show x ++ " at " ++ show pos ++ " - [evalIdentWithType@1]"
-- --     Just t -> if t == type_ then Right Void else Left $ "Type mismatch at " ++ show pos ++ ". Expected " ++ show t ++ ", got " ++ show type_ ++ " - [evalIdentWithType@2]"

-- -- mapType :: AbsGramatyka.Type -> Type
-- -- mapType x = case x of
-- --   AbsGramatyka.Int _ -> Int
-- --   AbsGramatyka.Str _ -> Str
-- --   AbsGramatyka.Bool _ -> Bool

-- evalExpr :: Store -> Env -> Env -> Funcs -> AbsGramatyka.Expr -> (Store, Result)
-- evalExpr store env localEnv funcs x = (Map.empty, failure "evalExpr")

-- -- evalExpr :: Env -> Funcs -> AbsGramatyka.Expr -> Result
-- -- evalExpr env funcs x = case x of
-- --   AbsGramatyka.EVar _ ident -> case Map.lookup ident env of
-- --     Nothing -> Left $ "Undeclared variable " ++ show ident ++ " at " ++ show x ++ " - [evalExpr]"
-- --     Just t -> Right t
-- --   AbsGramatyka.ELitInt _ integer -> Right Int
-- --   AbsGramatyka.ELitTrue _ -> Right Bool
-- --   AbsGramatyka.ELitFalse _ -> Right Bool
-- --   AbsGramatyka.EApp pos ident exprs -> evalApp env funcs pos ident exprs
-- --   AbsGramatyka.EString _ string -> Right Str
-- --   AbsGramatyka.Neg pos expr -> evalExprWithType env funcs pos Int expr
-- --   AbsGramatyka.Not pos expr -> evalExprWithType env funcs pos Bool expr
-- --   AbsGramatyka.EMul pos expr1 mulop expr2 -> evalBinaryIntOp env funcs pos expr1 expr2
-- --   AbsGramatyka.EAdd pos expr1 addop expr2 -> evalBinaryIntOp env funcs pos expr1 expr2
-- --   AbsGramatyka.ERel pos expr1 relop expr2 -> evalBinaryRelOp env funcs pos expr1 expr2
-- --   AbsGramatyka.EAnd pos expr1 expr2 -> evalBinaryBoolOp env funcs pos expr1 expr2
-- --   AbsGramatyka.EOr pos expr1 expr2 -> evalBinaryBoolOp env funcs pos expr1 expr2

-- -- evalApp :: Env -> Funcs -> a -> AbsGramatyka.Ident -> [AbsGramatyka.Expr] -> Result
-- -- evalApp env funcs pos ident exprs = case Map.lookup ident funcs of
-- --   Nothing -> Left $ "Undeclared function " ++ show ident ++ " at " ++ show pos
-- --   Just (type_, args) -> if length args == length exprs then
-- --     case foldResults $ map (\(arg, expr) -> evalExprWithType env funcs pos arg expr) $ zip args exprs of
-- --       Left err -> Left err
-- --       Right _ -> Right type_
-- --   else Left $ "Wrong number of arguments at " ++ show pos ++ ". Expected " ++ show (length args) ++ ", got " ++ show (length exprs) ++ " - [evalApp]"

-- -- evalExprWithType :: Env -> Funcs -> a -> Type -> AbsGramatyka.Expr -> Result
-- -- evalExprWithType env funcs pos type_ expr = case evalExpr env funcs expr of
-- --   Left err -> Left err
-- --   Right t -> if t == type_ then Right t else Left $ "Type mismatch at " ++ show pos ++ ". Expected " ++ show type_ ++ ", got " ++ show t ++ " - [evalExprWithType]"

-- -- evalBinaryIntOp :: Env -> Funcs -> a -> AbsGramatyka.Expr -> AbsGramatyka.Expr -> Result
-- -- evalBinaryIntOp env funcs pos expr1 expr2 = case evalExpr env funcs expr1 of
-- --     Left err -> Left err
-- --     Right t -> if t == Int then
-- --       case evalExpr env funcs expr2 of
-- --         Left err -> Left err
-- --         Right t -> if t == Int then Right Int else Left $ "Type mismatch at " ++ show pos ++ " expected Int, got " ++ show t ++ " - [evalBinaryIntOp@1]"
-- --     else Left $ "Type mismatch at " ++ show pos ++ " expected Int, got " ++ show t ++ " - [evalBinaryIntOp@2]"

-- -- evalBinaryRelOp :: Env -> Funcs -> a -> AbsGramatyka.Expr -> AbsGramatyka.Expr -> Result
-- -- evalBinaryRelOp env funcs pos expr1 expr2 = case evalExpr env funcs expr1 of
-- --     Left err -> Left err
-- --     Right t -> if t == Int then
-- --       case evalExpr env funcs expr2 of
-- --         Left err -> Left err
-- --         Right t -> if t == Int then Right Bool else Left $ "Type mismatch at " ++ show pos ++ " expected Int, got " ++ show t ++ " - [evalBinaryRelOp@1]"
-- --     else Left $ "Type mismatch at " ++ show pos ++ " expected Int, got " ++ show t ++ " - [evalBinaryRelOp@2]"

-- -- evalBinaryBoolOp :: Env -> Funcs -> a -> AbsGramatyka.Expr -> AbsGramatyka.Expr -> Result
-- -- evalBinaryBoolOp env funcs pos expr1 expr2 = case evalExpr env funcs expr1 of
-- --     Left err -> Left err
-- --     Right t -> if t == Bool then
-- --       case evalExpr env funcs expr2 of
-- --         Left err -> Left err
-- --         Right t -> if t == Bool then Right Bool else Left $ "Type mismatch at " ++ show pos ++ " expected Bool, got " ++ show t ++ " - [evalBinaryBoolOp@1]"
-- --     else Left $ "Type mismatch at " ++ show pos ++ " expected Bool, got " ++ show t ++ " - [evalBinaryBoolOp@2]"
