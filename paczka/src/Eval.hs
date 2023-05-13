module Eval (eval) where

import qualified AbsGramatyka
import qualified Data.Map as Map
import qualified Data.List as List

data Value = I Integer | S String | B Bool | Void | Return Value | List [Value] | Ref Loc
  deriving (Eq, Show)

data FuncBody = Code AbsGramatyka.Type [AbsGramatyka.Arg] AbsGramatyka.Block | BuiltIn String
  deriving (Eq, Show)

type ErrorMsg = String
type OutputMsg = String

type Ident = String
type Loc = Int
type Store = Map.Map Loc Value
type Env = Map.Map Ident Loc
type Funcs = Map.Map Ident FuncBody

type Memory = (Store, Env, Env, Funcs, Loc, Value, OutputMsg) -- sotre, env, localEnv, funcs, newloc, return_value, output

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
getArgName (AbsGramatyka.ArgVar _ _ ident) = getIdentName ident

getIdentLoc :: AbsGramatyka.Ident -> Env -> Env -> Loc
getIdentLoc = getVarLoc . getIdentName

getItemLoc :: AbsGramatyka.Item -> Env -> Env -> Loc
getItemLoc = getVarLoc . getItemName

getVarLoc :: Ident -> Env -> Env -> Loc
getVarLoc ident env localEnv = case Map.lookup ident localEnv of
  Just loc -> loc
  Nothing -> case Map.lookup ident env of
    Just loc -> loc
    Nothing -> error $ "Variable " ++ ident ++ " not found!" -- this should never happen

getValue :: Loc -> Store -> Value
getValue loc store = case Map.lookup loc store of
  Just val -> val

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

isVarArg :: AbsGramatyka.Arg -> Bool
isVarArg = not . isArg


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
  AbsGramatyka.FnDef _ type_ ident args block -> Map.insert (getIdentName ident) (Code type_ args block) funcs

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
  (varArgs', varArgsLocs') = unzip $ map (\(a, Ref v) -> (getArgName a, v)) $ filter (\(a, _) -> isVarArg a) $ zip args argsVals
  n = length args'
  store' = Map.union store $ Map.fromList $ zip [newloc..] argsVals'
  localEnv' = Map.fromList (zip args' [newloc..]) `Map.union` Map.fromList (zip varArgs' varArgsLocs')
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
  (_, env, localEnv, funcs, newloc, _, _) = mem
  in case Map.lookup ident funcs of
    Just (Code type_ args block) -> evalBlock (makeLocalEnv mem args argsVals) block >>=
      \(store, _, _, _, _, v, output) -> case v of
        Return val -> Right (store, env, localEnv, funcs, newloc, val, output)
        _ -> Right (store, env, localEnv, funcs, newloc, defaultVal type_, output)
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
  AbsGramatyka.Block _ stmts -> evalStmts (store, env, localEnv, funcs, newloc, v, output) stmts >>=
    \(store', _, _, _, _, v', output') -> Right (store', env, localEnv, funcs, newloc, v', output')

evalStmts :: Memory -> [AbsGramatyka.Stmt] -> Result
evalStmts mem [] = Right mem
evalStmts mem (x:xs) = evalStmt mem x >>= \(store, env, localEnv, funcs, newloc, v, output) -> case v of
  Return _ -> Right (store, env, localEnv, funcs, newloc, v, output)
  _ -> evalStmts (store, env, localEnv, funcs, newloc, v, output) xs

evalStmt :: Memory -> AbsGramatyka.Stmt -> Result
evalStmt mem x = case x of
  AbsGramatyka.Empty _ -> Right mem
  AbsGramatyka.BStmt _ block -> evalBlock mem block
  AbsGramatyka.Decl _ type_ item -> evalItem mem (defaultVal type_) item
  AbsGramatyka.Ass _ ident expr -> evalExpr mem expr >>= \mem' -> let
    (store, env, localEnv, funcs, newloc, v, output) = mem'
    store' = Map.insert (getIdentLoc ident env localEnv) v store
    in Right (store', env, localEnv, funcs, newloc, Void, output)
  AbsGramatyka.Ret _ expr -> evalExpr mem expr >>= \(store, env, localEnv, funcs, newloc, v, output) ->
    Right (store, env, localEnv, funcs, newloc, Return v, output)
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
evalExpr (store, env, localEnv, funcs, newloc, v, output) x = case x of
  AbsGramatyka.EVar _ ident -> Right (store, env, localEnv, funcs, newloc, getValue (getIdentLoc ident env localEnv) store, output)
  AbsGramatyka.ERefVar _ ident -> Right (store, env, localEnv, funcs, newloc, Ref (getIdentLoc ident env localEnv), output)
  AbsGramatyka.ELitInt _ integer -> Right (store, env, localEnv, funcs, newloc, I integer, output)
  AbsGramatyka.ELitTrue _ -> Right (store, env, localEnv, funcs, newloc, B True, output)
  AbsGramatyka.ELitFalse _ -> Right (store, env, localEnv, funcs, newloc, B False, output)
  AbsGramatyka.EApp _ ident exprs -> evalExprs (store, env, localEnv, funcs, newloc, v, output) exprs >>=
    \(store', env', localEnv', funcs', newloc', List argsVals, output') -> let
      funcName = getIdentName ident
      in evalFunc (store', env', localEnv', funcs', newloc', Void, output') funcName argsVals
  AbsGramatyka.EString _ string -> Right (store, env, localEnv, funcs, newloc, S string, output)
  AbsGramatyka.Neg _ expr -> evalExpr (store, env, localEnv, funcs, newloc, v, output) expr >>=
    \(store', env', localEnv', funcs', newloc', I n, output') -> Right (store', env', localEnv', funcs', newloc', I (-n), output')
  AbsGramatyka.Not _ expr -> evalExpr (store, env, localEnv, funcs, newloc, v, output) expr >>=
    \(store', env', localEnv', funcs', newloc', B b, output') -> Right (store', env', localEnv', funcs', newloc', B (not b), output')
  AbsGramatyka.EMul _ expr1 mulop expr2 -> evalExpr (store, env, localEnv, funcs, newloc, v, output) expr1 >>=
    \mem -> let (_, _, _, _, _, I n1, _) = mem in evalExpr mem expr2 >>= \(store', env', localEnv', funcs', newloc', I n2, output') ->
    case mulop of
      AbsGramatyka.Times _ -> Right (store', env', localEnv', funcs', newloc', I (n1 * n2), output')
      AbsGramatyka.Div _ -> Right (store', env', localEnv', funcs', newloc', I (n1 `div` n2), output')
      AbsGramatyka.Mod _ -> Right (store', env', localEnv', funcs', newloc', I (n1 `mod` n2), output')
  AbsGramatyka.EAdd _ expr1 addop expr2 -> evalExpr (store, env, localEnv, funcs, newloc, v, output) expr1 >>=
    \mem -> let (_, _, _, _, _, I n1, _) = mem in evalExpr mem expr2 >>= \(store', env', localEnv', funcs', newloc', I n2, output') ->
    case addop of
      AbsGramatyka.Plus _ -> Right (store', env', localEnv', funcs', newloc', I (n1 + n2), output')
      AbsGramatyka.Minus _ -> Right (store', env', localEnv', funcs', newloc', I (n1 - n2), output')
  AbsGramatyka.ERel _ expr1 relop expr2 -> evalExpr (store, env, localEnv, funcs, newloc, v, output) expr1 >>=
    \mem -> let (_, _, _, _, _, I n1, _) = mem in evalExpr mem expr2 >>= \(store', env', localEnv', funcs', newloc', I n2, output') ->
    case relop of
      AbsGramatyka.LTH _ -> Right (store', env', localEnv', funcs', newloc', B (n1 < n2), output')
      AbsGramatyka.LE _ -> Right (store', env', localEnv', funcs', newloc', B (n1 <= n2), output')
      AbsGramatyka.GTH _ -> Right (store', env', localEnv', funcs', newloc', B (n1 > n2), output')
      AbsGramatyka.GE _ -> Right (store', env', localEnv', funcs', newloc', B (n1 >= n2), output')
      AbsGramatyka.EQU _ -> Right (store', env', localEnv', funcs', newloc', B (n1 == n2), output')
      AbsGramatyka.NE _ -> Right (store', env', localEnv', funcs', newloc', B (n1 /= n2), output')
  AbsGramatyka.EAnd _ expr1 expr2 -> evalExpr (store, env, localEnv, funcs, newloc, v, output) expr1 >>=
    \(store', env', localEnv', funcs', newloc', B b1, output') ->
      if b1 then evalExpr (store', env', localEnv', funcs', newloc', v, output') expr2
      else Right (store', env', localEnv', funcs', newloc', B False, output')
  AbsGramatyka.EOr _ expr1 expr2 -> evalExpr (store, env, localEnv, funcs, newloc, v, output) expr1 >>=
    \(store', env', localEnv', funcs', newloc', B b1, output') ->
      if b1 then Right (store', env', localEnv', funcs', newloc', B True, output')
      else evalExpr (store', env', localEnv', funcs', newloc', v, output') expr2
  
evalExprs :: Memory -> [AbsGramatyka.Expr] -> Result
evalExprs mem [] = let
  (store, env, localEnv, funcs, newloc, _, output) = mem
  in Right (store, env, localEnv, funcs, newloc, List [], output)
evalExprs mem (x:xs) = evalExpr mem x >>= \mem' -> let
  (_, _, _, _, _, v, _) = mem'
  res = evalExprs mem' xs
  in res >>= \(store', env', localEnv', funcs', newloc', List v', output') ->
    Right (store', env', localEnv', funcs', newloc', List (v : v'), output')
