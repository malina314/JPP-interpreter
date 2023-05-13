module Eval (eval) where

import qualified AbsGramatyka
import qualified Data.Map as Map

data Value = I Int | S String | B Bool | Void
  deriving (Eq, Show)

data FuncBody = Code [AbsGramatyka.Arg] AbsGramatyka.Block | BuiltIn String
  deriving (Eq, Show)

type Err = String
type Output = String
type Ok = (Value, Output)
type Result = Either Err Ok
type InitResult = Either Err Output
type Ident = String
type Loc = Int
type Store = Map.Map Loc Value
type Env = Map.Map Ident Loc
type Funcs = Map.Map Ident FuncBody

eval :: AbsGramatyka.Program -> Result
eval = evalProgram

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

evalProgram :: AbsGramatyka.Program -> Result
evalProgram x = case x of
  AbsGramatyka.Program _ topdefs -> let
    (env, _) = mapVarsToLocs topdefs Map.empty 0
    funcs = mapFuncsToCode topdefs Map.empty
    funcs' = Map.union funcs (Map.fromList [("printInt", BuiltIn "printInt"), ("printString", BuiltIn "printString"), ("printBool", BuiltIn "printBool")])
    (store, ir) = initVars topdefs Map.empty env funcs
    in ir >>>= evalMain store env funcs'

(>>>=) :: InitResult -> Result -> Result
(>>>=) ir r = case ir of
  Left err -> Left err
  Right out -> case r of
    Left err -> Left err
    Right (val, out') -> Right (val, out ++ out')

mapVarsToLocs :: [AbsGramatyka.TopDef] -> Env -> Loc -> (Env, Loc)
mapVarsToLocs [] env newloc = (env, newloc)
mapVarsToLocs (x:xs) env newloc = let
  (env', newloc') = mapVarToLoc x env newloc
  in mapVarsToLocs xs env' newloc'

mapVarToLoc :: AbsGramatyka.TopDef -> Env -> Loc -> (Env, Loc)
mapVarToLoc x env newloc = case x of
  AbsGramatyka.VarDef pos type_ item -> (Map.insert (getItemName item) newloc env, newloc + 1)
  AbsGramatyka.FnDef _ type_ ident args block -> (env, newloc)

getIdent :: AbsGramatyka.Item -> AbsGramatyka.Ident
getIdent x = case x of
  AbsGramatyka.NoInit _ ident -> ident
  AbsGramatyka.Init _ ident _ -> ident

getIdentName :: AbsGramatyka.Ident -> Ident
getIdentName (AbsGramatyka.Ident name) = name

getItemName :: AbsGramatyka.Item -> Ident
getItemName = getIdentName . getIdent

mapFuncsToCode :: [AbsGramatyka.TopDef] -> Funcs -> Funcs
mapFuncsToCode [] funcs = funcs
mapFuncsToCode (x:xs) funcs = mapFuncsToCode xs $ mapFuncToCode x funcs

mapFuncToCode :: AbsGramatyka.TopDef -> Funcs -> Funcs
mapFuncToCode x funcs = case x of
  AbsGramatyka.VarDef pos type_ item -> funcs
  AbsGramatyka.FnDef pos type_ ident args block -> Map.insert (getIdentName ident) (Code args block) funcs

initVars :: [AbsGramatyka.TopDef] -> Store -> Env -> Funcs -> (Store, InitResult)
initVars [] store _ _ = (store, Right "")
initVars (x:xs) store env funcs = let
  (store', ir) = initVar x store env funcs
  (store'', ir') = initVars xs store' env funcs
  in (store'', ir +++ ir')

(+++) :: InitResult -> InitResult -> InitResult
(+++) (Left err) _ = Left err
(+++) _ (Left err) = Left err
(+++) (Right output) (Right output') = Right $ output ++ output'

initVar :: AbsGramatyka.TopDef -> Store -> Env -> Funcs -> (Store, InitResult)
initVar x store env funcs = case x of
  AbsGramatyka.VarDef pos type_ item -> initItem item store env funcs $ defaultVal type_
  AbsGramatyka.FnDef pos type_ ident args block -> (store, Right "")

initItem :: AbsGramatyka.Item -> Store -> Env -> Funcs -> Value -> (Store, InitResult)
initItem item store env funcs default_ = case item of
  AbsGramatyka.NoInit pos ident -> (Map.insert (getItemLoc item env) default_ store, Right "")
  AbsGramatyka.Init pos ident expr -> case evalExpr expr store env funcs of
    Left err -> (store, Left err)
    Right (val, output) -> let
      loc = getItemLoc item env
      in (Map.insert loc val store, Right output)

getItemLoc :: AbsGramatyka.Item -> Env -> Loc
getItemLoc = getVarLoc . getItemName

getVarLoc :: Ident -> Env -> Loc
getVarLoc ident env = case Map.lookup ident env of
  Just loc -> loc

defaultVal :: AbsGramatyka.Type -> Value
defaultVal x = case x of
  AbsGramatyka.Int _ -> I 0
  AbsGramatyka.Str _ -> S ""
  AbsGramatyka.Bool _ -> B False

-- todo: remove if not needed
-- resToInitResult :: Result -> InitResult
-- resToInitResult (Left err) = Left err
-- resToInitResult (Right (val, output)) = Right output

evalMain :: Store -> Env -> Funcs -> Result
evalMain store env funcs = failure "evalMain"
-- evalMain store env funcs = case Map.lookup (AbsGramatyka.Ident "main") env of
--   Nothing -> Left "No main function"
--   Just loc -> evalBlock store env funcs (Code (AbsGramatyka.Block [] []))

evalExpr :: AbsGramatyka.Expr -> Store -> Env -> Funcs -> Result
evalExpr x store env funcs = failure "evalExpr"

-- typeFunc :: Show a => AbsGramatyka.TopDef' a -> Funcs
-- typeFunc x = case x of
--   AbsGramatyka.VarDef pos type_ item -> Map.empty
--   AbsGramatyka.FnDef _ type_ ident args block -> Map.insert ident (getFuncRetType x, getFuncArgsTypes x) Map.empty

-- typeFuncs :: Show a => [AbsGramatyka.TopDef' a] -> Funcs
-- typeFuncs [] = Map.empty
-- typeFuncs (x:xs) = let
--   funcs = typeFunc x
--   funcs' = typeFuncs xs
--   in Map.union funcs funcs' -- todo: redeklaracje nie są dozwolone

-- getFuncRetType :: Show a => AbsGramatyka.TopDef' a -> Type
-- getFuncRetType x = case x of
--   AbsGramatyka.FnDef _ type_ ident args block -> mapType type_

-- getFuncArgsTypes :: Show a => AbsGramatyka.TopDef' a -> [Type]
-- getFuncArgsTypes x = case x of
--   AbsGramatyka.FnDef _ type_ ident args block -> map typeArg args

-- evalTopDef :: Show a => Store -> Env -> Funcs -> AbsGramatyka.TopDef' a -> Result
-- evalTopDef store env funcs x = case x of
--   AbsGramatyka.VarDef pos type_ item -> Right Void
--   AbsGramatyka.FnDef _ type_ ident args block -> 
--     evalBlock block -- todo: wrzucić args do env

-- typeArg :: Show a => AbsGramatyka.Arg' a -> Type
-- typeArg x = case x of
--   AbsGramatyka.Arg _ type_ ident -> mapType type_
--   AbsGramatyka.ArgVar _ type_ ident -> mapType type_

-- evalArg :: Show a => AbsGramatyka.Arg' a -> Result
-- evalArg x = case x of
--   AbsGramatyka.Arg _ type_ ident -> Right $ mapType type_ -- todo: potencjalnie ident trzeba zapisać w stanie
--   AbsGramatyka.ArgVar _ type_ ident -> Right $ mapType type_ -- todo: potencjalnie ident trzeba zapisać w stanie

-- evalBlock :: Show a => AbsGramatyka.Block' a -> Result
-- evalBlock x = case x of
--   AbsGramatyka.Block _ stmts -> foldResults $ map evalStmt stmts

-- foldResults :: [Result] -> Result
-- foldResults [] = Right Void
-- foldResults (x:xs) = case x of
--   Left err -> Left err
--   Right Void -> foldResults xs

-- evalStmt :: Show a => AbsGramatyka.Stmt' a -> Result
-- evalStmt x = case x of
--   AbsGramatyka.Empty _ -> Right Void
--   AbsGramatyka.BStmt _ block -> evalBlock block
--   AbsGramatyka.Decl pos type_ item -> evalItemWithType pos type_ item
--   AbsGramatyka.Ass pos ident expr -> case evalExpr expr of
--     Left err -> Left err
--     Right t -> evalIdentWithType pos ident t
--   AbsGramatyka.Ret _ expr -> failure x -- todo: sprawdzić czy return jest zgodny z typem funckji
--   AbsGramatyka.Cond pos expr stmt -> case evalExpr expr of
--     Left err -> Left err
--     Right t -> if t == Bool then evalStmt stmt else Left $ "Type mismatch at " ++ show pos ++ ". Expected Bool" ++ ", got " ++ show t
--   AbsGramatyka.CondElse pos expr stmt1 stmt2 -> case evalExpr expr of
--     Left err -> Left err
--     Right t -> if t == Bool then foldResults [evalStmt stmt1, evalStmt stmt2] else Left $ "Type mismatch at " ++ show pos ++ ". Expected Bool" ++ ", got " ++ show t
--   AbsGramatyka.While pos expr stmt -> case evalExpr expr of
--     Left err -> Left err
--     Right t -> if t == Bool then evalStmt stmt else Left $ "Type mismatch at " ++ show pos ++ ". Expected Bool" ++ ", got " ++ show t
--   AbsGramatyka.SExp _ expr -> evalExpr expr

-- evalItem :: Show a => AbsGramatyka.Item' a -> Result
-- evalItem x = case x of
--   AbsGramatyka.NoInit _ ident -> Right Void
--   AbsGramatyka.Init _ ident expr -> evalExpr expr

-- evalItemWithType :: Show a => a -> AbsGramatyka.Type' a -> AbsGramatyka.Item' a -> Result
-- evalItemWithType pos type_ item = case evalItem item of
--   Left err -> Left err
--   Right Void -> Right Void
--   Right t -> if t == mapType type_ then Right Void else Left $ "Type mismatch at " ++ show pos ++ ". Expected " ++ show type_ ++ ", got " ++ show t

-- evalIdentWithType :: a -> AbsGramatyka.Ident -> Type -> Result
-- evalIdentWithType pos x type_ = case x of
--   AbsGramatyka.Ident string -> failure x -- todo: odczytać typ ze store

-- mapType :: Show a => AbsGramatyka.Type' a -> Type
-- mapType x = case x of
--   AbsGramatyka.Int _ -> Int
--   AbsGramatyka.Str _ -> Str
--   AbsGramatyka.Bool _ -> Bool

-- evalExpr :: Show a => AbsGramatyka.Expr' a -> Result
-- evalExpr x = case x of
--   AbsGramatyka.EVar _ ident -> failure x -- todo: odczytać ze stanu
--   AbsGramatyka.ELitInt _ integer -> Right Int
--   AbsGramatyka.ELitTrue _ -> Right Bool
--   AbsGramatyka.ELitFalse _ -> Right Bool
--   AbsGramatyka.EApp _ ident exprs -> failure x -- todo: odczytać ze stanu funkcji i sprawdzić typy argumentów
--   AbsGramatyka.EString _ string -> Right Str
--   AbsGramatyka.Neg _ expr -> Right Int
--   AbsGramatyka.Not _ expr -> Right Bool
--   AbsGramatyka.EMul pos expr1 mulop expr2 -> evalBinaryIntOp pos expr1 expr2
--   AbsGramatyka.EAdd pos expr1 addop expr2 -> evalBinaryIntOp pos expr1 expr2
--   AbsGramatyka.ERel pos expr1 relop expr2 -> evalBinaryIntOp pos expr1 expr2
--   AbsGramatyka.EAnd pos expr1 expr2 -> evalBinaryBoolOp pos expr1 expr2
--   AbsGramatyka.EOr pos expr1 expr2 -> evalBinaryBoolOp pos expr1 expr2

-- evalBinaryIntOp :: Show a => a -> AbsGramatyka.Expr' a -> AbsGramatyka.Expr' a -> Result
-- evalBinaryIntOp pos expr1 expr2 = case evalExpr expr1 of
--     Left err -> Left err
--     Right t -> if t == Int then
--       case evalExpr expr2 of
--         Left err -> Left err
--         Right t -> if t == Int then Right Int else Left $ "Type mismatch at " ++ show pos ++ " expected Int"
--     else Left $ "Type mismatch at " ++ show pos ++ " expected Int"

-- evalBinaryBoolOp :: Show a => a -> AbsGramatyka.Expr' a -> AbsGramatyka.Expr' a -> Result
-- evalBinaryBoolOp pos expr1 expr2 = case evalExpr expr1 of
--     Left err -> Left err
--     Right t -> if t == Bool then
--       case evalExpr expr2 of
--         Left err -> Left err
--         Right t -> if t == Bool then Right Bool else Left $ "Type mismatch at " ++ show pos ++ " expected Bool"
--     else Left $ "Type mismatch at " ++ show pos ++ " expected Bool"
