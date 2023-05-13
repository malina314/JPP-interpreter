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
    (env, newloc) = mapVarsToLocs topdefs Map.empty 0
    funcs = mapFuncsToCode topdefs Map.empty
    funcs' = Map.union funcs (Map.fromList [("printInt", BuiltIn "printInt"), ("printString", BuiltIn "printString"), ("printBool", BuiltIn "printBool")])
    (store, ir) = initVars topdefs Map.empty env funcs
    in ir >>>= evalMain store env funcs' newloc

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

getArgName :: AbsGramatyka.Arg -> Ident
getArgName (AbsGramatyka.Arg _ _ ident) = getIdentName ident

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
  AbsGramatyka.Init pos ident expr -> case snd $ evalExpr store env Map.empty funcs expr of -- todo: to nie obsługuje var args
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

evalMain :: Store -> Env -> Funcs -> Loc -> Result
evalMain store env funcs newloc = failure "evalMain"

evalFunc :: Store -> Env -> Funcs -> Loc -> Ident -> [Value] -> Result
evalFunc store env funcs newloc ident argsValues = case Map.lookup ident funcs of
  Just (Code args block) -> failure "evalFunc" -- todo
  Just (BuiltIn _) -> evalPrint $ head argsValues
  Nothing -> failure "evalFunc" -- this should never happen

evalPrint :: Value -> Result
evalPrint val = case val of
  I x -> Right (I 0, show x)
  S x -> Right (I 0, x)
  B x -> Right (I 0, show x)

-- todo
-- makeLocalEnv :: [AbsGramatyka.Arg] -> [Value] -> Env
-- makeLocalEnv [] [] = Map.empty
-- makeLocalEnv (x:xs) (y:ys) = Map.insert (getArgName x) y $ makeLocalEnv xs ys -- todo: to nie obsługuje var arg

thrd :: (a, b, c) -> c
thrd (_, _, x) = x

discardSnd :: (a, b, c) -> (a, c)
discardSnd (x, _, z) = (x, z)

evalBlock :: Store -> Env -> Env -> Funcs -> Loc -> AbsGramatyka.Block -> (Store, Result)
evalBlock store env loaclEnv funcs newloc x = case x of
  AbsGramatyka.Block _ stmts -> discardSnd $ evalStmts store env loaclEnv funcs newloc stmts

evalStmts :: Store -> Env -> Env -> Funcs -> Loc -> [AbsGramatyka.Stmt] -> (Store, Loc, Result)
evalStmts store _ _ _ newloc [] = (store, newloc, Right (Void, ""))
evalStmts store env localEnv funcs newloc (x:xs) = let
  (store', newloc', res) = evalStmt store env localEnv funcs newloc x
  in res >>+ evalStmts store' env localEnv funcs newloc' xs

(>>>) :: (Store, Loc, Result) -> (Store, Loc, Result) -> (Store, Loc, Result)
(>>>) (store, loc, res) (store', loc', res') = case res of
  Left err -> (store, loc, Left err)
  Right (_, output) -> (store', loc', output >+ res')

(>+) :: Output -> Result -> Result
(>+) output (Left err) = Left err
(>+) output (Right (v, output')) = Right (v, output ++ output')

(>>+) :: Result -> (Store, Loc, Result) -> (Store, Loc, Result)
(>>+) (Left err) _ = (Map.empty, 0, Left err)
(>>+) (Right (v, output)) (store', loc', res') = (store', loc', output >+ res')

(>>+=) :: Result -> ((Value, Output) -> (Store, Loc, Result)) -> (Store, Loc, Result)
(>>+=) (Left err) _ = (Map.empty, 0, Left err)
(>>+=) (Right r) lambda = lambda r

(+>>) :: Output -> (Store, Loc, Result) -> (Store, Loc, Result)
(+>>) output (store', loc', res') = case res' of
  Left err -> (Map.empty, 0, Left err)
  Right (v, output') -> (store', loc', Right (v, output ++ output'))

evalStmt :: Store -> Env -> Env -> Funcs -> Loc -> AbsGramatyka.Stmt -> (Store, Loc, Result)
evalStmt store env localEnv funcs newloc x = case x of
  AbsGramatyka.Empty _ -> (store, newloc, Right (Void, ""))
  AbsGramatyka.BStmt _ block -> let
    (store', res) = evalBlock store env localEnv funcs newloc block
    in (store', newloc, res)
  AbsGramatyka.Decl pos type_ item -> (store, newloc, Right (Void, "")) -- todo
  AbsGramatyka.Ass pos ident expr -> (store, newloc, Right (Void, "")) -- todo
  AbsGramatyka.Ret pos expr -> (store, newloc, Right (Void, "")) -- todo
  AbsGramatyka.Cond pos expr stmt -> let
    (store', res) = evalExpr store env localEnv funcs expr
    in res >>+= \(B b, output) -> if b then output +>> evalStmt store' env localEnv funcs newloc stmt else (store', newloc, Right (Void, output))
  AbsGramatyka.CondElse pos expr stmt1 stmt2 ->(store, newloc, Right (Void, "")) -- todo
  AbsGramatyka.While pos expr stmt ->(store, newloc, Right (Void, "")) -- todo
  AbsGramatyka.SExp _ expr ->(store, newloc, Right (Void, "")) -- todo


-- evalItem :: Env -> Funcs -> AbsGramatyka.Item -> Result
-- evalItem env funcs x = case x of
--   AbsGramatyka.NoInit _ ident -> Right Void
--   AbsGramatyka.Init _ ident expr -> evalExpr env funcs expr

-- evalItemWithType :: Env -> Funcs -> a -> AbsGramatyka.Type -> AbsGramatyka.Item -> Result
-- evalItemWithType env funcs pos type_ item = let
--   t = mapType type_
--   in case evalItem env funcs item of
--   Left err -> Left err
--   Right Void -> Right t
--   Right t' -> if t' == t then Right t else Left $ "Type mismatch at " ++ show pos ++ ". Expected " ++ show type_ ++ ", got " ++ show t ++ " - [evalItemWithType]"

-- evalIdentWithType :: a -> AbsGramatyka.Ident -> Type -> Env -> Result
-- evalIdentWithType pos x type_ env = case Map.lookup x env of
--     Nothing -> Left $ "Undeclared variable " ++ show x ++ " at " ++ show pos ++ " - [evalIdentWithType@1]"
--     Just t -> if t == type_ then Right Void else Left $ "Type mismatch at " ++ show pos ++ ". Expected " ++ show t ++ ", got " ++ show type_ ++ " - [evalIdentWithType@2]"

-- mapType :: AbsGramatyka.Type -> Type
-- mapType x = case x of
--   AbsGramatyka.Int _ -> Int
--   AbsGramatyka.Str _ -> Str
--   AbsGramatyka.Bool _ -> Bool

evalExpr :: Store -> Env -> Env -> Funcs -> AbsGramatyka.Expr -> (Store, Result)
evalExpr store env loaclEnv funcs x = (Map.empty, failure "evalExpr")

-- evalExpr :: Env -> Funcs -> AbsGramatyka.Expr -> Result
-- evalExpr env funcs x = case x of
--   AbsGramatyka.EVar _ ident -> case Map.lookup ident env of
--     Nothing -> Left $ "Undeclared variable " ++ show ident ++ " at " ++ show x ++ " - [evalExpr]"
--     Just t -> Right t
--   AbsGramatyka.ELitInt _ integer -> Right Int
--   AbsGramatyka.ELitTrue _ -> Right Bool
--   AbsGramatyka.ELitFalse _ -> Right Bool
--   AbsGramatyka.EApp pos ident exprs -> evalApp env funcs pos ident exprs
--   AbsGramatyka.EString _ string -> Right Str
--   AbsGramatyka.Neg pos expr -> evalExprWithType env funcs pos Int expr
--   AbsGramatyka.Not pos expr -> evalExprWithType env funcs pos Bool expr
--   AbsGramatyka.EMul pos expr1 mulop expr2 -> evalBinaryIntOp env funcs pos expr1 expr2
--   AbsGramatyka.EAdd pos expr1 addop expr2 -> evalBinaryIntOp env funcs pos expr1 expr2
--   AbsGramatyka.ERel pos expr1 relop expr2 -> evalBinaryRelOp env funcs pos expr1 expr2
--   AbsGramatyka.EAnd pos expr1 expr2 -> evalBinaryBoolOp env funcs pos expr1 expr2
--   AbsGramatyka.EOr pos expr1 expr2 -> evalBinaryBoolOp env funcs pos expr1 expr2

-- evalApp :: Env -> Funcs -> a -> AbsGramatyka.Ident -> [AbsGramatyka.Expr] -> Result
-- evalApp env funcs pos ident exprs = case Map.lookup ident funcs of
--   Nothing -> Left $ "Undeclared function " ++ show ident ++ " at " ++ show pos
--   Just (type_, args) -> if length args == length exprs then
--     case foldResults $ map (\(arg, expr) -> evalExprWithType env funcs pos arg expr) $ zip args exprs of
--       Left err -> Left err
--       Right _ -> Right type_
--   else Left $ "Wrong number of arguments at " ++ show pos ++ ". Expected " ++ show (length args) ++ ", got " ++ show (length exprs) ++ " - [evalApp]"

-- evalExprWithType :: Env -> Funcs -> a -> Type -> AbsGramatyka.Expr -> Result
-- evalExprWithType env funcs pos type_ expr = case evalExpr env funcs expr of
--   Left err -> Left err
--   Right t -> if t == type_ then Right t else Left $ "Type mismatch at " ++ show pos ++ ". Expected " ++ show type_ ++ ", got " ++ show t ++ " - [evalExprWithType]"

-- evalBinaryIntOp :: Env -> Funcs -> a -> AbsGramatyka.Expr -> AbsGramatyka.Expr -> Result
-- evalBinaryIntOp env funcs pos expr1 expr2 = case evalExpr env funcs expr1 of
--     Left err -> Left err
--     Right t -> if t == Int then
--       case evalExpr env funcs expr2 of
--         Left err -> Left err
--         Right t -> if t == Int then Right Int else Left $ "Type mismatch at " ++ show pos ++ " expected Int, got " ++ show t ++ " - [evalBinaryIntOp@1]"
--     else Left $ "Type mismatch at " ++ show pos ++ " expected Int, got " ++ show t ++ " - [evalBinaryIntOp@2]"

-- evalBinaryRelOp :: Env -> Funcs -> a -> AbsGramatyka.Expr -> AbsGramatyka.Expr -> Result
-- evalBinaryRelOp env funcs pos expr1 expr2 = case evalExpr env funcs expr1 of
--     Left err -> Left err
--     Right t -> if t == Int then
--       case evalExpr env funcs expr2 of
--         Left err -> Left err
--         Right t -> if t == Int then Right Bool else Left $ "Type mismatch at " ++ show pos ++ " expected Int, got " ++ show t ++ " - [evalBinaryRelOp@1]"
--     else Left $ "Type mismatch at " ++ show pos ++ " expected Int, got " ++ show t ++ " - [evalBinaryRelOp@2]"

-- evalBinaryBoolOp :: Env -> Funcs -> a -> AbsGramatyka.Expr -> AbsGramatyka.Expr -> Result
-- evalBinaryBoolOp env funcs pos expr1 expr2 = case evalExpr env funcs expr1 of
--     Left err -> Left err
--     Right t -> if t == Bool then
--       case evalExpr env funcs expr2 of
--         Left err -> Left err
--         Right t -> if t == Bool then Right Bool else Left $ "Type mismatch at " ++ show pos ++ " expected Bool, got " ++ show t ++ " - [evalBinaryBoolOp@1]"
--     else Left $ "Type mismatch at " ++ show pos ++ " expected Bool, got " ++ show t ++ " - [evalBinaryBoolOp@2]"
