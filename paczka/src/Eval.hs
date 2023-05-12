module Eval (eval) where

import qualified AbsGramatyka
import qualified Data.Map as Map

type ErrT = String
type OkT = String
type Result = Either ErrT OkT

-- data Type = Int | Str | Bool | Void | Func (Type, [Type])
--   deriving (Eq, Show)

type Loc = Int
data Val = I Int | S String | B Bool
  deriving (Eq, Show)
type Store = Map.Map Loc Val
type Env = Map.Map AbsGramatyka.Ident Loc
type Funcs = Map.Map AbsGramatyka.Ident (Env, AbsGramatyka.Block) -- Env is global env

eval :: AbsGramatyka.Program -> Result
eval _ = Right "OK"

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

-- evalProgram :: Show a => AbsGramatyka.Program' a -> Result
-- evalProgram x = case x of
--    AbsGramatyka.Program _ topdefs -> let
--     env = typeGlobalVars topdefs Map.empty
--     funcs = typeFuncs topdefs
--     funcs' = Map.union funcs (Map.fromList [(AbsGramatyka.Ident "printInt", (Int, [Int])), (AbsGramatyka.Ident "printString", (Int, [Str])), (AbsGramatyka.Ident "printBool", (Bool, [Bool]))]) -- todo: redeklaracje nie są dozwolone
--     in evalMain funcs' >>= \_ -> foldResults $ map (evalTopDef env funcs') topdefs


-- -- evalProgram :: Show a => Store -> Env -> Funcs -> AbsGramatyka.Program' a -> Result
-- evalProgram :: Show a => AbsGramatyka.Program' a -> Result
-- evalProgram x = case x of
--    AbsGramatyka.Program _ topdefs -> let
--     (_, store, env) = typeGlobalVars 0 topdefs
--     funcs = typeFuncs topdefs
--     funcs' = Map.union funcs (Map.fromList [(AbsGramatyka.Ident "printInt", (Int, [Int])), (AbsGramatyka.Ident "printString", (Int, [Str])), (AbsGramatyka.Ident "printBool", (Bool, [Bool]))]) -- todo: redeklaracje nie są dozwolone
--     in foldResults $ map (evalTopDef store env funcs') topdefs

-- typeGlobalVar :: Show a => Int -> AbsGramatyka.TopDef' a -> (Int, Store, Env)
-- typeGlobalVar newloc x = case x of
--   AbsGramatyka.VarDef pos type_ item -> let
--     ident = getIdent item
--     in (newloc + 1, Map.insert newloc (mapType type_) Map.empty, Map.insert ident newloc Map.empty)
--   AbsGramatyka.FnDef _ type_ ident args block -> (newloc, Map.empty, Map.empty)

-- getIdent :: Show a => AbsGramatyka.Item' a -> AbsGramatyka.Ident
-- getIdent x = case x of
--   AbsGramatyka.NoInit _ ident -> ident
--   AbsGramatyka.Init _ ident _ -> ident

-- typeGlobalVars :: Show a => Int -> [AbsGramatyka.TopDef' a] -> (Int, Store, Env)
-- typeGlobalVars newloc [] = (newloc, Map.empty, Map.empty)
-- typeGlobalVars newloc (x:xs) = let
--   (newloc', store, env) = typeGlobalVar newloc x
--   (newloc'', store', env') = typeGlobalVars newloc' xs
--   in (newloc'', Map.union store store', Map.union env env') -- todo: redeklaracje nie są dozwolone

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
