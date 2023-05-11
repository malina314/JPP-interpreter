module TypeChecker (checkTypes) where

import qualified AbsGramatyka

transProgram :: AbsGramatyka.Program -> Either String String
transProgram x = case x of
  AbsGramatyka.Program topdefs -> Right "OK"
  _ -> Left "Invalid type"

-- todo: annotate with type
-- checkTypes :: Monad m => a -> m (Either b String)
checkTypes tree = do
  return (transProgram tree)
