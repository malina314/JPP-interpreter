module TypeChecker (checkTypes) where

checkTypes :: Monad m => p -> m (Either a String)
checkTypes _ = do
  return (Right "OK")
