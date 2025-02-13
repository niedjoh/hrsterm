-- |utility type and function providing monad interface for fresh variable ideas
module Utils.FreshMonad (FreshM,fresh,freshVar) where

import Control.Monad.State (State,get,put)

import Term.Type (Var(..))

-- |state monad with 'Int' state
type FreshM = State Int

-- |Returns a fresh integer by incrementing the state by 1.
fresh :: FreshM Int
fresh = do
  i <- get
  put (i+1)
  pure i

-- |Wraps result of 'fresh' into the 'Var' datatype.
freshVar :: FreshM Var
freshVar = Fresh <$> fresh
