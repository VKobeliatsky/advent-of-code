{-# LANGUAGE TypeFamilies #-}

module Common.RefMonad where

import Control.Monad.Reader (ReaderT)
import Control.Monad.ST
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Text.Parsec (ParsecT)

class (Monad monad) => RefMonad monad where
  type Ref monad :: Type -> Type
  newRef :: a -> monad (Ref monad a)
  readRef :: Ref monad a -> monad a
  writeRef :: Ref monad a -> a -> monad ()
  updateRef :: Ref monad a -> (a -> a) -> monad a
  updateRef ref f = do
    x <- readRef ref
    let result = f x
    writeRef ref result
    pure result

instance RefMonad IO where
  type Ref IO = IORef
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

instance RefMonad (ST s) where
  type Ref (ST s) = STRef s
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef

instance (RefMonad m) => RefMonad (StateT s m) where
  type Ref (StateT s m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref val = lift $ writeRef ref val

instance (RefMonad m) => RefMonad (ParsecT i u m) where
  type Ref (ParsecT i u m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref val = lift $ writeRef ref val

instance (RefMonad m) => RefMonad (ReaderT r m) where
  type Ref (ReaderT r m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref val = lift $ writeRef ref val