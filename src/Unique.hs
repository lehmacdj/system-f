{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- provides a monad that can be used for obtaining unique elements of some
-- uniquable set
module Unique
    ( Uniquable(..)
    , MonadUnique(..)
    , UniqueT
    , Unique
    , runUniqueT
    , runUnique
    ) where

import Control.Monad.State
import Data.Functor.Identity (Identity(..))

class Eq u => Uniquable u where
    -- ustream needs to be infinite (or at least near infinite)
    -- and values in it should be unique
    ustream :: [u]

class (Monad m, Uniquable u) => MonadUnique u m where
    fresh :: m u

newtype UniqueT u m a = UniqueT (StateT [u] m a)
    deriving (Functor, Applicative, Monad)

instance (Monad m, Uniquable u) => MonadUnique u (UniqueT u m) where
    fresh = UniqueT $ do
        us <- get
        case us of
          [] -> error "ustream must be near infinite for Unique monad"
          (u:rest) -> do
            put rest
            pure u

type Unique u a = UniqueT u Identity a

runUniqueT :: (Uniquable u, Monad m) => UniqueT u m a -> [u] -> m a
runUniqueT (UniqueT m) used = fst <$> runStateT m freshVars
    where freshVars = filter (`notElem` used) ustream

runUnique :: Uniquable u => Unique u a -> [u] -> a
runUnique (UniqueT m) used = fst . runIdentity $ runStateT m freshVars
    where freshVars = filter (`notElem` used) ustream

instance Uniquable String where
    ustream = varStream

varStream :: [String]
varStream = letters ++ (varStream >>= appendEachLetter)
    where appendEachLetter s = map (s++) letters
          letters = map (:[]) ['a'..'z']
