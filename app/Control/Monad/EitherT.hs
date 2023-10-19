{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.EitherT where

import Control.Monad
import Control.Monad.Writer

newtype EitherT m a b = EitherT (m (Either a b))

runEitherT :: EitherT m a b -> m (Either a b)
runEitherT (EitherT m) = m

instance Monad m => Monad (EitherT m a) where
    return = pure
    (EitherT m) >>= g = EitherT $ m >>= either (pure . Left) (runEitherT . g)

instance Monad m => Applicative (EitherT m a) where
    pure = EitherT . pure . Right
    m1 <*> m2 = m1 >>= \f -> m2 >>= pure . f

instance Monad m => Functor (EitherT m a) where
    fmap f m = m >>= pure . f

instance MonadIO m => MonadIO (EitherT m a) where
    liftIO io = EitherT $ liftIO io >>= pure . Right

instance MonadWriter w m => MonadWriter w (EitherT m a) where
    tell w = EitherT $ tell w >>= pure . Right
    listen (EitherT m) = EitherT $ listen m >>= \(e, w) -> pure $ e >>= Right . (,w)
    pass (EitherT m) = EitherT $ m >>= either (pure . Left) (pass . pure >=> pure . Right)
