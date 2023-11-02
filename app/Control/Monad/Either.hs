{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Either where

import Control.Monad.Reader

newtype EitherT e m a = EitherT (m (Either e a))

runEitherT :: EitherT e m a -> m (Either e a)
runEitherT (EitherT m) = m

instance Monad m => Monad (EitherT e m) where
    return = pure
    (EitherT m) >>= g = EitherT $ m >>= either (pure . Left) (runEitherT . g)

instance Monad m => Applicative (EitherT e m) where
    pure = EitherT . pure . Right
    m1 <*> m2 = m1 >>= \f -> m2 >>= pure . f

instance Monad m => Functor (EitherT e m) where
    fmap f m = m >>= pure . f

instance MonadIO m => MonadIO (EitherT e m) where
    liftIO io = EitherT $ liftIO io >>= pure . Right

instance MonadReader r m => MonadReader r (EitherT e m) where
    ask = EitherT $ Right <$> ask
    local f = EitherT . local f . runEitherT
