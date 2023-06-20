{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MTL where

-- import Control.Monad.Identity

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()
  modify :: (s -> s) -> m ()
  modify f = get >>= put . f

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
  tell :: w -> m ()

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadIO IO where
  liftIO = id

data StateT s m a = StateT { runStateT :: (s -> m (a, s)) }

instance Monad m => Functor (StateT s m) where
  fmap f (StateT s2mas) = StateT $ \s0 -> do 
    (a, s) <- s2mas s0
    return (f a, s)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s0 -> do
    return (a, s0)
  StateT s2mfs <*> StateT s2mas = StateT $ \s0 -> do
    (f, s1) <- s2mfs s0
    (a, s2) <- s2mas s1 
    return (f a, s2)

instance Monad m => Monad (StateT s m) where
  return = pure
  StateT s2mas >>= k = StateT $ \s0 -> do
    (a, s1) <- s2mas s0
    (b, s2) <- runStateT (k a) s1
    return (b, s2)

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance (Monoid w, Monad m) => Functor (WriterT w m) where
  fmap f (WriterT maw) = WriterT $ do
    (a, w) <- maw
    return (f a, w)

instance (Monoid w, Monad m) => Applicative (WriterT w m) where
  pure a = WriterT (return (a, mempty))
  WriterT mfw <*> WriterT maw = WriterT $ do
    (f, w1) <-  mfw
    (a, w2) <-  maw
    return (f a, w1 `mappend` w2)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return = pure
  WriterT maw >>= k = WriterT $ do
    (a, w1) <- maw
    (b, w2) <- runWriterT (k a)
    return (b, w1 `mappend` w2)

instance Monoid w => MonadTrans (WriterT w) where
  lift ma = WriterT $ do
    a <- ma
    return (a, mempty)

instance Monad m => MonadState s (StateT s m) where
  get = StateT $ \s0 -> return (s0, s0)
  put s = StateT $ \s0 -> return ((), s)

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  tell w = WriterT $ return ((), w)

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  get = lift get
  put = lift . put

instance (Monoid w, MonadWriter w m) => MonadWriter w (StateT s m) where
  tell = lift . tell

instance (Monad m, MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO

instance (Monoid w, Monad m, MonadIO m) => MonadIO (WriterT w m) where
  liftIO = lift . liftIO


newtype WST w s m a = WST { runWST :: s -> m (a, s, w) }

instance Monad m => Functor (WST w s m) where
  fmap f (WST wst) = WST (\s -> wst s >>= \(a, s1, w1) -> return (f a, s1, w1))

instance (Monoid w, Monad m) => Applicative (WST w s m) where
  pure a = WST (\s -> return (a, s, mempty))
  WST wstf <*> WST wsta = WST (\s -> wstf s >>= \(f, s1, w1) -> wsta s1 >>= \(a, s2, w2) -> return (f a, s2, w1 `mappend` w2))

instance (Monoid w, Monad m) => Monad (WST w s m) where
  return = pure
  WST wsta >>= k = WST (\s -> wsta s >>= \(a, s1, w1) -> runWST (k a) s1 >>= \(b, s2, w2) -> return (b, s2, w1 `mappend` w2))

instance Monoid w => MonadTrans (WST w s) where
  lift ma = WST (\s -> ma >>= \a -> return (a, s, mempty))

instance (Monoid w, Monad m) => MonadState s (WST w s m) where
  get = WST (\s -> return (s, s, mempty))
  put s' = WST (\s -> return ((), s', mempty))

instance (Monoid w, Monad m) => MonadWriter w (WST w s m) where
  tell w = WST (\s -> return ((), s, w))

instance (Monoid w, Monad m, MonadIO m) => MonadIO (WST w s m) where
  liftIO = lift . liftIO
