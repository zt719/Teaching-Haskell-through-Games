{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module AlgEff where

import Control.Monad.State.Class
import Control.Monad.Fail

data Free sig x = Var x | Op (sig (Free sig x))

instance Functor sig => Functor (Free sig) where
  fmap f (Var x) = Var (f x)
  fmap f (Op op) = Op (fmap (fmap f) op)

instance Functor sig => Applicative (Free sig) where
  pure = Var
  Var f <*> Var x = Var (f x)
  Var f <*> Op op = Op (fmap (fmap f) op) 

instance Functor sig => Monad (Free sig) where
  return = pure
  Var x >>= f = f x
  Op op >>= f = Op (fmap (>>= f) op)

fold :: Functor sig => (a -> b) -> (sig b -> b) -> (Free sig a -> b)
fold gen alg (Var x) = gen x
fold gen alg (Op op) = alg (fmap (fold gen alg) op)

data (sigL :+: sigR) a = L (sigL a) | R (sigR a)

instance (Functor sigL, Functor sigR) =>
  Functor (sigL :+: sigR) where
  fmap f (L sigL) = L (fmap f sigL)
  fmap f (R sigL) = R (fmap f sigL)

(\/) :: (sigL b -> b) -> (sigR b -> b) -> (sigL :+: sigR) b -> b
(algL \/ algR) (L op) = algL op
(algL \/ algR) (R op) = algR op

fwd :: Functor sig => sig (Free sig b) -> Free sig b
fwd op = Op op

left :: (Functor sigL, Functor sigR) => Free sigL a -> Free (sigL :+: sigR) a
left (Var x) = Var x
left (Op op) = Op (L (fmap left op))

right :: (Functor sigL, Functor sigR) => Free sigR a -> Free (sigL :+: sigR) a
right (Var x) = Var x
right (Op op) = Op (R (fmap right op))

---------------------------------------------------------------------

data VoidSig k deriving (Functor)

foldV :: Free VoidSig a -> a
foldV (Var a) = a

---------------------------------------------------------------------

data StateSig s k = Get (s -> k) | Put s (() -> k)
  deriving Functor

instance MonadState s (Free (StateSig s)) where
  get = Op (Get return)
  put s = Op (Put s return)

genS :: a -> (s -> a)
genS x = \s -> x

algS :: (StateSig s) (s -> a) -> (s -> a)
algS (Get k) = \s -> k s s
algS (Put s k) = \_ -> k () s

foldS :: Free (StateSig s) a -> s -> a
foldS = fold genS algS

---------------------------------------------------------------------

data FailSig k = FailSig String
  deriving Functor

instance MonadFail (Free FailSig) where
  fail message = Op (FailSig message)

---------------------------------------------------------------------

genF :: a -> Maybe a
genF x = Just x

algF :: FailSig (Maybe a) -> Maybe a
algF (FailSig _) = Nothing

foldF :: Free FailSig a -> Maybe a
foldF = fold genF algF

---------------------------------------------------------------------

genF' :: a -> Either String a
genF' x = Right x

algF' :: FailSig (Either String a) -> Either String a
algF' (FailSig message) = Left message

foldF' :: Free FailSig a -> Either String a
foldF' = fold genF' algF'

---------------------------------------------------------------------

genMS :: Monad m => a -> m (s -> m a)
genMS x = return (\s -> return x)

algMS :: Monad m => StateSig s (m (s -> m a)) -> m (s -> m a)
algMS (Get k) = return (\s -> k s >>= \f -> f s)
algMS (Put s k) = return (\_ -> k () >>= \f -> f s)

foldMS :: Functor sig => Free (StateSig s :+: sig) a -> Free sig (s -> Free sig a)
foldMS = fold genMS (algMS \/ fwd)

finallyMS :: Monad m => s -> m (s -> m a) -> m a
finallyMS s = \p -> p >>= \f -> f s

---------------------------------------------------------------------

genMF :: Monad m => a -> m (Maybe a)
genMF x = return (Just x)

algMF :: Monad m => FailSig (m (Maybe a)) -> m (Maybe a)
algMF (FailSig _) = return Nothing

foldMF :: Functor sig => Free (FailSig :+: sig) a -> Free sig (Maybe a)
foldMF = fold genMF (algMF \/ fwd)

---------------------------------------------------------------------

genMF' :: Monad m => a -> m (Either String a)
genMF' x = return (Right x)

algMF' :: Monad m => FailSig (m (Either String a)) -> m (Either String a)
algMF' (FailSig message) = return (Left message)

foldMF' :: Functor sig => Free (FailSig :+: sig) a -> Free sig (Either String a)
foldMF' = fold genMF' (algMF' \/ fwd)

---------------------------------------------------------------------

class MonadIO m where
  getL :: m String
  putL :: String -> m ()

data IOSig k = PutStrLn String (() -> k) | GetLine (String -> k)
  deriving Functor

instance MonadIO (Free IOSig) where
  getL = Op (GetLine return)
  putL message = Op (PutStrLn message return)

genIO :: a -> IO a
genIO x = return x

algIO :: IOSig (IO a) -> IO a
algIO (PutStrLn message k) = putStrLn message >> k ()
algIO (GetLine k) = getLine >>= k

foldIO :: Free IOSig a -> IO a
foldIO = fold genIO algIO

---------------------------------------------------------------------



instance MonadState s (Free (StateSig s :+: (FailSig :+: IOSig))) where
  get = left get
  put = left . put

instance MonadFail (Free (StateSig Int :+: (FailSig :+: IOSig))) where
  fail = right . left . fail
  
---------------------------------------------------------------------

algS2F :: Functor sig =>
  StateSig Int (Free (FailSig :+: sig) (Int -> Free (FailSig :+: sig) a)) ->
  Free (FailSig :+: sig) (Int -> Free (FailSig :+: sig) a)
algS2F (Get k)   = return (\s -> k s >>= \f -> f s)
algS2F (Put 0 k) = Op (L (FailSig "Cannot put 0"))
algS2F (Put s k) = return (\_ -> k () >>= \f -> f s)

fwdR :: (Functor sigL, Functor sigR) =>
  sigR (Free (sigL :+: sigR) a) -> Free (sigL :+: sigR) a
fwdR op = Op (R op)
 
handleS2F :: Functor sig => Free (StateSig Int :+: sig) a -> Free (FailSig :+: sig) (Int -> Free (FailSig :+: sig) a)
handleS2F = fold genMS (algS2F \/ fwdR)

---------------------------------------------------------------------
  --
foldSF :: Int -> Free (StateSig Int :+: FailSig) Int -> Maybe Int
foldSF s = foldF . finallyMS s . foldMS
