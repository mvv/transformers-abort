{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Abort (
    Abort,
    runAbort,
    runAbort',
    AbortT(..),
    runAbortT',
    abort,
    recover
  ) where

import Data.Functor.Identity
import Data.Functor.Alt
import Data.Functor.Plus
import Data.Functor.Bind
import Data.Functor.Bind.Trans
import Data.Default
import Control.Applicative
import Control.Monad (ap, MonadPlus(..))
import Control.Monad.Base
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Control.Monad.IO.Control
import Control.Failure

newtype AbortT e μ α = AbortT { runAbortT ∷ μ (Either e α) }

type Abort e α = AbortT e Identity α

runAbort ∷ Abort e α → Either e α
runAbort = runIdentity . runAbortT

instance Functor μ ⇒ Functor (AbortT e μ) where
  fmap f = AbortT . fmap (fmap f) . runAbortT

instance (Functor μ, Monad μ) ⇒ Alt (AbortT e μ) where
  m <!> m' = recover m (const m')

instance (Functor μ, Monad μ, Default e) ⇒ Plus (AbortT e μ) where
  zero = mzero

instance (Functor μ, Monad μ) ⇒ Apply (AbortT e μ) where
  (<.>) = ap

instance (Functor μ, Monad μ) ⇒ Applicative (AbortT e μ) where
  pure  = return
  (<*>) = ap

instance (Functor μ, Monad μ, Default e) ⇒ Alternative (AbortT e μ) where
  empty = zero
  (<|>) = (<!>)

instance (Functor μ, Monad μ) ⇒ Bind (AbortT e μ) where
  (>>-) = (>>=)

instance Monad μ ⇒ Monad (AbortT e μ) where
  return  = AbortT . return . Right
  m >>= f = AbortT $ runAbortT m >>= either (return . Left) (runAbortT . f)
  fail    = AbortT . fail

instance (Monad μ, Default e) ⇒ MonadPlus (AbortT e μ) where
  mzero = abort def
  m `mplus` m' = recover m (const m')

instance MonadFix μ ⇒ MonadFix (AbortT e μ) where
  mfix f = AbortT $ mfix $
    runAbortT . f . either (error "mfix(AbortT): Left") id

instance MonadIO μ ⇒ MonadIO (AbortT e μ) where
  liftIO = lift . liftIO

instance MonadBase μ η ⇒ MonadBase (AbortT e μ) η where
  liftBase = lift . liftBase

instance BindTrans (AbortT e) where
  liftB = AbortT . fmap Right

instance MonadTrans (AbortT e) where
  lift = AbortT . ap (return Right)

instance MonadTransControl (AbortT e) where
  liftControl f = lift $ f $ (return . AbortT . return =<<) . runAbortT

instance MonadControlIO μ ⇒ MonadControlIO (AbortT e μ) where
  liftControlIO = liftLiftControlBase liftControlIO

instance Monad μ ⇒ Failure e (AbortT e μ) where
  failure = abort

runAbortT' ∷ Monad μ ⇒ AbortT α μ α → μ α
runAbortT' m = runAbortT m >>= return . either id id

runAbort' ∷ Abort α α → α
runAbort' = runIdentity . runAbortT'

abort ∷ Monad μ ⇒ e → AbortT e μ α
abort = AbortT . return . Left

recover ∷ Monad μ ⇒ AbortT e μ α → (e → AbortT e μ α) → AbortT e μ α
recover m h = AbortT $ runAbortT m >>= either (runAbortT . h) (return . Right)

