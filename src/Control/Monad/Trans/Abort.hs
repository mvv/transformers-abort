{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Abort
  ( Abort
  , runAbort
  , AbortT(..)
  , abort
  , recover
  ) where

import Data.Pointed
import Data.Functor.Identity
import Data.Functor.Alt
import Data.Functor.Plus
import Data.Functor.Bind
import Data.Functor.Bind.Trans
import Data.Default.Class
import Control.Applicative
import Control.Monad (ap, MonadPlus(..))
#if !MIN_VERSION_monad_control(1,0,0)
import Control.Monad (liftM)
#endif
import Control.Monad.Base
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.IO.Class

newtype AbortT e μ α = AbortT { runAbortT ∷ μ (Either e α) }

type Abort e α = AbortT e Identity α

runAbort ∷ Abort e α → Either e α
runAbort = runIdentity . runAbortT

instance Monad μ ⇒ Pointed (AbortT e μ) where
  point = AbortT . return . Right

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

instance MonadBase η μ ⇒ MonadBase η (AbortT e μ) where
  liftBase = lift . liftBase

instance BindTrans (AbortT e) where
  liftB = AbortT . fmap Right

instance MonadTrans (AbortT e) where
  lift = AbortT . ap (return Right)

instance MonadTransControl (AbortT e) where
#if MIN_VERSION_monad_control(1,0,0)
  type StT (AbortT e) α = Either e α
  liftWith f = lift $ f $ runAbortT
  restoreT   = AbortT
#else
  newtype StT (AbortT e) α = StAbort { unStAbort ∷ Either e α }
  liftWith f = lift $ f $ liftM StAbort . runAbortT
  restoreT   = AbortT . liftM unStAbort
#endif

instance MonadBaseControl η μ ⇒ MonadBaseControl η (AbortT e μ) where
#if MIN_VERSION_monad_control(1,0,0)
  type StM (AbortT e μ) α = ComposeSt (AbortT e) μ α
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
#else
  newtype StM (AbortT e μ) α =
    StMAbort { unStMAbort ∷ ComposeSt (AbortT e) μ α }
  liftBaseWith = defaultLiftBaseWith StMAbort
  restoreM     = defaultRestoreM unStMAbort
#endif

abort ∷ Monad μ ⇒ e → AbortT e μ α
abort = AbortT . return . Left

recover ∷ Monad μ ⇒ AbortT e μ α → (e → AbortT e μ α) → AbortT e μ α
recover m h = AbortT $ runAbortT m >>= either (runAbortT . h) (return . Right)
