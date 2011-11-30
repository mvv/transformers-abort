{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Finish (
    Finish,
    runFinish,
    runFinish',
    FinishT(..),
    runFinishT',
    finish,
  ) where

import Data.Pointed
import Data.Functor.Identity
import Data.Functor.Bind
import Data.Functor.Bind.Trans
import Control.Applicative
import Control.Monad (liftM, ap)
import Control.Monad.Base
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.IO.Class

newtype FinishT f μ α = FinishT { runFinishT ∷ μ (Either f α) }

type Finish f α = FinishT f Identity α

runFinish ∷ Finish f α → Either f α
runFinish = runIdentity . runFinishT

instance Monad μ ⇒ Pointed (FinishT f μ) where
  point = FinishT . return . Right

instance Functor μ ⇒ Functor (FinishT f μ) where
  fmap f = FinishT . fmap (fmap f) . runFinishT

instance (Functor μ, Monad μ) ⇒ Apply (FinishT f μ) where
  (<.>) = ap

instance (Functor μ, Monad μ) ⇒ Applicative (FinishT f μ) where
  pure  = return
  (<*>) = ap

instance (Functor μ, Monad μ) ⇒ Bind (FinishT f μ) where
  (>>-) = (>>=)

instance Monad μ ⇒ Monad (FinishT f μ) where
  return  = FinishT . return . Right
  m >>= f = FinishT $ runFinishT m >>= either (return . Left) (runFinishT . f)
  fail    = FinishT . fail

instance MonadFix μ ⇒ MonadFix (FinishT f μ) where
  mfix f = FinishT $ mfix $
    runFinishT . f . either (error "mfix(FinishT): Left") id

instance MonadIO μ ⇒ MonadIO (FinishT f μ) where
  liftIO = lift . liftIO

instance MonadBase η μ ⇒ MonadBase η (FinishT f μ) where
  liftBase = lift . liftBase

instance BindTrans (FinishT f) where
  liftB = FinishT . fmap Right

instance MonadTrans (FinishT f) where
  lift = FinishT . ap (return Right)

instance MonadTransControl (FinishT f) where
  newtype StT (FinishT f) α = StFinish (Either f α)
  liftControl f = lift $ f $ liftM StFinish . runFinishT
  restoreT (StFinish e) = FinishT $ return e

instance MonadBaseControl η μ ⇒ MonadBaseControl η (FinishT e μ) where
  newtype StM (FinishT e μ) α = StMFinish (ComposeSt (FinishT e) μ α)
  liftBaseControl = liftBaseControlDefault StMFinish
  restoreM (StMFinish stBase) = FinishT $
    restoreM stBase >>= runFinishT . restoreT

runFinishT' ∷ Monad μ ⇒ FinishT α μ α → μ α
runFinishT' m = runFinishT m >>= return . either id id

runFinish' ∷ Finish α α → α
runFinish' = runIdentity . runFinishT'

finish ∷ Monad μ ⇒ f → FinishT f μ α
finish = FinishT . return . Left

