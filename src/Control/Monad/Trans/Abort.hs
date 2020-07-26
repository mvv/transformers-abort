{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Error monad transformer.
module Control.Monad.Trans.Abort
  ( AbortT(..)
  , abort
  , recover
  , Abort
  , runAbort
  ) where

import Data.Monoid (mempty)
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Data.Pointed
import Data.Functor.Identity
import Data.Functor.Bind
import Data.Functor.Bind.Trans
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
import Data.Functor.Classes
#endif
#if !MIN_VERSION_base(4,9,0)
import Control.Applicative (Applicative(..))
#endif
import Control.Applicative (liftA2)
import Control.Monad (ap)
#if !MIN_VERSION_monad_control(1,0,0)
import Control.Monad (liftM)
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad.Base
import Control.Monad.Fix
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(..))
#endif
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.IO.Class

-- | A monad transformer that extends monad @μ@ with the ability
--   to raise errors of type @e@ and to recover from them.
newtype AbortT e μ α = AbortT { runAbortT ∷ μ (Either e α) }

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
instance (Eq f, Eq1 μ) ⇒ Eq1 (AbortT f μ) where
  liftEq f m₁ m₂ = liftEq (liftEq f) (runAbortT m₁) (runAbortT m₂)

instance (Ord f, Ord1 μ) ⇒ Ord1 (AbortT f μ) where
  liftCompare f m₁ m₂ =
    liftCompare (liftCompare f) (runAbortT m₁) (runAbortT m₂)

instance (Show f, Show1 μ) ⇒ Show1 (AbortT f μ) where
  liftShowsPrec sp sl d =
      showsUnaryWith (liftShowsPrec sp' sl') "AbortT" d . runAbortT
    where
      sp' = liftShowsPrec sp sl
      sl' = liftShowList sp sl

instance (Read f, Read1 μ) ⇒ Read1 (AbortT f μ) where
  liftReadsPrec rp rl =
      readsData $ readsUnaryWith (liftReadsPrec rp' rl') "AbortT" AbortT
    where
      rp' = liftReadsPrec rp rl
      rl' = liftReadList rp rl

instance (Eq f, Eq1 μ, Eq α) ⇒ Eq (AbortT f μ α) where
  (==) = eq1

instance (Ord f, Ord1 μ, Ord α) ⇒ Ord (AbortT f μ α) where
  compare = compare1

instance (Show f, Show1 μ, Show α) ⇒ Show (AbortT f μ α) where
  showsPrec = showsPrec1

instance (Read f, Read1 μ, Read α) ⇒ Read (AbortT f μ α) where
  readsPrec = readsPrec1
#endif

instance Foldable μ ⇒ Foldable (AbortT e μ) where
  foldMap f = foldMap (either (const mempty) f) . runAbortT

instance Traversable μ ⇒ Traversable (AbortT e μ) where
  traverse f = fmap AbortT
             . traverse (either (pure . Left) (fmap Right . f))
             . runAbortT

instance Pointed μ ⇒ Pointed (AbortT e μ) where
  point = AbortT . point . Right

instance Functor μ ⇒ Functor (AbortT e μ) where
  fmap f = AbortT . fmap (fmap f) . runAbortT

instance (Functor μ, Monad μ) ⇒ Apply (AbortT e μ) where
  (<.>) = ap

instance (Functor μ, Monad μ) ⇒ Applicative (AbortT e μ) where
  pure  = return
  (<*>) = ap

instance (Functor μ, Monad μ) ⇒ Bind (AbortT e μ) where
  (>>-) = (>>=)

instance Monad μ ⇒ Monad (AbortT e μ) where
  return  = AbortT . return . Right
  m >>= f = AbortT $ runAbortT m >>= either (return . Left) (runAbortT . f)
#if !MIN_VERSION_base(4,13,0)
  fail    = AbortT . fail
#endif

#if MIN_VERSION_base(4,9,0)
instance Fail.MonadFail μ ⇒ Fail.MonadFail (AbortT f μ) where
  fail = AbortT . Fail.fail
#endif

instance MonadFix μ ⇒ MonadFix (AbortT e μ) where
  mfix f = AbortT $ mfix $
    runAbortT . f . either (error "mfix(AbortT): Left") id

#if MIN_VERSION_base(4,4,0)
instance MonadZip μ ⇒ MonadZip (AbortT f μ) where
  mzipWith f m₁ m₂ =
    AbortT $ mzipWith (liftA2 f) (runAbortT m₁) (runAbortT m₂)
#endif

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

-- | Raise an error.
abort ∷ Monad μ ⇒ e → AbortT e μ α
abort = AbortT . return . Left

-- | Recover from an error.
recover ∷ Monad μ ⇒ AbortT e μ α → (e → AbortT e μ α) → AbortT e μ α
recover m h = AbortT $ runAbortT m >>= either (runAbortT . h) (return . Right)

-- | An alias for 'AbortT' over 'Identity'.
type Abort e α = AbortT e Identity α

-- | 'runAbortT' specialized for 'Abort'.
runAbort ∷ Abort e α → Either e α
runAbort = runIdentity . runAbortT
