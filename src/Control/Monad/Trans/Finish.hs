{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Short-circuit monad transformer.
module Control.Monad.Trans.Finish
  ( FinishT(..)
  , runFinishT'
  , finish
  , Finish
  , runFinish
  , runFinish'
  ) where

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
--   to short-circuit computations with a value of type @f@.
--
-- @
--   'runFinishT' $ do someStuff
--                   'Control.Monad.when' condition $ 'finish' reason
--                   otherwiseContinue
-- @
--
newtype FinishT f μ α = FinishT { runFinishT ∷ μ (Either f α) }

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
instance (Eq f, Eq1 μ) ⇒ Eq1 (FinishT f μ) where
  liftEq f m₁ m₂ = liftEq (liftEq f) (runFinishT m₁) (runFinishT m₂)

instance (Ord f, Ord1 μ) ⇒ Ord1 (FinishT f μ) where
  liftCompare f m₁ m₂ =
    liftCompare (liftCompare f) (runFinishT m₁) (runFinishT m₂)

instance (Show f, Show1 μ) ⇒ Show1 (FinishT f μ) where
  liftShowsPrec sp sl d =
      showsUnaryWith (liftShowsPrec sp' sl') "FinishT" d . runFinishT
    where
      sp' = liftShowsPrec sp sl
      sl' = liftShowList sp sl

instance (Read f, Read1 μ) ⇒ Read1 (FinishT f μ) where
  liftReadsPrec rp rl =
      readsData $ readsUnaryWith (liftReadsPrec rp' rl') "FinishT" FinishT
    where
      rp' = liftReadsPrec rp rl
      rl' = liftReadList rp rl

instance (Eq f, Eq1 μ, Eq α) ⇒ Eq (FinishT f μ α) where
  (==) = eq1

instance (Ord f, Ord1 μ, Ord α) ⇒ Ord (FinishT f μ α) where
  compare = compare1

instance (Show f, Show1 μ, Show α) ⇒ Show (FinishT f μ α) where
  showsPrec = showsPrec1

instance (Read f, Read1 μ, Read α) ⇒ Read (FinishT f μ α) where
  readsPrec = readsPrec1
#endif

instance Pointed μ ⇒ Pointed (FinishT f μ) where
  point = FinishT . point . Right

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

#if MIN_VERSION_base(4,9,0)
instance Fail.MonadFail μ ⇒ Fail.MonadFail (FinishT f μ) where
  fail = FinishT . Fail.fail
#endif

instance MonadFix μ ⇒ MonadFix (FinishT f μ) where
  mfix f = FinishT $ mfix $
    runFinishT . f . either (const $ error "mfix(FinishT): Left") id

#if MIN_VERSION_base(4,4,0)
instance MonadZip μ ⇒ MonadZip (FinishT f μ) where
  mzipWith f m₁ m₂ =
    FinishT $ mzipWith (liftA2 f) (runFinishT m₁) (runFinishT m₂)
#endif

instance MonadIO μ ⇒ MonadIO (FinishT f μ) where
  liftIO = lift . liftIO

instance MonadBase η μ ⇒ MonadBase η (FinishT f μ) where
  liftBase = lift . liftBase

instance BindTrans (FinishT f) where
  liftB = FinishT . fmap Right

instance MonadTrans (FinishT f) where
  lift = FinishT . ap (return Right)

instance MonadTransControl (FinishT f) where
#if MIN_VERSION_monad_control(1,0,0)
  type StT (FinishT f) α = Either f α
  liftWith f = lift $ f $ runFinishT
  restoreT   = FinishT
#else
  newtype StT (FinishT f) α = StFinish { unStFinish ∷ Either f α }
  liftWith f = lift $ f $ liftM StFinish . runFinishT
  restoreT   = FinishT . liftM unStFinish
#endif

instance MonadBaseControl η μ ⇒ MonadBaseControl η (FinishT e μ) where
#if MIN_VERSION_monad_control(1,0,0)
  type StM (FinishT e μ) α = ComposeSt (FinishT e) μ α
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
#else
  newtype StM (FinishT e μ) α =
    StMFinish { unStMFinish ∷ ComposeSt (FinishT e) μ α }
  liftBaseWith = defaultLiftBaseWith StMFinish
  restoreM     = defaultRestoreM unStMFinish
#endif

-- | A version of 'runFinishT' for the cases where you don't need to
--   distinguish between short-circuits and regular computation results.
runFinishT' ∷ Monad μ ⇒ FinishT α μ α → μ α
runFinishT' m = runFinishT m >>= return . either id id

-- | Short-circuit the enclosing computation.
finish ∷ Monad μ ⇒ f → FinishT f μ α
finish = FinishT . return . Left

-- | An alias for 'FinishT' over 'Identity'.
type Finish f α = FinishT f Identity α

-- | 'runFinishT' specialized for 'Finish'.
runFinish ∷ Finish f α → Either f α
runFinish = runIdentity . runFinishT

-- | 'runFinishT'' specialized to 'Finish'.
runFinish' ∷ Finish α α → α
runFinish' = runIdentity . runFinishT'
