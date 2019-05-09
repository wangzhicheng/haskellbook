module Chapter26.MonadTransInstances where

import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..))
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Control.Monad.Trans.State  (StateT (..))

-- instance MonadTrans (ExceptT e) where
--     lift = ExceptT . liftM Right

-- instance MonadTrans MaybeT where
--     lift = MaybeT . liftM Just

-- instance MonadTrans (ReaderT r) where
--     lift = ReaderT . const

-- instance MonadTrans (StateT s) where
--     lift m = StateT $ \s -> do
--         a <- m
--         return (a, s)

-- newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- instance MonadTrans (EitherT e) where
--     lift = EitherT . liftM Right
