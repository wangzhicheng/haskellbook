module Chapter26.ReaderT where

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
    pure = ReaderT . pure . pure
    ReaderT rma <*> ReaderT rma' = ReaderT $ (<*>) <$> rma <*> rma'

instance Monad m => Monad (ReaderT r m) where
    return = pure
    ReaderT rma >>= f = ReaderT $ go ramb rma
        where
            ramb = flip (runReaderT . f)
            go h g = \r -> g r >>= h r
