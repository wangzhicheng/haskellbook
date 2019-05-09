module Chapter26.MaybeT where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT mm) = MaybeT $ (fmap . fmap) f mm

instance Applicative m => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure
    MaybeT mm <*> MaybeT mm' = MaybeT $ (<*>) <$> mm <*> mm'

instance Monad m => Monad (MaybeT m) where
    return = pure
    MaybeT mm >>= f = MaybeT $ do
        a <- mm
        case a of
          Nothing -> return Nothing
          Just a' -> (runMaybeT . f) a'
