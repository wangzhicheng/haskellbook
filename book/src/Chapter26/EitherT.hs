module Chapter26.EitherT where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
    pure = EitherT . pure . pure
    EitherT mea <*> EitherT mea' = EitherT $ (<*>) <$> mea <*> mea'

instance Monad m => Monad (EitherT e m) where
    return = pure
    EitherT mea >>= f = EitherT $
        do
            ea <- mea
            case ea of
              Left e  -> return (Left e)
              Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither (Left e)  = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea

eitherT :: Monad m
        => (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT mab) = do
    ab <- mab
    case ab of
      Left a  -> f a
      Right b -> g b
