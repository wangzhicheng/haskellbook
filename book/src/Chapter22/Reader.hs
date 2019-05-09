module Chapter22.Reader where

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
    pure a = Reader (const a)
    Reader rab <*> Reader ra = Reader (\r -> rab r (ra r))

instance Monad (Reader r) where
    return a = Reader (const a)
    m >>= f = Reader $ flip (runReader . f ) <*> runReader m

