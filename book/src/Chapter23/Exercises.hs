module Chapter23.Exercises where

import           State

get :: Moi s s
get = Moi (\s -> (s, s))

put :: s -> Moi s ()
put s = Moi $ const ((), s)

exec :: Moi s a -> s -> s
exec sa s = snd $ runMoi sa s

eval :: Moi s a -> s -> a
eval sa s = fst $ runMoi sa s

modify :: (s -> s) -> Moi s ()
modify f = Moi (\s -> ((), f s))
