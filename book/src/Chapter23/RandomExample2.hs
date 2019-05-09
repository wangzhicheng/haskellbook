module Chapter23.RandomExample2 where

import           Control.Applicative       (liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           RandomExample
import           System.Random

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThrice :: State StdGen (Die, Die, Die)
rollDieThrice = liftA3 (,,) rollDie' rollDie' rollDie'

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

ndie :: Int -> State StdGen [Die]
ndie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
               in go (sum + die) (count + 1) nextGen
