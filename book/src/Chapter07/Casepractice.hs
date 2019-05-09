module Chapter07.CasePractice where

functionC :: Ord a => a -> a -> a
functionC x y =
    case (x > y) of
      True -> x
      _    -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n = case (even n) of
                 True -> n + 2
                 _    -> n

nums :: (Ord a, Num a) => a -> a
nums x = case compare x 0 of
           LT -> -1
           GT -> 1
           EQ -> 0


data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

isBossOf :: Employee -> Employee -> IO ()
isBossOf b s = putStrLn $ show b ++ " is the boss of " ++ show s

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _     = GT
codersRuleCEOsDrool _ Coder     = LT
codersRuleCEOsDrool e e'        = compare e e'

employeeRank :: (Employee -> Employee -> Ordering)
             -> Employee -> Employee -> IO ()
employeeRank comp e e' =
             case comp e e' of
              GT -> isBossOf e e'
              EQ -> putStrLn "Neither employee is the boss"
              LT -> (flip isBossOf)  e e'
