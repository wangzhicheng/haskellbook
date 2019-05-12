module Main where

import           Control.Exception
import           System.Environment (getArgs)

willFail :: Integer -> IO (Either ArithException ())
willFail denom = try $ print $ div 5 denom

onlyReportError :: Show e
                => IO (Either e a)
                -> IO ()
onlyReportError action = do
    result <- action
    case result of
      Left e  -> print e
      Right _ -> return ()

testDiv :: String -> IO ()
testDiv d = onlyReportError $ willFail (read d)

main :: IO ()
main = mapM_ testDiv =<< getArgs
