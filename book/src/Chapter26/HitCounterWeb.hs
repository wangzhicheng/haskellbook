{-# LANGUAGE OverloadedStrings #-}

module Chapter26.HitCounterWeb where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           System.Environment         (getArgs)
import           Web.Scotty.Trans

data Config = Config { counts :: IORef (M.Map Text Integer)
                     , prefix :: Text
                     }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m = undefined

app :: Scotty ()
app =
    get "/:key" $ do
        unprefixed <- param "key"
        let key' = mappend undefined unprefixed
        newInteger <- undefined
        html $
            mconcat [ "<h1>Success! Coun was:"
                    , TL.pack $ show newInteger
                    , "</h1>"
                    ]

main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    let config = undefined
        runR = undefined
    ScottyT 3000 runR app


