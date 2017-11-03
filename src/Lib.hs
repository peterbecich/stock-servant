{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import System.Random (randomRIO)

import Data.Aeson
import Data.Aeson.TH

import Data.Time.Clock

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Control.Monad.IO.Class (liftIO)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "time" :> Get '[JSON] UTCTime
         :<|> "randomInt" :> Get '[PlainText] String
         :<|> "i" :> Raw

type StaticAPI = "i" :> Raw

startApp :: IO ()
startApp = run 1234 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = (liftIO getCurrentTime)
       :<|> (liftIO $ show <$> (randomRIO(1,10) :: IO Int))
       :<|> (serveDirectoryWebApp "stock-frontend/static")

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , User 3 "Stephen" "Hawking"
        ]

-- https://hackage.haskell.org/package/servant-server-0.11.0.1/docs/Servant-Server-Internal-Handler.html

-- https://www.stackbuilders.com/tutorials/functional-full-stack/purescript-bridge/
