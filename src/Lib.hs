{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

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


-- getCurrentTime :: IO UTCTime

-- getTime :: Handler String
-- getTime = do
--   t <- getCurrentTime

--type API = "users" :> Get '[JSON] [User]
type API = "users" :> Get '[JSON] UTCTime
     --    :<|> "currentTime" :> getTime


startApp :: IO ()
startApp = run 1234 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

-- https://hackage.haskell.org/package/servant-server-0.11.0.1/docs/Servant-Server-Internal-Handler.html

-- Handler [User]
--   runHandler :: ExceptT ServantErr (IO a)
server :: Server API
server = do
  -- IO UTCTime -> Handler UTCTime
  time <- liftIO getCurrentTime
  return time

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , User 3 "Stephen" "Hawking"
        ]
