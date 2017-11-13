{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( startApp
    ) where

import Control.Monad.IO.Class (liftIO)

import GHC.Generics

import Data.Aeson
import Data.Aeson.TH
import Data.Time.Clock

import Network.Wai
import Network.Wai.Handler.Warp

import Servant

import System.Random (randomRIO)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data TickerQueryResponse = TickerQueryResponse
  { tickerSymbol :: String
  , description :: String
  } deriving Generic

instance ToJSON TickerQueryResponse

fakeTickerQueryResponse = TickerQueryResponse "foo" "bar"

type API = "time" :> Get '[JSON] UTCTime
         :<|> "tickerQuery" :> QueryParam "q" String :> Get '[JSON] TickerQueryResponse
         :<|> "randomInt" :> Get '[PlainText] String
         :<|> "i" :> Raw

startApp :: IO ()
startApp = run 1234 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = timeEndpoint
  :<|> tickerQueryEndpoint
  :<|> randomIntEndpoint
  :<|> staticEndpoint
  where timeEndpoint :: Handler UTCTime
        timeEndpoint = liftIO getCurrentTime

        tickerQueryEndpoint :: Maybe String -> Handler TickerQueryResponse
        tickerQueryEndpoint (Just query) = pure fakeTickerQueryResponse
        tickerQueryEndpoint Nothing = pure fakeTickerQueryResponse

        randomIntEndpoint :: Handler String
        randomIntEndpoint = liftIO $ show <$> (randomRIO(1,10) :: IO Int)

        staticEndpoint :: Server Raw
        staticEndpoint = serveDirectoryWebApp "stock-frontend"

-- https://hackage.haskell.org/package/servant-server-0.11.0.1/docs/Servant-Server-Internal-Handler.html

-- https://www.stackbuilders.com/tutorials/functional-full-stack/purescript-bridge/
