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
import Data.ByteString.Char8
import Data.Monoid
import Data.UUID
import Database.Redis

import Network.Wai
import Network.Wai.Handler.Warp

import Servant

import System.Random (randomRIO)

import Types.Stock
import Types.Stock.Psql
import Types.Stock.JSON
import Types.MostRecentTick
import Types.MostRecentTick.Redis
import Types.MostRecentTick.JSON
import Types.Tick

import DB.Redis
import DB.Psql

type API = "stocks" :> Get '[JSON] [Stock]
         :<|> "latestTickerTimestamp" :> QueryParam "stockId" UUID :> Get '[JSON] MostRecentTick
         :<|> "latestTickerTimestamps" :> Get '[JSON] [MostRecentTick]
         :<|> Raw
         
         -- :<|> "tickerQuery" :> QueryParam "q" String :> Get '[JSON] TickerQueryResponse
         -- :<|> "correlated" :> QueryParam "q" String :> QueryParam "limit" Int :> QueryParam "timespan" Int :> Get '[JSON] [TickerQueryResponse]
         -- :<|> "randomInt" :> Get '[PlainText] String

startApp :: IO ()
startApp = run 1234 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

confPath = "conf/servant.conf"

server :: Server API
server = stocksEndpoint
         :<|> latestTickerTimestampEndpoint
         :<|> latestTickerTimestampsEndpoint
         :<|> staticEndpoint


  where
    stocksEndpoint :: Handler [Stock]
    stocksEndpoint = liftIO $ do
      psqlConn <- getPsqlConnection confPath

      stocks <- getStocks psqlConn

      closePsqlConnection psqlConn

      return stocks

    latestTickerTimestampEndpoint :: Maybe UUID -> Handler MostRecentTick
    -- unsafe endpoint!
    latestTickerTimestampEndpoint (Just stockId) = do
      -- TODO improve this!  Don't open a connection for even req
      redisConn <- liftIO $ getRedisConnection confPath

      -- really unsafe!!
      (Right mLatestTimestamp) <- liftIO $ runRedis redisConn (getLatestTimestamp' stockId)
      let (Just latestTimestamp) = mLatestTimestamp
      liftIO $ closeRedisConnection redisConn

      return latestTimestamp


    latestTickerTimestampsEndpoint :: Handler [MostRecentTick]
    latestTickerTimestampsEndpoint = do
      -- TODO improve this!  Don't open a connection for even req
      latestTimestamps <- pure $ do
        redisConn <- getRedisConnection confPath
        lt <- runRedis redisConn getLatestTimestamps
        closeRedisConnection redisConn
        return lt
      liftIO $ latestTimestamps

    staticEndpoint :: Server Raw
    staticEndpoint = serveDirectoryWebApp "stock-frontend"

-- https://hackage.haskell.org/package/servant-server-0.11.0.1/docs/Servant-Server-Internal-Handler.html

-- https://www.stackbuilders.com/tutorials/functional-full-stack/purescript-bridge/
