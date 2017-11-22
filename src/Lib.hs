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

import qualified Data.Map.Lazy as Map

import Network.Wai
import Network.Wai.Handler.Warp
import Data.Time.Clock (UTCTime)

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

type API = "stock" :> QueryParam "stockId" UUID :> Get '[JSON] Stock
           :<|> "stocks" :> Get '[JSON] [Stock]
           :<|> "latestTickerTimestamp" :> QueryParam "stockId" UUID :> Get '[PlainText] String
           :<|> "latestTickerTimestamps" :> Get '[JSON] (Map.Map UUID UTCTime)
           :<|> Raw

startApp :: IO ()
startApp = run 1234 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

confPath = "conf/servant.conf"

server :: Server API
server = stockEndpoint
         :<|> stocksEndpoint
         :<|> latestTickerTimestampEndpoint
         :<|> latestTickerTimestampsEndpoint
         :<|> staticEndpoint


  where
    stockEndpoint :: Maybe UUID -> Handler Stock
    -- TODO make safe!!!
    stockEndpoint (Just stockId) = liftIO $ do
      psqlConn <- getPsqlConnection confPath

      stock <- getStock stockId psqlConn

      closePsqlConnection psqlConn

      return (stock !! 0)
    
    stocksEndpoint :: Handler [Stock]
    stocksEndpoint = liftIO $ do
      psqlConn <- getPsqlConnection confPath

      stocks <- getStocks psqlConn

      closePsqlConnection psqlConn

      return stocks

    latestTickerTimestampEndpoint :: Maybe UUID -> Handler String
    -- unsafe endpoint!
    latestTickerTimestampEndpoint (Just stockId) = do
      -- TODO improve this!  Don't open a connection for even req
      redisConn <- liftIO $ getRedisConnection confPath

      -- really unsafe!!
      (Right mLatestTimestamp) <- liftIO $ runRedis redisConn (getLatestTimestamp stockId)
      let (Just latestTimestamp) = mLatestTimestamp
      liftIO $ closeRedisConnection redisConn

      return $ show latestTimestamp


    latestTickerTimestampsEndpoint :: Handler (Map.Map UUID UTCTime)
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
