{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Char8
import Data.Either
import Data.Monoid
import Data.Time.Clock (UTCTime)
import Data.UUID
import Database.Redis
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Random (randomRIO)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map.Lazy as Map

import Types.Stock
import Types.Stock.Psql
import Types.Stock.JSON
import Types.MostRecentTick
import Types.MostRecentTick.Redis
import Types.MostRecentTick.JSON
import Types.Tick

import DB.Redis
import DB.Psql

-- type StockHandler = "stock" :> QueryParam "stockId" UUID :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String] Stock)
type StockHandler = "stock"
                    :> QueryParam "stockId" UUID
                    :> Get '[JSON] Stock

type StocksHandler = "stocks"
                     :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String] [Stock])

type LatestTickerTimestampHandler = "latestTickerTimestamp"
                                    :> QueryParam "stockId" UUID
                                    :> Get '[PlainText] (Headers '[Header "Access-Control-Allow-Origin" String] String)

type LatestTickerTimestampsHandler = "latestTickerTimestamps"
                                     :> Get '[JSON] (Map.Map UUID UTCTime)

type RawHandler = Headers '[Header "Access-Control-Allow-Origin" String] Raw

type API = StockHandler
           :<|> StocksHandler
           :<|> LatestTickerTimestampHandler
           :<|> LatestTickerTimestampsHandler
           :<|> RawHandler

startApp :: Int -> IO ()
startApp port = run port app

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
    -- http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#failing-through-servanterr

    stockEndpoint :: Maybe UUID -> Handler Stock
    stockEndpoint (Just stockId) = do
      stocks <- liftIO $ do
        psqlConn <- getPsqlConnection confPath
        stocks <- getStock stockId psqlConn
        closePsqlConnection psqlConn
        return stocks
        
      case stocks of
        (stock:_) -> return stock
        _ -> throwError err404 { errBody = C.pack ("No stock with ID "<> (show stockId)) }
    stockEndpoint Nothing = throwError err400 { errBody = "Missing stock UUID parameter: \"/stockId?stockId=[UUID]\"" }
    
    -- stocksEndpoint :: Handler [Stock]
    stocksEndpoint = liftIO $ do
      psqlConn <- getPsqlConnection confPath

      stocks <- getStocks psqlConn

      closePsqlConnection psqlConn

      return $ addHeader "http://peterbecich.me" stocks

    -- latestTickerTimestampEndpoint :: Maybe UUID -> Handler String
    latestTickerTimestampEndpoint (Just stockId) = do
      mTimestamp <- liftIO $ do
        redisConn <- getRedisConnection confPath
        eTimestamp <- runRedis redisConn (getLatestTimestamp stockId) :: IO (Either Reply (Maybe UTCTime))
        let
          mTimestamp :: Maybe UTCTime
          mTimestamp = either (\_ -> Nothing) id eTimestamp
        closeRedisConnection redisConn

        return mTimestamp
        
      case mTimestamp of
        (Just timestamp) -> return $ addHeader "http://peterbecich.me" $ show timestamp
        -- Nothing -> throwError $ err404 { errBody = C.pack ("No stock with ID "<> (show stockId)) }
    -- latestTickerTimestampEndpoint Nothing =
    --   addHeader "http://peterbecich.me" <$> throwError $ err400 { errBody = "Missing stock UUID parameter: \"/stockId?stockId=[UUID]\"" }  


    --latestTickerTimestampsEndpoint :: Handler (Map.Map UUID UTCTime)
    latestTickerTimestampsEndpoint = do
      latestTimestamps <- pure $ do
        redisConn <- getRedisConnection confPath
        lt <- runRedis redisConn getLatestTimestamps
        closeRedisConnection redisConn
        return lt
      liftIO $ latestTimestamps

    staticEndpoint :: Server (Headers '[Header "Access-Control-Allow-Origin" String] Raw)
    staticEndpoint = serveDirectoryWebApp "stock-frontend"

-- https://hackage.haskell.org/package/servant-server-0.11.0.1/docs/Servant-Server-Internal-Handler.html

-- https://www.stackbuilders.com/tutorials/functional-full-stack/purescript-bridge/
