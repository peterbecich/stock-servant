-- {-# LANGUAGE DeriveGeneric #-}

-- module FooStock where

-- import Data.Aeson
-- import Data.Aeson.TH
-- import Data.Monoid

-- import GHC.Generics


-- data TickerQueryResponse = TickerQueryResponse
--   { tickerSymbol :: String
--   , description :: String
--   } deriving Generic

-- instance ToJSON TickerQueryResponse

-- fakeTickerQueryResponse = TickerQueryResponse "foo" "bar"

-- fakeStocks name = fmap (\i -> TickerQueryResponse (name ++ show i) ("bar" ++ show i)) [1..]
