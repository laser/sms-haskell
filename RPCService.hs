{-# LANGUAGE OverloadedStrings #-}

module RPCService (handleRPC) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson ((.=), (.:), withObject, object, FromJSON(..), ToJSON(..))
import Network.JsonRpc.Server (Parameter(..), RpcResult, Method, (:+:) (..), call, toMethod)

import qualified Data.ByteString.Lazy as BL

data Person = Person { name :: String
                   , age :: Integer } deriving (Show)

instance FromJSON Person where
  parseJSON = withObject "person" $ \o ->
    Person <$> o .: "name" <*> o .: "age"

instance ToJSON Person where
  toJSON p = object [ "name" .= name p, "age" .= age p ]

add :: Monad m => Method m
add = toMethod "add" f (Required "x" :+: Required "y" :+: ())
  where f :: Monad m => Double -> Double -> RpcResult m Double
        f x y = return (x + y)

divide :: Monad m => Method m
divide = toMethod "divide" f (Required "n" :+: Required "d" :+: ())
  where f :: Monad m => Double -> Double -> RpcResult m Double
        f n d = return (n / d)

greet :: Monad m => Method m
greet = toMethod "greet" f (Required "person" :+: ())
  where f :: Monad m => Person -> RpcResult m String
        f p = return ("Hello, you " ++ (show $ age p) ++ " year old person named " ++ (name p))

handleRPC :: Monad m => BL.ByteString -> m (Maybe BL.ByteString)
handleRPC = call [add, divide, greet]