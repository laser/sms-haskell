{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getEnvironment)
import qualified Web.Scotty as Scotty
import Network.JsonRpc.Server (Parameter(..), RpcResult, (:+:) (..), call, toMethod)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson ((.=), (.:), withObject, object, FromJSON(..), ToJSON(..))

data Person = Person { name :: String
                     , age :: Integer } deriving (Show)

instance FromJSON Person where
  parseJSON = withObject "person" $ \o ->
    Person <$> o .: "name" <*> o .: "age"

instance ToJSON Person where
  toJSON p = object [ "name" .= name p, "age" .= age p ]

add = toMethod "add" f (Required "x" :+: Required "y" :+: ())
  where f :: Monad m => Double -> Double -> RpcResult m Double
        f x y = return (x + y)

divide = toMethod "divide" f (Required "n" :+: Required "d" :+: ())
  where f :: Monad m => Double -> Double -> RpcResult m Double
        f n d = return (n / d)

greet = toMethod "greet" f (Required "person" :+: ())
  where f :: Monad m => Person -> RpcResult m String
        f p = return ("Hello, you " ++ (show $ age p) ++ " year old person named " ++ (name p))

main = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env

  Scotty.scotty port $ do
    Scotty.post "/api" $ do
      b <- Scotty.body
      r <- (call [add, divide, greet] b)
      Scotty.json $ maybe "" id r