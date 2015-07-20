{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getEnvironment)
import Web.Scotty (scotty, post, json, body)
import Network.JsonRpc.Server (Parameter(..), RpcResult, (:+:) (..), call, toMethod)

add = toMethod "add" f (Required "x" :+: Required "y" :+: ())
    where f :: Monad m => Double -> Double -> RpcResult m Double
          f x y = return (x + y)

divide = toMethod "divide" f (Required "n" :+: Required "d" :+: ())
    where f :: Monad m => Double -> Double -> RpcResult m Double
          f n d = return (n / d)

main = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env

  scotty port $ do
    post "/api" $ do
      b <- body
      r <- (call [add, divide] b)
      json $ maybe "" id r