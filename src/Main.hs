{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import           Control.Exception                    (SomeException)
import           Control.Monad.Trans.Except           (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader           (ReaderT, runReaderT)
import           Data.ByteString.Builder              (lazyByteString)
import           Data.String.Conversions              (cs)
import           Network.HTTP.Types                   (Status, status500)
import           Network.Wai                          (Response,
                                                       responseBuilder)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           System.Environment                   (getEnv)
import           Web.Scotty.Trans                     (middleware, scottyT)

import           Config                               (Config (..),
                                                       ServerConfig (..),
                                                       getConfig)
import           Routes                               (routes)
import           Types                                (OAuth2WebFlow (..))

main :: IO ()
main = do
  cfg <- getConfig

  let
    runAction :: ReaderT Config (ExceptT SomeException IO) Response -> IO Response
    runAction et = do
      r <- response et
      case r of
        Left err -> handleError err
        Right r' -> return r'

    response :: ReaderT Config (ExceptT SomeException IO) Response -> IO (Either SomeException Response)
    response = runExceptT . flip runReaderT cfg

    handleError :: SomeException -> IO Response
    handleError ex = return $ msgBuilder (show ex) status500

    msgBuilder :: String -> Status -> Response
    msgBuilder msg s =
      responseBuilder s [("Content-Type","text/plain")] . lazyByteString . cs $ msg

  scottyT (serverPort (serverConfig cfg)) runAction $ do
    middleware logStdoutDev
    routes
