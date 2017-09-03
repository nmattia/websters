{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import Data.IORef
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Utils.StaticFiles (serveDirectory)
import System.Environment (getArgs)
import Types

type MyApi = WebstersAPI :<|> Raw

myAPI :: Proxy MyApi
myAPI = Proxy

main :: IO ()
main = do
  [webappPath] <- getArgs
  users <- newIORef []
  run 8081 (webstersApp webappPath users)

webstersApp :: FilePath -> IORef [Webster] -> Application
webstersApp webappPath users =
    serve (Proxy :: Proxy MyApi) (serveApi :<|> serveWebapp)
  where
    serveApi = webstersGet :<|> webstersPost
    webstersGet = liftIO $ readIORef users
    webstersPost newWebster = liftIO $ do
      modifyIORef users (newWebster:)
      readIORef users
    serveWebapp = serveDirectory webappPath
