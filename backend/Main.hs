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

main :: IO ()
-- Function body
main = do
  -- read CLI arguments
  [webappPath] <- getArgs

  -- create a reference containing an empty list ([])
  websters <- newIORef []

  -- run our webapp "webstersApp" (see next slide)
  run 8081 (webstersApp websters webappPath)

-- ┌ Function name       Function return type ┐
-- │                                          │
-- │           ┌ Function arguments types     │
-- │           │                  │           │
webstersApp :: IORef [Webster] -> FilePath -> Application
--
--          ┌────────┬ Function arguments
--          │        │                     Serve the assets ┐
webstersApp websters path = --                       ┌──────┴──────────┐
    serve (Proxy :: Proxy MyApi) (serveWebsters :<|> serveDirectory path)
  where
    serveWebsters = webstersGet :<|> webstersPost
    webstersGet = liftIO $ readIORef websters -- GET /websters
    webstersPost newWebster = liftIO $ do -- POST /websters
      modifyIORef websters (newWebster:)
      readIORef websters  --          └ Insert in list
