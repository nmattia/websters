{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Data.Default
import Data.Proxy
import Reflex
import Reflex.Dom
import Servant.API
import Servant.Reflex
import Types

main :: IO ()
main = mainWidget webstersWidget

webstersWidget :: forall t m. (MonadWidget t m, Reflex t) => m ()
webstersWidget = do
    getResponse <- webstersGet =<< button "refresh"

    t <- textInput def

    postResponse <- webstersPost
      ((Right . Webster) <$> _textInput_value t)
      (keypress Enter t)

    websters <- holdDyn [] $ fmapMaybe reqSuccess $ leftmost
      [ getResponse
      , postResponse
      ]

    void $ simpleList websters $ \webster ->
      el "p" $ dynText (name <$> webster)
  where
    webstersGet :<|> webstersPost = client
      (Proxy :: Proxy WebstersAPI)
      (Proxy :: Proxy m)
      (Proxy :: Proxy ())
      (constDyn (BasePath "/"))
