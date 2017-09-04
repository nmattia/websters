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
--
--  Attach a button to trigger a GET ─┐
--                        ────────────┴───────────────
    webstersGetTrigger <- webstersGet =<< button "refresh"

    newWebsterInput <- textInput def -- ─────┐
--                                Same with a text input for POST
    webstersPostTrigger <- webstersPost -- ─────┴┐
      ((Right . Webster) <$> _textInput_value newWebsterInput)
      (keypress Enter newWebsterInput)

    websters <- holdDyn [] $ fmapMaybe reqSuccess $ leftmost
      [ webstersGetTrigger --  Define "websters", our list of "Webster"s,
      , webstersPostTrigger -- which is the latest result from either a
      ] --                     GET or a POST

    -- Basically a for-each
    void $ simpleList websters $ \webster ->
      el "p" $ dynText (name <$> webster)
  where
    webstersGet :<|> webstersPost = client -- ┐
      (Proxy :: Proxy WebstersAPI) -- ────────┤
      (Proxy :: Proxy m) --  Function that derives query functions
      (Proxy :: Proxy ()) --       (statically, at compile time)
      (constDyn (BasePath "/"))
