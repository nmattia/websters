{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Data.Aeson.Types
import Data.Text
import GHC.Generics
import Servant.API

type WebstersAPI =
  -- GET /websters
  -- returns a list of "Webster"s
  "websters" :> Get '[JSON] [Webster] :<|>
  -- POST /websters
  -- inserts a "Webster" and returns a list of "Webster"s
  "websters" :> ReqBody '[JSON] Webster :> Post '[JSON] [Webster]

-- | A "Webster" record datatype (Java's "Class", Go's "Struct")
data Webster = Webster
  { name :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Webster
instance FromJSON Webster
