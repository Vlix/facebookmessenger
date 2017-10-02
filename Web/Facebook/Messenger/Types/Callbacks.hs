{-|
Module      : Web.Facebook.Messenger.Types.Callbacks
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

Callbacks received from Facebook's Messenger API Webhooks
-}
module Web.Facebook.Messenger.Types.Callbacks (
    -- * Callbacks
    Callback (..)
    , CallbackEntry (..)
    -- * Exported Modules
    , module Web.Facebook.Messenger.Types.Callbacks.Messaging
    ) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (isJust)
import Data.Text

import Web.Facebook.Messenger.Types.Callbacks.Messaging


-- ============================== --
--       FACEBOOK CALLBACKS       --
-- ============================== --

newtype Callback = Callback {cbEntries  :: [CallbackEntry]} -- Array containing event data
  deriving (Eq, Show)

data CallbackEntry = CallbackEntry
    { entryId  :: Text
    , entryTime :: Int
    , entryMessaging :: [CallbackMessaging]
    , entryStandby :: Bool
    } deriving (Eq, Show)


-- ------------------------ --
--    FACEBOOK INSTANCES    --
-- ------------------------ --

instance FromJSON Callback where
  parseJSON = withObject "Callback" $ \o -> do
      obj <- o .: "object" :: Parser Text
      unless (obj == "page") $
        fail $ "Callback: \"object\" value not \"page\": " `mappend` unpack obj 
      Callback <$> o .: "entry"

instance FromJSON CallbackEntry where
  parseJSON = withObject "CallbackEntry" $ \o -> do
      mStandby <- o .:? "standby"
      ident <- parseId o
      CallbackEntry <$> pure ident
                    <*> o .: "time"
                    <*> maybe (o .: "messaging") pure mStandby
                    <*> pure (isJust mStandby)
    where parseId o = identNum <|> identTxt
            where identNum = fmap (pack . show) (o .: "id" :: Parser Integer)
                  identTxt = o .: "id" :: Parser Text

instance ToJSON Callback where
  toJSON (Callback entries) =
      object [ "object" .= String "page"
             , "entry" .= entries
             ]

instance ToJSON CallbackEntry where
  toJSON (CallbackEntry ident time messaging standby) =
      object [ "id" .= ident
             , "time" .= time
             , msging .= messaging
             ]
    where msging = if standby then "standby" else "messaging"
