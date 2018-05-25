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
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (isJust)
import Data.Text

import Web.Facebook.Messenger.Internal (checkValue)
import Web.Facebook.Messenger.Types.Callbacks.Messaging
import Web.Facebook.Messenger.Types.Static (PageID (..))


-- ============================== --
--       FACEBOOK CALLBACKS       --
-- ============================== --

-- | Top callback object to be returned from Facebook's Webhooks.
--
-- Be sure to iterate over @[`CallbackEntry`]@ to process all events.
newtype Callback = Callback {cbEntries :: [CallbackEntry]}
  deriving (Eq, Show, Read)

-- | A callback event. Although @[`CallbackMessaging`]@ is also a list, there is no batching mechanism for @[`CallbackMessaging`]@ at the moment.
data CallbackEntry = CallbackEntry
    { entryId  :: PageID -- ^ Page ID of the page
    , entryTime :: Integer -- ^ Time of update (epoch time in milliseconds)
    , entryMessaging :: CallbackMessaging
    -- ^ Facebook sends an array of one messaging object.
    -- (Even though this is a list, it will only contain one messaging object at the moment,
    -- hence just the single data type in @entryMessaging@)
    , entryStandby :: Bool -- ^ Whether this `CallbackEntry` is a standby event, meaning the receiving bot is a `SecondaryReceiver`
    } deriving (Eq, Show, Read)


-- ------------------------ --
--    FACEBOOK INSTANCES    --
-- ------------------------ --

instance FromJSON Callback where
  parseJSON = checkValue
      "Callback"
      "object"
      ("page" :: Text)
      $ \o -> Callback <$> o .: "entry"

instance FromJSON CallbackEntry where
  parseJSON = withObject "CallbackEntry" $ \o -> do
      mStandby <- o .:? "standby"
      msgArray <- maybe (o .: "messaging") pure mStandby
      case msgArray of
        [msg] -> do
            ident <- parseId o
            CallbackEntry <$> pure (PageID ident)
                          <*> o .: "time"
                          <*> pure msg
                          <*> pure (isJust mStandby)
        _ -> fail "not just one messaging object"
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
             , msging .= [messaging]
             ]
    where msging = if standby then "standby" else "messaging"
