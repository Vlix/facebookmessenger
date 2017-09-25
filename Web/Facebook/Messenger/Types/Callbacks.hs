module Web.Facebook.Messenger.Types.Callbacks
    ( Callback (..)
    , CallbackEntry (..)
    , module Web.Facebook.Messenger.Types.Callbacks.Messaging
    ) where

import Control.Applicative  ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (isJust)
import Data.Text

import Web.Facebook.Messenger.Types.Callbacks.Messaging


-- ============================== --
--       FACEBOOK CALLBACKS       --
-- ============================== --

data Callback = Callback
  { cbObject :: Text -- Value will be `page`
  , cbEntry  :: [CallbackEntry] -- Array containing event data
  } deriving (Eq, Show)

data CallbackEntry =
  CallbackEntry
    { entryId  :: Text
    , entryTime :: Int
    , entryMessaging :: [CallbackMessaging]
    , entryStandby :: Bool }
  deriving (Eq, Show)


-- ------------------------ --
--    FACEBOOK INSTANCES    --
-- ------------------------ --

instance FromJSON Callback where
  parseJSON = withObject "Callback" $ \o ->
      Callback <$> o .: "object"
               <*> o .: "entry"

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
  toJSON (Callback obj entry) =
      object [ "object" .= obj
             , "entry" .= entry
             ]

instance ToJSON CallbackEntry where
  toJSON (CallbackEntry ident time messaging standby) =
      object [ "id" .= ident
             , "time" .= time
             , msging .= messaging
             ]
    where msging = if standby then "standby" else "messaging"
