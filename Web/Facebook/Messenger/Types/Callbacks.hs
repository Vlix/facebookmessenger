module Web.Facebook.Messenger.Types.Callbacks
    ( Callback (..)
    , CallbackEntry (..)
    , module Web.Facebook.Messenger.Types.Callbacks.Messaging
    ) where

import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)

import Web.Facebook.Messenger.Types.Callbacks.Messaging


-- ============================== --
--       FACEBOOK CALLBACKS       --
-- ============================== --

data Callback = Callback
  { cb_object :: Text            -- Value will be `page`
  , cb_entry  :: [CallbackEntry] -- Array containing event data
  } deriving (Eq, Show)

data CallbackEntry =
  CallbackEntryNumber
    { cb_entrynumber_id  :: Int
    , cb_entry_time      :: Int
    , cb_entry_messaging :: [CallbackMessaging]
    }
    -- For some reason Facebook gives the id arguments of a postback `entry` object as a `Number`
  | CallbackEntry
    { cb_entry_id        :: Text                -- Page ID of page
    , cb_entry_time      :: Int                 -- Time of update (epoch time in milliseconds)
    , cb_entry_messaging :: [CallbackMessaging] -- Array containing objects related to messaging
    }
  deriving (Eq, Show)


-- ------------------------ --
--    FACEBOOK INSTANCES    --
-- ------------------------ --

instance FromJSON Callback where
  parseJSON (Object o) = Callback <$> o .: "object"
                                  <*> o .: "entry"
  parseJSON wat = typeMismatch "Callback" wat

instance FromJSON CallbackEntry where
  parseJSON (Object o) = CallbackEntryNumber <$> o .: "id"
                                             <*> o .: "time"
                                             <*> o .: "messaging"
                     <|> CallbackEntry <$> o .: "id"
                                       <*> o .: "time"
                                       <*> o .: "messaging"
  parseJSON wat = typeMismatch "CallbackEntryNumber" wat


instance ToJSON Callback where
  toJSON (Callback obj entry) = object [ "object" .= obj
                                       , "entry"  .= entry
                                       ]

instance ToJSON CallbackEntry where
  toJSON (CallbackEntry ident time messaging) =
    object [ "id"        .= ident
           , "time"      .= time
           , "messaging" .= messaging
           ]
  toJSON (CallbackEntryNumber ident time messaging) =
    object [ "id"        .= ident
           , "time"      .= time
           , "messaging" .= messaging
           ]
