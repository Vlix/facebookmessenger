module Web.Facebook.Messenger.Types.Callbacks
    ( FBCallback (..)
    , FBCallbackEntry (..)
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

data FBCallback = FBCallback { fbcb_object :: Text              -- Value will be `page`
                             , fbcb_entry  :: [FBCallbackEntry] -- Array containing event data
                             }

data FBCallbackEntry = FBCallbackEntry
    { fbcb_entry_id        :: Text                  -- Page ID of page
    , fbcb_entry_time      :: Int                   -- Time of update (epoch time in milliseconds)
    , fbcb_entry_messaging :: [FBCallbackMessaging] -- Array containing objects related to messaging
    }
    -- For some reason Facebook gives the id arguments of a postback `entry` object as a `Number`
    | FBCallbackEntryPostback
    { fbcb_entrypostback_id :: Int
    , fbcb_entry_time       :: Int
    , fbcb_entry_messaging  :: [FBCallbackMessaging]
    }


-- ------------------------ --
--    FACEBOOK INSTANCES    --
-- ------------------------ --

instance FromJSON FBCallback where
    parseJSON (Object o) = FBCallback <$> o .: "object"
                                      <*> o .: "entry"
    parseJSON wat = typeMismatch "FBCallback" wat

instance FromJSON FBCallbackEntry where
    parseJSON (Object o) = FBCallbackEntry <$> o .: "id"
                                           <*> o .: "time"
                                           <*> o .: "messaging"
                       <|> FBCallbackEntryPostback <$> o .: "id"
                                                   <*> o .: "time"
                                                   <*> o .: "messaging"
    parseJSON wat = typeMismatch "FBCallbackEntryPostback" wat


instance ToJSON FBCallback where
    toJSON (FBCallback obj entry) = object [ "object" .= obj
                                           , "entry" .= entry
                                           ]

instance ToJSON FBCallbackEntry where
    toJSON (FBCallbackEntry ident time messaging) = object [ "id" .= ident
                                                           , "time" .= time
                                                           , "messaging" .= messaging
                                                           ]
    toJSON (FBCallbackEntryPostback ident time messaging) = object [ "id" .= ident
                                                                   , "time" .= time
                                                                   , "messaging" .= messaging
                                                                   ]
