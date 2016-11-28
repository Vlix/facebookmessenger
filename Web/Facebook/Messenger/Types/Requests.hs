module Web.Facebook.Messenger.Types.Requests
    ( SendRequest          (..)
    , SenderActionRequest  (..)
    , RequestRecipient     (..)
    , AccountUnlinkRequest (..)
    , module Web.Facebook.Messenger.Types.Requests.Message
    , module Web.Facebook.Messenger.Types.Requests.Settings
    , module Web.Facebook.Messenger.Types.Requests.Airline
    ) where

import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)
import qualified Data.HashMap.Strict        as HM


import Web.Facebook.Messenger.Types.Static
import Web.Facebook.Messenger.Types.Requests.Message
import Web.Facebook.Messenger.Types.Requests.Settings
import Web.Facebook.Messenger.Types.Requests.Airline

-- ============================== --
--      SEND MESSAGE REQUEST      --
-- ============================== --

-- POST request to --->  https://graph.facebook.com/v2.6/me/messages?access_token=<PAGE_ACCESS_TOKEN>
data SendRequest = SendRequest
  { req_recipient         :: RequestRecipient -- Recipient object
  , req_message           :: RequestMessage   -- Message object
  , req_notification_type :: Maybe NotificationType -- Optional; by default, messages will be REGULAR push notification type
  } deriving (Eq, Show)

data SenderActionRequest = SenderActionRequest
  { sar_recipient     :: RequestRecipient -- Recipient object
  , sar_sender_action :: SenderActionType -- Message state: TYPING_ON, TYPING_OFF, MARK_SEEN
  } deriving (Eq, Show)

data AccountUnlinkRequest = AccountUnlinkRequest { accun_psid :: Text }
  deriving (Eq, Show)

data RequestRecipient = RecipientID    { req_recipient_id    :: Text } -- (PS)ID of recipient
-- These IDs are page-scoped IDs (PSID). This means that the IDs are unique for a given page.
                      | RecipientPhone { req_recipient_phone :: Text } -- format -> +1(212)555-2368
                      | RecipientRef   { req_recipient_ref   :: Text }
-- You can call the Send API to start messaging the user using the user_ref field in recipient.
-- Note that this field is the same as the unique user_ref param used in rendering the plugin and used in confirming the opt-in.
  deriving (Eq, Show)

-- ------------------------ --
--  SEND MESSAGE INSTANCES  --
-- ------------------------ --

instance ToJSON SendRequest where
  toJSON (SendRequest recipient message notification_type) =
    object [ "recipient"         .= recipient
           , "message"           .= message
           , "notification_type" .= notification_type
           ]

instance ToJSON SenderActionRequest where
  toJSON (SenderActionRequest recipient saction) =
    object [ "recipient"     .= recipient
           , "sender_action" .= saction
           ]

instance ToJSON AccountUnlinkRequest where
  toJSON (AccountUnlinkRequest psid) = object [ "psid" .= psid ]

instance ToJSON RequestRecipient where
  toJSON (RecipientID ident)     = object [ "id"       .= ident ]
  toJSON (RecipientPhone phone)  = object [ "phone"    .= phone ]
  toJSON (RecipientRef user_ref) = object [ "user_ref" .= user_ref]

instance FromJSON SendRequest where
  parseJSON (Object o) = SendRequest <$> o .: "recipient"
                                     <*> o .: "message"
                                     <*> o .:? "notification_type"
  parseJSON wat = typeMismatch "SendRequest" wat

instance FromJSON SenderActionRequest where
  parseJSON (Object o) = SenderActionRequest <$> o .: "recipient"
                                             <*> o .: "sender_action"
  parseJSON wat = typeMismatch "SenderActionRequest" wat

instance FromJSON AccountUnlinkRequest where
  parseJSON (Object o) = AccountUnlinkRequest <$> o .: "psid"
  parseJSON wat = typeMismatch "AccountUnlinkRequest" wat

instance FromJSON RequestRecipient where
  parseJSON (Object o) = RecipientID    <$> o .: "id"
                     <|> RecipientPhone <$> o .: "phone"
                     <|> RecipientRef   <$> o .: "user_ref"
  parseJSON wat = typeMismatch "Recipient" wat
