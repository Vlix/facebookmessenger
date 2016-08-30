module Web.Facebook.Messenger.Types.Requests
    ( SendRequest (..)
    , RequestRecipient (..)
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
data SendRequest =
  SendMessageRequest
    { req_recipient         :: RequestRecipient -- Recipient object
    , req_message           :: RequestMessage   -- Message object
    , req_notification_type :: Maybe NotificationType -- Optional; by default, messages will be REGULAR push notification type
    }
  | SenderActionRequest
    { req_recipient     :: RequestRecipient        -- Recipient object
    , req_sender_action :: SenderActionType -- Message state: TYPING_ON, TYPING_OFF, MARK_SEEN
    }
  deriving (Eq, Show)

data RequestRecipient = RecipientID    { req_recipient_id    :: Text } -- (PS)ID of recipient
                    -- These IDs are page-scoped IDs (PSID). This means that the IDs are unique for a given page.
                      | RecipientPhone { req_recipient_phone :: Text } -- format -> +1(212)555-2368
  deriving (Eq, Show)

-- ------------------------ --
--  SEND MESSAGE INSTANCES  --
-- ------------------------ --

instance ToJSON SendRequest where
    toJSON (SendMessageRequest recipient message notification_type) = object [ "recipient" .= recipient
                                                                             , "message" .= message
                                                                             , "notification_type" .= notification_type
                                                                             ]
    toJSON (SenderActionRequest recipient saction) = object [ "recipient" .= recipient
                                                            , "sender_action" .= saction
                                                            ]

instance ToJSON RequestRecipient where
    toJSON (RecipientID ident)    = object [ "id" .= ident ]
    toJSON (RecipientPhone phone) = object [ "phone" .= phone ]


instance FromJSON SendRequest where
    parseJSON (Object o) = SendMessageRequest <$> o .: "recipient"
                                              <*> o .: "message"
                                              <*> o .:? "notification_type"
                       <|> SenderActionRequest <$> o .: "recipient"
                                               <*> o .: "sender_action"
    parseJSON wat = typeMismatch "SendMessageRequest" wat

instance FromJSON RequestRecipient where
    parseJSON (Object o) = RecipientID    <$> o .: "id"
                       <|> RecipientPhone <$> o .: "phone"
    parseJSON wat = typeMismatch "Recipient" wat
