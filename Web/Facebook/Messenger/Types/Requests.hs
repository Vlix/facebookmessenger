module Web.Facebook.Messenger.Types.Requests
    ( FBSendRequest (..)
    , FBRequestRecipient (..)
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
data FBSendRequest =
  FBSendMessageRequest
    { fbreq_recipient         :: FBRequestRecipient -- Recipient object
    , fbreq_message           :: FBRequestMessage   -- Message object
    , fbreq_notification_type :: Maybe FBRequestNotificationType -- Optional; by default, messages will be REGULAR push notification type
    }
  | FBSenderActionRequest
    { fbreq_recipient     :: FBRequestRecipient        -- Recipient object
    , fbreq_sender_action :: FBRequestSenderActionType -- Message state: TYPING_ON, TYPING_OFF, MARK_SEEN
    }

data FBRequestRecipient = FBRequestRecipientID    { fbreq_recipient_id    :: Text } -- (PS)ID of recipient
                      -- These IDs are page-scoped IDs (PSID). This means that the IDs are unique for a given page.
                        | FBRequestRecipientPhone { fbreq_recipient_phone :: Text } -- format -> +1(212)555-2368

-- ------------------------ --
--  SEND MESSAGE INSTANCES  --
-- ------------------------ --

instance ToJSON FBSendRequest where
    toJSON (FBSendMessageRequest recipient message notification_type) = object [ "recipient" .= recipient
                                                                               , "message" .= message
                                                                               , "notification_type" .= notification_type
                                                                               ]
    toJSON (FBSenderActionRequest recipient saction) = object [ "recipient" .= recipient
                                                              , "sender_action" .= saction
                                                              ]

instance ToJSON FBRequestRecipient where
    toJSON (FBRequestRecipientID ident)    = object [ "id" .= ident ]
    toJSON (FBRequestRecipientPhone phone) = object [ "phone" .= phone ]


instance FromJSON FBSendRequest where
    parseJSON (Object o) = FBSendMessageRequest <$> o .: "recipient"
                                                <*> o .: "message"
                                                <*> o .:? "notification_type"
                       <|> FBSenderActionRequest <$> o .: "recipient"
                                                 <*> o .: "sender_action"
    parseJSON wat = typeMismatch "FBSendMessageRequest" wat

instance FromJSON FBRequestRecipient where
    parseJSON (Object o) = FBRequestRecipientID    <$> o .: "id"
                       <|> FBRequestRecipientPhone <$> o .: "phone"
    parseJSON wat = typeMismatch "FBRequestRecipient" wat
