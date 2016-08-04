module Web.Facebook.Messenger.Types.Requests.Message
    ( FBRequestMessage (..)
    , FBRequestQuickReply (..)
    , module Web.Facebook.Messenger.Types.Requests.Attachment
    ) where

import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)

import Web.Facebook.Messenger.Types.Requests.Attachment


-- ----------------- --
--  MESSAGE REQUEST  --
-- ----------------- --

data FBRequestMessage =
  FBRequestMessageText
    { fbreq_message_text        :: Text -- Message text (UTF8 - 320 character limit)
    , fbreq_message_quick_reply :: Maybe [FBRequestQuickReply] -- Array of quick_reply to be sent with messages (max 10)
    }
  | FBRequestMessageAttachment
    { fbreq_message_attachment  :: FBRequestAttachment   -- Attachment object
    , fbreq_message_quick_reply :: Maybe [FBRequestQuickReply] -- Array of quick_reply to be sent with messages (max 10)
    }
  deriving (Eq, Show)

data FBRequestQuickReply = FBRequestQuickReply
    { fbreq_quick_reply_title   :: Text -- Caption of button (20 char limit)
    , fbreq_quick_reply_payload :: Text -- Custom data that will be sent back to you via webhook (1000 char limit)
    }
  deriving (Eq, Show)


-- ------------------- --
--  MESSAGE INSTANCES  --
-- ------------------- --

instance ToJSON FBRequestMessage where
    toJSON (FBRequestMessageText text qreplies) = object [ "text" .= text
                                                         , "quick_replies" .= qreplies
                                                         ]
    toJSON (FBRequestMessageAttachment attach qreplies) = object [ "attachment" .= attach
                                                                 , "quick_replies" .= qreplies
                                                                 ]

instance ToJSON FBRequestQuickReply where
    toJSON (FBRequestQuickReply title payload) = object [ "content_type" .= String "text"
                                                        , "title" .= title
                                                        , "payload" .= payload ]


instance FromJSON FBRequestMessage where
    parseJSON (Object o) = FBRequestMessageText <$> o .: "text"
                                                <*> o .:? "quick_replies"
                       <|> FBRequestMessageAttachment <$> o .: "attachment"
                                                      <*> o .:? "quick_replies"
    parseJSON wat = typeMismatch "FBRequestMessage" wat

instance FromJSON FBRequestQuickReply where
    parseJSON (Object o) = FBRequestQuickReply <$> o .: "title"
                                               <*> o .: "payload"
    parseJSON wat = typeMismatch "FBRequestQuickReply" wat
