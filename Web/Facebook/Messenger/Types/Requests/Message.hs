module Web.Facebook.Messenger.Types.Requests.Message
    ( RequestMessage (..)
    , RequestQuickReply (..)
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

data RequestMessage =
  RequestMessageText
    { req_message_text        :: Text -- Message text (UTF8 - 320 character limit)
    , req_message_quick_reply :: Maybe [RequestQuickReply] -- Array of quick_reply to be sent with messages (max 10)
    }
  | RequestMessageAttachment
    { req_message_attachment  :: RequestAttachment   -- Attachment object
    , req_message_quick_reply :: Maybe [RequestQuickReply] -- Array of quick_reply to be sent with messages (max 10)
    }
  deriving (Eq, Show)

data RequestQuickReply = RequestQuickReply
    { req_quick_reply_title   :: Text -- Caption of button (20 char limit)
    , req_quick_reply_payload :: Text -- Custom data that will be sent back to you via webhook (1000 char limit)
    }
  deriving (Eq, Show)


-- ------------------- --
--  MESSAGE INSTANCES  --
-- ------------------- --

instance ToJSON RequestMessage where
    toJSON (RequestMessageText text qreplies) = object [ "text" .= text
                                                       , "quick_replies" .= qreplies
                                                       ]
    toJSON (RequestMessageAttachment attach qreplies) = object [ "attachment" .= attach
                                                               , "quick_replies" .= qreplies
                                                               ]

instance ToJSON RequestQuickReply where
    toJSON (RequestQuickReply title payload) = object [ "content_type" .= String "text"
                                                      , "title" .= title
                                                      , "payload" .= payload
                                                      ]


instance FromJSON RequestMessage where
    parseJSON (Object o) = RequestMessageText <$> o .: "text"
                                              <*> o .:? "quick_replies"
                       <|> RequestMessageAttachment <$> o .: "attachment"
                                                    <*> o .:? "quick_replies"
    parseJSON wat = typeMismatch "RequestMessage" wat

instance FromJSON RequestQuickReply where
    parseJSON (Object o) = RequestQuickReply <$> o .: "title"
                                             <*> o .: "payload"
    parseJSON wat = typeMismatch "RequestQuickReply" wat
