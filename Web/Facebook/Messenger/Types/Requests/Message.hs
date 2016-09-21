module Web.Facebook.Messenger.Types.Requests.Message
    ( RequestMessage (..)
    , RequestQuickReply (..)
    , module Web.Facebook.Messenger.Types.Requests.Attachment
    ) where

import           Control.Applicative  ((<|>))
import           Data.Text
import           Data.Aeson
import           Data.Aeson.Types     (typeMismatch)
import qualified Data.HashMap.Strict  as HM

import           Web.Facebook.Messenger.Types.Requests.Attachment


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

data RequestQuickReply =
  RequestQuickReply
    { req_quick_reply_title     :: Text -- Caption of button (20 char limit)
    , req_quick_reply_payload   :: Text -- Custom data that will be sent back to you via webhook (1000 char limit)
    , req_quick_reply_image_url :: Maybe Text -- URL of image for text quick replies (Image for image_url should be at least 24x24 and will be cropped and resized)
    }
  | LocationQuickReply
    { req_quick_reply_image_url :: Maybe Text }
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
    toJSON (RequestQuickReply title payload imageurl) = object [ "content_type" .= String "text"
                                                               , "title"        .= title
                                                               , "payload"      .= payload
                                                               , "image_url"    .= imageurl
                                                               ]
    toJSON (LocationQuickReply imageurl) = object [ "content_type" .= String "location"
                                                  , "image_url"    .= imageurl
                                                  ]

instance FromJSON RequestMessage where
    parseJSON (Object o) = RequestMessageText <$> o .: "text"
                                              <*> o .:? "quick_replies"
                       <|> RequestMessageAttachment <$> o .: "attachment"
                                                    <*> o .:? "quick_replies"
    parseJSON wat = typeMismatch "RequestMessage" wat

instance FromJSON RequestQuickReply where
    parseJSON (Object o) = case HM.lookup "content_type" o of
        Just "text" -> RequestQuickReply <$> o .: "title"
                                         <*> o .: "payload"
                                         <*> o .:? "image_url"
        Just "location" -> LocationQuickReply <$> o .:? "image_url"
        _ -> fail "QuickReply object expected \"text\" or \"location\" in [content_type] argument"
    parseJSON wat = typeMismatch "RequestQuickReply" wat
