{-|
Module      : Web.Facebook.Messenger.Types.Requests.Message
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental


-}
module Web.Facebook.Messenger.Types.Requests.Message
  ( RequestMessage (..)
  , textRequest
  , attachmentRequest
  , qr
  , locQR
  , module Web.Facebook.Messenger.Types.Requests.Attachment
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import Web.Facebook.Messenger.Types.Requests.Attachment (RequestAttachment)
import Web.Facebook.Messenger.Types.Static



-- ----------------- --
--  MESSAGE REQUEST  --
-- ----------------- --

data RequestMessage = RMText RequestMessageText
                    | RMAttachment RequestMessageAttachment
  deriving (Eq, Show)

textRequest :: Text -> [RequestQuickReply] -> Maybe Text -> RequestMessage
textRequest txt qrs = RMText . RequestMessageText txt qrs

data RequestMessageText = RequestMessageText
    { rmtText :: Text -- Message text (UTF8 - 320 character limit)
    , rmtQuickReply :: [RequestQuickReply] -- Array of quick_reply to be sent with messages (max 11)
    , rmtMetadata :: Maybe Text -- Has a 1000 character limit
    } deriving (Eq, Show)

attachmentRequest :: RequestAttachment -> [RequestQuickReply] -> Maybe Text -> RequestMessage
attachmentRequest reqAtt qrs = RMAttachment . RequestMessageAttachment reqAtt qrs

data RequestMessageAttachment = RequestMessageAttachment
    { rmaAttachment :: RequestAttachment -- Attachment object
    , rmaQuickReply :: [RequestQuickReply] -- Array of quick_reply to be sent with messages (max 11)
    , rmaMetadata :: Maybe Text -- Has a 1000 character limit
    } deriving (Eq, Show)

qr :: Text -> Text -> Maybe Text -> RequestQuickReply
qr title payload = RQR . RQuickReply title payload

locQR :: Maybe Text -> RequestQuickReply
locQR = RLQR . LocationQuickReply

-- |  Quick Replies can be added to Text, Image and Template message types
data RequestQuickReply = RQR RQuickReply
                       | RLQR LocationQuickReply
  deriving (Eq, Show)

data RQuickReply = RQuickReply
    { rqrTitle :: Text -- Caption of button (20 char limit)
    , rqrPayload :: Text -- Custom data that will be sent back to you via webhook (1000 char limit)
    , rqrImageUrl :: Maybe Text -- URL of image for text quick replies (Image for image_url should be at least 24x24 and will be cropped and resized)
    } deriving (Eq, Show)

data LocationQuickReply = LocationQuickReply { rarImageUrl :: Maybe Text }
  deriving (Eq, Show)


-- ------------------- --
--  MESSAGE INSTANCES  --
-- ------------------- --

instance ToJSON RequestMessage where
  toJSON (RMText x) = toJSON x
  toJSON (RMAttachment x) = toJSON x

instance ToJSON RequestMessageText where
  toJSON (RequestMessageText text qrs metadata) =
      object' [ "text" .=! text
              , mEmptyList "quick_replies" $ Prelude.take 10 qrs
              , "metadata" .=!! metadata
              ]

instance ToJSON RequestMessageAttachment where
  toJSON (RequestMessageAttachment attach qrs metadata) =
      object' [ "attachment" .=! attach
              , mEmptyList "quick_replies" $ Prelude.take 10 qrs
              , "metadata" .=!! metadata
              ]

instance ToJSON RequestQuickReply where
  toJSON (RQR x) = toJSON x
  toJSON (RLQR x) = toJSON x

instance ToJSON RQuickReply where
  toJSON (RQuickReply title payload imageurl) =
      object' [ "content_type" .=! String "text"
              , "title" .=! title
              , "payload" .=! payload
              , "image_url" .=!! imageurl
              ]

instance ToJSON LocationQuickReply where
  toJSON (LocationQuickReply imageurl) =
      object' [ "content_type" .=! String "location"
              , "image_url" .=!! imageurl
              ]
  

instance FromJSON RequestMessage where
  parseJSON = withObject "RequestMessage" $ \o ->
        RMText <$> parseJSON (Object o)
    <|> RMAttachment <$> parseJSON (Object o)

instance FromJSON RequestMessageText where
  parseJSON = withObject "RequestMessageText" $ \o ->
      RequestMessageText <$> o .: "text"
                         <*> o .:? "quick_replies" .!= []
                         <*> o .:? "metadata"

instance FromJSON RequestMessageAttachment where
  parseJSON = withObject "RequestMessageAttachment" $ \o ->
      RequestMessageAttachment <$> o .: "attachment"
                               <*> o .:? "quick_replies" .!= []
                               <*> o .:? "metadata"

instance FromJSON RequestQuickReply where
  parseJSON = withObject "RequestQuickReply" $ \o ->
        RQR <$> parseJSON (Object o)
    <|> RLQR <$> parseJSON (Object o)

instance FromJSON RQuickReply where
  parseJSON = withObject "RQuickReply" $ \o -> do
      typ <- o .: "content_type" :: Parser Text
      unless (typ == "text") $
        fail "RQuickReply: expected \"text\" in \"content_type\" field"
      RQuickReply <$> o .: "title"
                  <*> o .: "payload"
                  <*> o .:? "image_url"
                                   
instance FromJSON LocationQuickReply where
  parseJSON = withObject "LocationQuickReply" $ \o -> do
      typ <- o .: "content_type" :: Parser Text
      unless (typ == "location") $
        fail "LocationQuickReply: expected \"location\" in \"content_type\" field"
      LocationQuickReply <$> o .:? "image_url"
