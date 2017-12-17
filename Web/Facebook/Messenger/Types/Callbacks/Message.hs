{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.Message
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback will occur when a message has been sent to your page.
You may receive:

* text messages
* messages with attachments (image, audio, video, file, sticker or location)

You may also receive fallback attachments, which are attachments in Messenger other than the ones mentioned above.
A common example is attachments created from link scraping. Messages are always sent in order.

You can subscribe to this callback by selecting @"message"@ when setting up your webhook.

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/message
-}
module Web.Facebook.Messenger.Types.Callbacks.Message (
  -- * Regular Message
  Message (..)
  , MessageId
  , MessageContent (..)
  -- ** Text message
  , MessageText (..)
  , CallbackQuickReply (..)
  -- ** Attachment message
  , MessageAttachment (..)
  , CallbackAttachment (..)
  , MultimediaAttachment (..)
  , CallbackMultimediaPayload (..)
  , TemplateAttachment (..)
  , CallbackTemplate (..)
  -- ** Sticker message
  , MessageSticker (..)
  , StickerAttachment (..)
  , CallbackStickerPayload (..)
  -- ** Location message
  , MessageLocation (..)
  , CallbackLocation (..)
  , CallbackLocationPayload (..)
  , CallbackCoordinates (..)
  -- ** Fallback message
  , MessageFallback (..)
  , CallbackFallback (..)
  )
where


import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import Data.Scientific (scientific)
import Data.Text (Text)

import Web.Facebook.Messenger.Types.Requests.Extra (GenericElement)
import Web.Facebook.Messenger.Types.Static


-- ------------------ --
--  MESSAGE CALLBACK  --
-- ------------------ --

-- | Unique message ID
type MessageId = Text

-- | The user sent message
data Message = Message
    { mId :: MessageId -- ^ Unique message ID
    , mSeq :: Maybe Integer -- ^ Sequence number (deprecated?)
    , mContent :: MessageContent -- ^ Content of the message
    } deriving (Eq, Show)

-- | Content of the message
data MessageContent = MText MessageText -- ^ Text message or Quick Reply callback
                    | MAttachment MessageAttachment -- ^ Multimedia attachment message
                    | MSticker MessageSticker -- ^ Sticker message
                    | MLocation MessageLocation -- ^ Shared location
                    | MFallback MessageFallback -- ^ Other weird messages
  deriving (Eq, Show)

-- | Regular text message sent by a user.
-- If `mtQuickreply` is @Just@ then the user has pressed a Quick Reply.
-- In that case `mtText` is the label of the Quick Reply.
data MessageText = MessageText
    { mtText :: Text -- ^ Text of message (or label of the Quick Reply)
    , mtQuickreply :: Maybe CallbackQuickReply -- ^ Optional custom data provided by the sending app
    } deriving (Eq, Show)

-- | Optional custom data provided by the sending app. This is the payload of the pressed Quick Reply.
newtype CallbackQuickReply =
          CallbackQuickReply { cbQR :: Text }
  deriving (Eq, Show)

-- | Multimedia attachment with a list of attachments
newtype MessageAttachment =
          MessageAttachment { maAttachments :: [CallbackAttachment] }
  deriving (Eq, Show)

-- | Either Multimedia or Template
--
-- /N.B. Template can be sent by a user when they share a template from a different bot\/page with your bot\/page/
data CallbackAttachment = CAMultimedia MultimediaAttachment
                        | CATemplate TemplateAttachment
  deriving (Eq, Show)

-- | Multimedia attachment
data MultimediaAttachment = MultimediaAttachment
    { maType :: AttachmentType -- ^ `IMAGE` \/ `VIDEO` \/ `AUDIO` \/ `FILE`
    , maPayload :: CallbackMultimediaPayload -- ^ URL of the media
    } deriving (Eq, Show)

-- | The URL of the media sent
newtype CallbackMultimediaPayload =
          CallbackMultimediaPayload { cmpUrl :: URL }
  deriving (Eq, Show)

-- | A template sent by a user (rare, but possible)
data TemplateAttachment = TemplateAttachment
    { taTemplateTitle :: Maybe Text -- ^ Title of template
    , taTemplateSubtitle :: Maybe Text -- ^ Subtitle
    , taTemplateUrl :: Maybe URL -- ^ URL
    , taTemplatePayload :: CallbackTemplate -- ^ More of the template
    } deriving (Eq, Show)

-- | Elements of a generic template
data CallbackTemplate = CallbackTemplate
    { ctSharable :: Maybe Bool -- ^ Is sharable or not (maybe not used?)
    , ctElements :: [GenericElement] -- ^ Elements of the template
    } deriving (Eq, Show)

-- | Sticker sent by a user
data MessageSticker = MessageSticker
    { msAttachments :: [StickerAttachment] -- ^ Array containing sticker attachments
    , msStickerId :: Integer -- ^ Sticker ID
    } deriving (Eq, Show)

-- | Sticker Payload
newtype StickerAttachment =
          StickerAttachment { sticker :: CallbackStickerPayload }
  deriving (Eq, Show)

-- | Url and ID of the sticker
data CallbackStickerPayload = CallbackStickerPayload
    { cspStickerUrl :: URL -- ^ URL of the file
    , cspStickerId :: Integer -- ^ Sticker ID
    } deriving (Eq, Show)

-- | Location shared by a user
newtype MessageLocation =
          MessageLocation { mlCoords :: [CallbackLocation] } -- Array containing Location Quick Reply Callback (probably just 1)
  deriving (Eq, Show)

-- | Wrapper for JSON instance convenience
newtype CallbackLocation =
          CallbackLocation { clPayload :: CallbackLocationPayload }
  deriving (Eq, Show)

-- | Location payload
newtype CallbackLocationPayload =
          CallbackLocationPayload { clpCoords :: CallbackCoordinates }
    deriving (Eq, Show)

-- | Coordinates of the location payload
data CallbackCoordinates = CallbackCoordinates
    { ccLat :: Double -- ^ Latitude
    , ccLong :: Double -- ^ Longitude
    } deriving (Eq, Show)

-- | Fallback message. Mainly used for automated scraped links users used in their message.
data MessageFallback = MessageFallback
    { mfText :: Text -- ^ URL sent by the user
    , mfAttachments :: [CallbackFallback]
    } deriving (Eq, Show)

-- | Link scraped data
data CallbackFallback = CallbackFallback
    { cfTitle :: Text -- ^ Title of the URL attachment
    , cfURL :: URL -- ^ URL of the attachment
    } deriving (Eq, Show)


-- ------------------- --
--  MESSAGE INSTANCES  --
-- ------------------- --

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o ->
      Message <$> o .: "mid"
              <*> o .:? "seq"
              <*> parseJSON (Object o)


instance FromJSON MessageContent where
  parseJSON = withObject "MessageContent" $ \o ->
        MFallback <$> parseJSON (Object o)
    <|> MText <$> parseJSON (Object o)
    <|> MSticker <$> parseJSON (Object o)
    <|> MLocation <$> parseJSON (Object o)
    <|> MAttachment <$> parseJSON (Object o)


instance FromJSON MessageFallback where
  parseJSON = withObject "MessageFallback" $ \o ->
      MessageFallback <$> o .: "text"
                      <*> o .: "attachments"

instance FromJSON MessageText where
  parseJSON = withObject "MessageText" $ \o ->
      MessageText <$> o .: "text"
                  <*> o .:? "quick_reply"

instance FromJSON MessageSticker where
  parseJSON = withObject "MessageSticker" $ \o ->
      MessageSticker <$> o .: "attachments"
                     <*> o .: "sticker_id"

instance FromJSON MessageLocation where
  parseJSON = withObject "MessageLocation" $ \o ->
      MessageLocation <$> o .: "attachments"

instance FromJSON MessageAttachment where
  parseJSON = withObject "MessageAttachment" $ \o ->
      MessageAttachment <$> o .: "attachments"


instance FromJSON CallbackQuickReply where
  parseJSON = withObject "CallbackQuickReply" $ \o ->
      CallbackQuickReply <$> o .: "payload"

instance FromJSON StickerAttachment where
  parseJSON = withObject "StickerAttachment" $ \o -> do
      typ <- o .: "type"
      case typ of
        IMAGE -> StickerAttachment <$> o .: "payload"
        _ -> fail "StickerAttachment: no \"image\" type"

instance FromJSON CallbackStickerPayload where
  parseJSON = withObject "CallbackStickerPayload" $ \o ->
      CallbackStickerPayload <$> o .: "url"
                             <*> o .: "sticker_id"

instance FromJSON CallbackAttachment where
  parseJSON = withObject "CallbackAttachment" $ \o -> do
      typ <- o .: "type" :: Parser Text
      case typ of
        "template" -> CATemplate <$> parseJSON (Object o)
        _ -> CAMultimedia <$> parseJSON (Object o)

instance FromJSON TemplateAttachment where
  parseJSON = withObject "TemplateAttachment" $ \o ->
      TemplateAttachment <$> o .: "title"
                         <*> o .:? "subtitle"
                         <*> o .: "url"
                         <*> o .: "payload"

instance FromJSON MultimediaAttachment where
  parseJSON = withObject "MultimediaAttachment" $ \o ->
       MultimediaAttachment <$> o .: "type"
                            <*> o .: "payload"

instance FromJSON CallbackMultimediaPayload where
  parseJSON = withObject "CallbackMultimediaPayload" $ \o ->
      CallbackMultimediaPayload <$> o .: "url"

instance FromJSON CallbackTemplate where
  parseJSON = checkValue
      "CallbackTemplate"
      "template_type"
      ("generic" :: Text)
      $ \o -> CallbackTemplate <$> o .: "sharable"
                               <*> o .:? "elements" .!= []

instance FromJSON CallbackLocation where
  parseJSON = checkValue
      "CallbackLocation"
      "type"
      ("location" :: Text)
      $ \o -> CallbackLocation <$> o .: "payload"

instance FromJSON CallbackLocationPayload where
  parseJSON = withObject "CallbackLocationPayload" $ \o ->
      CallbackLocationPayload <$> o .: "coordinates"

instance FromJSON CallbackCoordinates where
  parseJSON = withObject "CallbackCoordinates" $ \o ->
      CallbackCoordinates <$> o .: "lat"
                          <*> o .: "long"

instance FromJSON CallbackFallback where
  parseJSON = checkValue
      "CallbackFallback"
      "type"
      ("fallback" :: Text)
      $ \o -> CallbackFallback <$> o .: "title"
                               <*> o .: "URL"


instance ToJSON Message where
  toJSON (Message ident mseq content) =
      case toJSON content of
        Object o -> Object $ go mseq $ HM.insert "mid" (String ident) o
        x -> x -- This should never happen. Content should be an object
    where go Nothing = id
          go (Just s) = HM.insert "seq" (Number $ scientific s 0)

instance ToJSON MessageContent where
  toJSON (MText x) = toJSON x
  toJSON (MSticker x) = toJSON x
  toJSON (MAttachment x) = toJSON x
  toJSON (MLocation x) = toJSON x
  toJSON (MFallback x) = toJSON x

instance ToJSON MessageText where
  toJSON (MessageText text qreply) =
      object' [ "text" .=! text
              , "quick_reply" .=!! qreply
              ]

instance ToJSON MessageSticker where
  toJSON (MessageSticker attachments stickerId) =
      object [ "attachments" .= attachments
             , "sticker_id" .= stickerId
             ]

instance ToJSON MessageAttachment where
  toJSON (MessageAttachment attachments) =
      object ["attachments" .= attachments]

instance ToJSON MessageLocation where
  toJSON (MessageLocation coords) =
      object ["attachments" .= coords]

instance ToJSON MessageFallback where
  toJSON (MessageFallback txt fallbacks) =
      object [ "text" .= txt
             , "attachments" .= fallbacks
             ]

instance ToJSON CallbackQuickReply where
  toJSON (CallbackQuickReply payload) =
      object ["payload" .= payload]

instance ToJSON StickerAttachment where
  toJSON (StickerAttachment payload) =
      object [ "type" .= String "image"
             , "payload" .= payload
             ]

instance ToJSON CallbackStickerPayload where
  toJSON (CallbackStickerPayload url stickerId) =
      object [ "url" .= url
             , "sticker_id" .= stickerId
             ]

instance ToJSON CallbackAttachment where
  toJSON (CAMultimedia x) = toJSON x
  toJSON (CATemplate x) = toJSON x

instance ToJSON MultimediaAttachment where
  toJSON (MultimediaAttachment typ payload) =
      object [ "type" .= typ
             , "payload" .= payload
             ]

instance ToJSON TemplateAttachment where
  toJSON (TemplateAttachment title subtitle url payload) =
      object [ "type" .= String "template"
             , "title" .= title
             , "subtitle" .= subtitle
             , "url" .= url
             , "payload" .= payload
             ]

instance ToJSON CallbackMultimediaPayload where
  toJSON (CallbackMultimediaPayload url) =
      object ["url" .= url]

instance ToJSON CallbackTemplate where
  toJSON (CallbackTemplate sharable elements) =
      object [ "template_type" .= String "generic"
             , "sharable" .= sharable
             , "elements" .= elements
             ]

instance ToJSON CallbackLocation where
  toJSON (CallbackLocation payload) =
      object [ "type" .= String "location"
             , "payload" .= payload
             ]

instance ToJSON CallbackFallback where
  toJSON (CallbackFallback title url) =
      object [ "type" .= String "fallback"
             , "payload" .= Null
             , "title" .= title
             , "URL" .= url
             ]

instance ToJSON CallbackLocationPayload where
  toJSON (CallbackLocationPayload coords) =
      object ["coordinates" .= coords]

instance ToJSON CallbackCoordinates where
  toJSON (CallbackCoordinates lat long) =
      object [ "lat" .= lat
             , "long" .= long
             ]
