module Web.Facebook.Messenger.Types.Callbacks.Message
  ( MessageId
  , Message (..)
  , MessageContent (..)
  , MessageText (..)
  , MessageAttachment (..)
  , MessageSticker (..)
  , MessageLocation (..)
  , MessageFallback (..)
  , CallbackQuickReply (..)
  , StickerAttachment (..)
  , CallbackStickerPayload (..)
  , CallbackAttachment (..)
  , MultimediaAttachment (..)
  , TemplateAttachment (..)
  , CallbackMultimediaPayload (..)
  , CallbackTemplate (..)
  , CallbackLocation (..)
  , CallbackLocationPayload (..)
  , CallbackCoordinates (..)
  , CallbackFallback (..)
  )
where


import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import Data.Scientific (scientific)
import Data.Text (Text, unpack)

import Web.Facebook.Messenger.Types.Requests.Extra (GenericElement)
import Web.Facebook.Messenger.Types.Static


-- ------------------ --
--  MESSAGE CALLBACK  --
-- ------------------ --

type MessageId = Text

data Message = Message
    { mId :: MessageId
    , mSeq :: Maybe Integer
    , mContent :: MessageContent
    } deriving (Eq, Show)

data MessageContent = MText MessageText
                    | MAttachment MessageAttachment
                    | MSticker MessageSticker
                    | MLocation MessageLocation
                    | MFallback MessageFallback
  deriving (Eq, Show)
    

data MessageText = MessageText
    { mtText :: Text -- Text of message
    , mtQuickreply :: Maybe CallbackQuickReply -- Optional custom data provided by the sending app
    } deriving (Eq, Show)

newtype MessageAttachment =
          MessageAttachment { maAttachments :: [CallbackAttachment] } -- Array containing attachment data
  deriving (Eq, Show)

data MessageSticker = MessageSticker
    { msAttachments :: [StickerAttachment] -- Array containing attachment data
    , msStickerId  :: Integer -- Sticker ID
    } deriving (Eq, Show)

newtype MessageLocation =
          MessageLocation { mlCoords :: [CallbackLocation] } -- Array containing Location Quick Reply Callback (probably just 1)
  deriving (Eq, Show)

data MessageFallback = MessageFallback
    { mfText :: Text
    , mfAttachments :: [CallbackFallback] -- URL scraped messages
    } deriving (Eq, Show)


newtype CallbackQuickReply =
          CallbackQuickReply { cbQR :: Text }
  deriving (Eq, Show)

newtype StickerAttachment =
          StickerAttachment { sticker :: CallbackStickerPayload }
  deriving (Eq, Show)

data CallbackStickerPayload = CallbackStickerPayload
    { cspStickerUrl :: Text -- URL of the file
    , cspStickerId :: Integer -- sticker_id
    } deriving (Eq, Show)

data CallbackAttachment = CAMultimedia MultimediaAttachment
                        | CATemplate TemplateAttachment
  deriving (Eq, Show)


data MultimediaAttachment = MultimediaAttachment
    { maType :: AttachmentType
    , maPayload :: CallbackMultimediaPayload
    } deriving (Eq, Show)

data TemplateAttachment = TemplateAttachment
    { taTemplateTitle :: Maybe Text
    , taTemplateSubtitle :: Maybe Text
    , taTemplateUrl :: Maybe Text
    , taTemplatePayload :: CallbackTemplate
    } deriving (Eq, Show)

newtype CallbackMultimediaPayload =
          CallbackMultimediaPayload { cmpUrl :: Text } -- URL of the file
  deriving (Eq, Show)

data CallbackTemplate = CallbackTemplate
    { ctSharable :: Maybe Bool
    , ctElements :: [GenericElement]
    } deriving (Eq, Show)

newtype CallbackLocation =
          CallbackLocation { clPayload :: CallbackLocationPayload }
  deriving (Eq, Show)

newtype CallbackLocationPayload =
          CallbackLocationPayload { clpCoords :: CallbackCoordinates }
    deriving (Eq, Show)

data CallbackCoordinates = CallbackCoordinates
    { ccLat :: Double -- Latitude
    , ccLong :: Double -- Longitude
    } deriving (Eq, Show)

data CallbackFallback = CallbackFallback
    { cfTitle :: Text
    , cfURL :: Text
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
  toJSON (Message ident mSeq content) =
      case toJSON content of
        Object o -> Object $ go mSeq $ HM.insert "mid" (String ident) o
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
