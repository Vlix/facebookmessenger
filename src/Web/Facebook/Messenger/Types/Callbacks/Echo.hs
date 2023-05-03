{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.Echo
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This callback will occur when a message has been sent by your page.
You may receive text messsages or messages with attachments (image, video, audio, template or fallback).
The payload will also include an optional custom metadata sent by the sender, and the corresponding app_id.
You can subscribe to this callback by selecting the @"message_echoes"@ field when setting up your webhook.

Multiple types of messages are supported:

* Text message
* Message with image, audio, video or file attachment
* Message with template attachment
* Message with fallback attachment

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/message-echoes
-}
module Web.Facebook.Messenger.Types.Callbacks.Echo (
  -- * Echo Callback
  Echo (..)
  , EchoContent (..)
  , EchoText (..)
  , EchoAttachment (..)
  , EchoButton (..)
  , EchoFallback (..)
  )
where


import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Web.Facebook.Messenger.Internal
import Web.Facebook.Messenger.Types.Requests
import Web.Facebook.Messenger.Types.Callbacks.Message (MessageId)


-- --------------- --
--  ECHO CALLBACK  --
-- --------------- --

-- | Echo callback sent when a message is sent as the page. This can also happen if an admin/editor of the page responds manually to a user.
data Echo = Echo
    { eIsEcho :: Bool -- ^ Indicates the message sent from the page itself
    , eAppId :: Maybe Integer
    -- ^ ID of the app from which the message was sent.
    -- (`eAppId` was a @Number@ at the time of making this module /2017-10-03/)
    , eMetaData :: Maybe Text
    -- ^ Custom string passed to the Send API as the metadata field.
    -- Only present if the metadata property was set in the original message.
    , eMid :: MessageId -- ^ Message ID
    , eSeq :: Maybe Integer -- ^ Sequence number
    , eContent :: EchoContent -- ^ Contents of the Echo callback
    } deriving stock (Eq, Show, Read, Ord)

-- | Different kinds of Echo callbacks
data EchoContent = EText EchoText -- ^ Regular message
                 | EAttachment EchoAttachment -- ^ Attachment message
                 | EButton EchoButton -- ^ Button Template
                 | EFallback EchoFallback -- ^ Any other message
  deriving stock (Eq, Show, Read, Ord)

-- | Text message
newtype EchoText = EchoText { eText :: Text }
  deriving stock (Eq, Show, Read, Ord)

-- | Attachment message. `RequestAttachment` as described in the Send API Reference ("Requests")
newtype EchoAttachment = EchoAttachment { eAttachments :: [RequestAttachment] }
  deriving stock (Eq, Show, Read, Ord)

-- | Button used in an echoed Button Template.
data EchoButton = EchoButton { ebText :: Text
                             , ebButtons :: NonEmpty TemplateButton
                             }
  deriving stock (Eq, Show, Read, Ord)

-- | Fallback message
data EchoFallback = EchoFallback { efText :: Maybe Text
                                 , efFallback :: [Fallback]
                                 }
  deriving stock (Eq, Show, Read, Ord)


-- ---------------- --
--  ECHO INSTANCES  --
-- ---------------- --

instance FromJSON Echo where
  parseJSON = withObject "Echo" $ \o -> do
      content <- getEchoContent o
      Echo <$> o .: "is_echo"
           <*> o .:? "app_id"
           <*> o .:? "metadata"
           <*> o .: "mid"
           <*> o .:? "seq"
           <*> pure content
    where getEchoContent o = EFallback <$> parseJSON (Object o) <|> tryRegular
            where tryRegular = do
                    mText <- o .:? "text"
                    case mText of
                      Just t -> EButton <$> parseJSON (Object o)
                            <|> pure (EText $ EchoText t)
                      Nothing -> EAttachment <$> parseJSON (Object o)

instance FromJSON EchoText where
  parseJSON = withObject "EchoText" $ \o ->
      EchoText <$> o .: "text"

instance FromJSON EchoAttachment where
  parseJSON = withObject "EchoAttachment" $ \o -> do
      atts <- o .: attField
      let actualAtts = filter (/= Object mempty) (atts :: [Value])
          newObj = KM.insert attField (toJSON actualAtts) o
      EchoAttachment <$> newObj .: attField
    where attField = "attachments"

instance FromJSON EchoButton where
  parseJSON = withObject "EchoButton" $ \o -> do
      att <- o .: "attachments"
      case att of
        [a] -> do
          pl <- a .: "payload"
          EchoButton <$> o .: "text" <*> pl .: "buttons"
        _ -> fail "more than one attachment"

instance FromJSON EchoFallback where
  parseJSON = withObject "EchoFallback" $ \o ->
      EchoFallback <$> o .:? "text"
                   <*> o .: "attachments"


instance ToJSON Echo where
  toJSON echo = Object $ KM.fromList (catMaybes basis) `KM.union` extra
   where
    basis = [ "is_echo" .=! eIsEcho echo
            , "mid" .=! eMid echo
            , "app_id" .=! eAppId echo
            , "metadata" .=!! eMetaData echo
            , "seq" .=!! eSeq echo
            ]
    extra = case eContent echo of
              EText x -> KM.fromList [ "text" .= eText x ]
              EButton x | Object o <- toJSON x -> o
                        | otherwise -> KM.empty
              EAttachment x -> KM.fromList [ "attachments" .= eAttachments x ]
              EFallback x -> KM.fromList . catMaybes $
                                [ "text" .=!! efText x
                                , "attachments" .=! efFallback x
                                ]

instance ToJSON EchoText where
  toJSON (EchoText txt) =
      object ["text" .= txt]

instance ToJSON EchoAttachment where
  toJSON (EchoAttachment atts) =
      object ["attachments" .= atts]

instance ToJSON EchoButton where
  toJSON (EchoButton txt btns) =
      object [ "text" .= txt
             , "attachments" .= [mkBtnTemplate]
             ]
    where  mkBtnTemplate = object [ "title" .= String ""
                                  , "url" .= Null
                                  , "type" .= String "template"
                                  , "payload" .= object [ "template_type" .= String "button"
                                                        , "buttons" .= btns
                                                        ]
                                  ]

instance ToJSON EchoFallback where
  toJSON (EchoFallback mTxt fbs) =
      object' [ "text" .=!! mTxt
              , "attachments" .=! fbs
              ]
