module Web.Facebook.Messenger.Types.Callbacks.Echo where


import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)

import Web.Facebook.Messenger.Types.Requests.Attachment (FBRequestAttachment)

-- --------------- --
--  ECHO CALLBACK  --
-- --------------- --

data FBCallbackEcho =
    FBCallbackEchoText
    { fbcb_echo_isecho   :: Bool -- Indicates the message sent from the page itself
    , fbcb_echo_appid    :: Text -- ID of the app from which the message was sent
  -- app_id might be Number, documentation is ambiguous
    , fbcb_echo_metadata :: Text -- Custom string passed to the Send API as the metadata field
    , fbcb_echo_mid      :: Text -- Message ID
    , fbcb_echo_seq      :: Int  -- Sequence number
    , fbcb_echo_text     :: Text } -- Text of message
  | FBCallbackEchoAttachment
    { fbcb_echo_isecho      :: Bool
    , fbcb_echo_appid       :: Text
    , fbcb_echo_metadata    :: Text
    , fbcb_echo_mid         :: Text
    , fbcb_echo_seq         :: Int
    , fbcb_echo_attachments :: [FBRequestAttachment] } -- Template payload as described in the Send API Reference (.Callbacks.Requests)
  | FBCallbackEchoFallback
    { fbcb_echo_isecho   :: Bool
    , fbcb_echo_appid    :: Text
    , fbcb_echo_metadata :: Text
    , fbcb_echo_mid      :: Text
    , fbcb_echo_seq      :: Int
    , fbcb_echo_fallback :: [FBCallbackFallback] } -- 

data FBCallbackFallback = FBCallbackFallback
    { fbcb_fallback_title   :: Maybe Text -- Title of attachment (optional)
    , fbcb_fallback_url     :: Maybe Text -- URL of attachment (optional)
    , fbcb_fallback_payload :: Maybe Text -- Payload of attachment (optional)
    }


-- ---------------- --
--  ECHO INSTANCES  --
-- ---------------- --

instance FromJSON FBCallbackEcho where
    parseJSON (Object o) = FBCallbackEchoText <$> o .: "is_echo"
                                              <*> o .: "app_id"
                                              <*> o .: "metadata"
                                              <*> o .: "mid"
                                              <*> o .: "seq"
                                              <*> o .: "text"
                       <|> FBCallbackEchoAttachment <$> o .: "is_echo"
                                                    <*> o .: "app_id"
                                                    <*> o .: "metadata"
                                                    <*> o .: "mid"
                                                    <*> o .: "seq"
                                                    <*> o .: "attachments"
                       <|> FBCallbackEchoFallback <$> o .: "is_echo"
                                                  <*> o .: "app_id"
                                                  <*> o .: "metadata"
                                                  <*> o .: "mid"
                                                  <*> o .: "seq"
                                                  <*> o .: "fallback"
    parseJSON wat = typeMismatch "FBCallbackEcho" wat

instance FromJSON FBCallbackFallback where
    parseJSON (Object o) = FBCallbackFallback <$> o .: "title"
                                              <*> o .: "url"
                                              <*> o .: "payload"
    parseJSON wat = typeMismatch "FBCallbackFallback" wat 


instance ToJSON FBCallbackEcho where
    toJSON (FBCallbackEchoText isecho appid metadata mid seq' txt) = object [ "is_echo" .= isecho
                                                                            , "app_id" .= appid
                                                                            , "metadata" .= metadata
                                                                            , "mid" .= mid
                                                                            , "seq" .= seq'
                                                                            , "text" .= txt
                                                                            ]
    toJSON (FBCallbackEchoAttachment isecho appid metadata mid seq' attachments) = object [ "is_echo" .= isecho
                                                                                          , "app_id" .= appid
                                                                                          , "metadata" .= metadata
                                                                                          , "mid" .= mid
                                                                                          , "seq" .= seq'
                                                                                          , "attachments" .= attachments
                                                                                          ]
    toJSON (FBCallbackEchoFallback isecho appid metadata mid seq' fallback) = object [ "is_echo" .= isecho
                                                                                     , "app_id" .= appid
                                                                                     , "metadata" .= metadata
                                                                                     , "mid" .= mid
                                                                                     , "seq" .= seq'
                                                                                     , "fallback" .= fallback
                                                                                     ]

instance ToJSON FBCallbackFallback where
    toJSON (FBCallbackFallback title url payload) = object [ "type" .= String "fallback"
                                                           , "title" .= title
                                                           , "url" .= url
                                                           , "payload" .= payload
                                                           ]
