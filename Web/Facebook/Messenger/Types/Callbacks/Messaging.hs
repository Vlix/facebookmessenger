module Web.Facebook.Messenger.Types.Callbacks.Messaging 
    ( FBCallbackMessaging (..)
    , FBCallbackSender (..)
    , FBCallbackRecipient (..)
    , module Web.Facebook.Messenger.Types.Callbacks.Message
    , module Web.Facebook.Messenger.Types.Callbacks.PostbackAuth
    , module Web.Facebook.Messenger.Types.Callbacks.Delivery
    , module Web.Facebook.Messenger.Types.Callbacks.AccountLink
    , module Web.Facebook.Messenger.Types.Callbacks.Read
    , module Web.Facebook.Messenger.Types.Callbacks.Echo
    ) where

import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)

import Web.Facebook.Messenger.Types.Callbacks.Message
import Web.Facebook.Messenger.Types.Callbacks.PostbackAuth
import Web.Facebook.Messenger.Types.Callbacks.Delivery
import Web.Facebook.Messenger.Types.Callbacks.AccountLink
import Web.Facebook.Messenger.Types.Callbacks.Read
import Web.Facebook.Messenger.Types.Callbacks.Echo


-- ------------------ --
--  MESSAGING OBJECT  --
-- ------------------ --

-- The different kinds of callbacks Facebook sends through WebHook
data FBCallbackMessaging =
    FBCallbackMessagingMessage
        { fbcb_message_sender    :: FBCallbackSender
        , fbcb_message_recipient :: FBCallbackRecipient
        , fbcb_message_timestamp :: Int
        , fbcb_message_message   :: FBCallbackMessage }
  | FBCallbackMessagingPostback
        { fbcb_postback_sender    :: FBCallbackSender
        , fbcb_postback_recipient :: FBCallbackRecipient
        , fbcb_postback_timestamp :: Int
        , fbcb_postback_postback  :: FBCallbackPostback }
  | FBCallbackMessagingAuth
        { fbcb_auth_sender    :: FBCallbackSender
        , fbcb_auth_recipient :: FBCallbackRecipient
        , fbcb_auth_timestamp :: Int
        , fbcb_auth_optin     :: FBCallbackOptin }
  | FBCallbackMessagingAccountLink
        { fbcb_account_sender    :: FBCallbackSender
        , fbcb_account_recipient :: FBCallbackRecipient
        , fbcb_account_timestamp :: Int
        , fbcb_account_linking   :: FBCallbackAccountLink }
  | FBCallbackMessagingDelivery
        { fbcb_delivery_sender    :: FBCallbackSender
        , fbcb_delivery_recipient :: FBCallbackRecipient
        , fbcb_delivery_delivery  :: FBCallbackDelivery }
  | FBCallbackMessagingRead
        { fbcb_read_sender    :: FBCallbackSender
        , fbcb_read_recipient :: FBCallbackRecipient
        , fbcb_read_timestamp :: Int
        , fbcb_read_read      :: FBCallbackRead }
  | FBCallbackMessagingEcho
        { fbcb_echo_sender    :: FBCallbackSender
        , fbcb_echo_recipient :: FBCallbackRecipient
        , fbcb_echo_timestamp :: Int
        , fbcb_echo_message   :: FBCallbackEcho }
  deriving (Eq, Show)

-- ALL MESSAGING HAS THESE TWO --
newtype FBCallbackSender = FBCallbackSender { fbcb_sender_id :: Text } -- Sender user ID
  deriving (Eq, Show)

newtype FBCallbackRecipient = FBCallbackRecipient { fbcb_recipient_id :: Text } -- Recipient user ID
  deriving (Eq, Show)

-- When representing a user, these IDs are page-scoped IDs (PSID). This means that the IDs of users are unique for a given page.


-- --------------------- --
--  MESSAGING INSTANCES  --
-- --------------------- --

instance FromJSON FBCallbackMessaging where
    parseJSON (Object o) = FBCallbackMessagingMessage <$> o .: "sender"
                                                      <*> o .: "recipient"
                                                      <*> o .: "timestamp"
                                                      <*> o .: "message"
                       <|> FBCallbackMessagingPostback <$> o .: "sender"
                                                       <*> o .: "recipient"
                                                       <*> o .: "timestamp"
                                                       <*> o .: "postback"
                       <|> FBCallbackMessagingAuth <$> o .: "sender"
                                                   <*> o .: "recipient"
                                                   <*> o .: "timestamp"
                                                   <*> o .: "optin"
                       <|> FBCallbackMessagingAccountLink <$> o .: "sender"
                                                          <*> o .: "recipient"
                                                          <*> o .: "timestamp"
                                                          <*> o .: "account_linking"
                       <|> FBCallbackMessagingDelivery <$> o .: "sender"
                                                       <*> o .: "recipient"
                                                       <*> o .: "delivery"
                       <|> FBCallbackMessagingRead <$> o .: "sender"
                                                   <*> o .: "recipient"
                                                   <*> o .: "timestamp"
                                                   <*> o .: "read"
                       <|> FBCallbackMessagingEcho <$> o .: "sender"
                                                   <*> o .: "recipient"
                                                   <*> o .: "timestamp"
                                                   <*> o .: "message"
    parseJSON wat = typeMismatch "FBCallbackMessaging" wat

-- ALL MESSAGING HAS THESE TWO --
instance FromJSON FBCallbackSender where
    parseJSON (Object o) = FBCallbackSender <$> o .: "id"
    parseJSON wat = typeMismatch "FBCallbackSender" wat

instance FromJSON FBCallbackRecipient where
    parseJSON (Object o) = FBCallbackRecipient <$> o .: "id"
    parseJSON wat = typeMismatch "FBCallbackRecipient" wat


instance ToJSON FBCallbackMessaging where
    toJSON (FBCallbackMessagingMessage sender recipient timestamp message) = object [ "sender" .= sender
                                                                                    , "recipient" .= recipient
                                                                                    , "timestamp" .= timestamp
                                                                                    , "message" .= message
                                                                                    ]
    toJSON (FBCallbackMessagingPostback sender recipient timestamp postback) = object [ "sender" .= sender
                                                                                      , "recipient" .= recipient
                                                                                      , "timestamp" .= timestamp
                                                                                      , "postback" .= postback
                                                                                      ]                                                                             
    toJSON (FBCallbackMessagingAuth sender recipient timestamp optin) = object [ "sender" .= sender
                                                                               , "recipient" .= recipient
                                                                               , "timestamp" .= timestamp
                                                                               , "optin" .= optin
                                                                               ]
    toJSON (FBCallbackMessagingAccountLink sender recipient timestamp linking) = object [ "sender" .= sender
                                                                                        , "recipient" .= recipient
                                                                                        , "timestamp" .= timestamp
                                                                                        , "account_linking" .= linking
                                                                                        ]
    toJSON (FBCallbackMessagingDelivery sender recipient delivery) = object [ "sender" .= sender
                                                                            , "recipient" .= recipient
                                                                            , "delivery" .= delivery
                                                                            ]
    toJSON (FBCallbackMessagingRead sender recipient timestamp read') = object [ "sender" .= sender
                                                                               , "recipient" .= recipient
                                                                               , "timestamp" .= timestamp
                                                                               , "read" .= read'
                                                                               ]
    toJSON (FBCallbackMessagingEcho sender recipient timestamp message) = object [ "sender" .= sender
                                                                                 , "recipient" .= recipient
                                                                                 , "timestamp" .= timestamp
                                                                                 , "message" .= message
                                                                                 ]

-- ALL MESSAGING HAS THESE TWO -- 
instance ToJSON FBCallbackSender where
    toJSON (FBCallbackSender ident) = object [ "id" .= ident ]

instance ToJSON FBCallbackRecipient where
    toJSON (FBCallbackRecipient ident) = object [ "id" .= ident ]
