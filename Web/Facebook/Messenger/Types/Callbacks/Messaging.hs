module Web.Facebook.Messenger.Types.Callbacks.Messaging 
    ( CallbackMessaging (..)
    , CallbackSender (..)
    , CallbackRecipient (..)
    , module Web.Facebook.Messenger.Types.Callbacks.Message
    , module Web.Facebook.Messenger.Types.Callbacks.PostbackOptin
    , module Web.Facebook.Messenger.Types.Callbacks.Delivery
    , module Web.Facebook.Messenger.Types.Callbacks.AccountLink
    , module Web.Facebook.Messenger.Types.Callbacks.Read
    , module Web.Facebook.Messenger.Types.Callbacks.Echo
    , module Web.Facebook.Messenger.Types.Callbacks.Payment
    , module Web.Facebook.Messenger.Types.Callbacks.CheckoutUpdate
    ) where

import Control.Applicative  ((<|>))
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)
import Data.Text
import Data.HashMap.Strict  as HM

import Web.Facebook.Messenger.Types.Callbacks.Message
import Web.Facebook.Messenger.Types.Callbacks.PostbackOptin
import Web.Facebook.Messenger.Types.Callbacks.Delivery
import Web.Facebook.Messenger.Types.Callbacks.AccountLink
import Web.Facebook.Messenger.Types.Callbacks.Read
import Web.Facebook.Messenger.Types.Callbacks.Echo
import Web.Facebook.Messenger.Types.Callbacks.Payment
import Web.Facebook.Messenger.Types.Callbacks.CheckoutUpdate


-- ------------------ --
--  MESSAGING OBJECT  --
-- ------------------ --

-- The different kinds of callbacks Facebook sends through WebHook
data CallbackMessaging =
    CallbackMessagingMessage
    { cb_message_sender    :: CallbackSender
    , cb_message_recipient :: CallbackRecipient
    , cb_message_timestamp :: Int
    , cb_message_message   :: CallbackMessage }
  | CallbackMessagingPostback
    { cb_postback_sender    :: CallbackSender
    , cb_postback_recipient :: CallbackRecipient
    , cb_postback_timestamp :: Int
    , cb_postback_postback  :: Postback }
  | CallbackMessagingOptin
    { cb_auth_sender    :: CallbackSender
    , cb_auth_recipient :: CallbackRecipient
    , cb_auth_timestamp :: Int
    , cb_auth_optin     :: Optin }
  | CallbackMessagingOptinRef
    { cb_optin_recipient :: CallbackRecipient
    , cb_optin_timestamp :: Int
    , cb_optin_optin     :: OptinRef }
  | CallbackMessagingAccountLink
    { cb_account_sender    :: CallbackSender
    , cb_account_recipient :: CallbackRecipient
    , cb_account_timestamp :: Int
    , cb_account_linking   :: AccountLink }
  | CallbackMessagingDelivery
    { cb_delivery_sender    :: CallbackSender
    , cb_delivery_recipient :: CallbackRecipient
    , cb_delivery_delivery  :: Delivery }
  | CallbackMessagingRead
    { cb_read_sender    :: CallbackSender
    , cb_read_recipient :: CallbackRecipient
    , cb_read_timestamp :: Int
    , cb_read_read      :: ReadCallback }
  | CallbackMessagingEcho
    { cb_echo_sender    :: CallbackSender
    , cb_echo_recipient :: CallbackRecipient
    , cb_echo_timestamp :: Int
    , cb_echo_message   :: Echo }
  | CallbackMessagingPayment
    { cb_payment_sender    :: CallbackSender
    , cb_payment_recipient :: CallbackRecipient
    , cb_payment_timestamp :: Int
    , cb_payment_payment   :: Payment }
  | CallbackMessagingCheckoutUpdate
    { cb_coupdate_sender    :: CallbackSender
    , cb_coupdate_recipient :: CallbackRecipient
    , cb_coupdate_timestamp :: Int
    , cb_coupdate_checkout_update :: CheckoutUpdate
    } deriving (Eq, Show)
-- Payment and Checkout should be added


-- ALL MESSAGING HAS THESE TWO --
--  (Except the OptinRef one)  --
newtype CallbackSender = CallbackSender { cb_sender_id :: Text } -- Sender user ID
  deriving (Eq, Show)

newtype CallbackRecipient = CallbackRecipient { cb_recipient_id :: Text } -- Recipient user ID/PAGE_ID
  deriving (Eq, Show)

-- When representing a user, these IDs are page-scoped IDs (PSID). This means that the IDs of users are unique for a given page.


-- --------------------- --
--  MESSAGING INSTANCES  --
-- --------------------- --

instance FromJSON CallbackMessaging where
  parseJSON (Object o) = case HM.lookup "sender" o of
    Nothing -> CallbackMessagingOptinRef <$> o .: "recipient"
                                         <*> o .: "timestamp"
                                         <*> o .: "optin"
    Just _  -> case HM.lookup "timestamp" o of
      Nothing -> CallbackMessagingDelivery <$> o .: "sender"
                                           <*> o .: "recipient"
                                           <*> o .: "delivery"
      Just _  -> case HM.lookup "message" o of
        Just _  -> CallbackMessagingEcho <$> o .: "sender"
                                         <*> o .: "recipient"
                                         <*> o .: "timestamp"
                                         <*> o .: "message"
               <|> CallbackMessagingMessage <$> o .: "sender"
                                            <*> o .: "recipient"
                                            <*> o .: "timestamp"
                                            <*> o .: "message"
        Nothing -> CallbackMessagingRead <$> o .: "sender"
                                         <*> o .: "recipient"
                                         <*> o .: "timestamp"
                                         <*> o .: "read"
               <|> CallbackMessagingPostback <$> o .: "sender"
                                             <*> o .: "recipient"
                                             <*> o .: "timestamp"
                                             <*> o .: "postback"
               <|> CallbackMessagingCheckoutUpdate <$> o .: "sender"
                                                   <*> o .: "recipient"
                                                   <*> o .: "timestamp"
                                                   <*> o .: "checkout_update"
               <|> CallbackMessagingPayment <$> o .: "sender"
                                            <*> o .: "recipient"
                                            <*> o .: "timestamp"
                                            <*> o .: "payment"
               <|> CallbackMessagingAccountLink <$> o .: "sender"
                                                <*> o .: "recipient"
                                                <*> o .: "timestamp"
                                                <*> o .: "account_linking"
               <|> CallbackMessagingOptin <$> o .: "sender"
                                          <*> o .: "recipient"
                                          <*> o .: "timestamp"
                                          <*> o .: "optin"
  parseJSON wat = typeMismatch "CallbackMessaging" wat

-- ALL MESSAGING HAS THESE TWO --
--  (Except the OptinRef one)  --
instance FromJSON CallbackSender where
  parseJSON (Object o) = CallbackSender <$> o .: "id"
  parseJSON wat = typeMismatch "CallbackSender" wat

instance FromJSON CallbackRecipient where
  parseJSON (Object o) = CallbackRecipient <$> o .: "id"
  parseJSON wat = typeMismatch "CallbackRecipient" wat


instance ToJSON CallbackMessaging where
  toJSON (CallbackMessagingMessage sender recipient timestamp message) =
    object [ "sender"    .= sender
           , "recipient" .= recipient
           , "timestamp" .= timestamp
           , "message"   .= message
           ]
  toJSON (CallbackMessagingPostback sender recipient timestamp postback) =
    object [ "sender"    .= sender
           , "recipient" .= recipient
           , "timestamp" .= timestamp
           , "postback"  .= postback
           ]
  toJSON (CallbackMessagingOptin sender recipient timestamp optin) =
    object [ "sender"    .= sender
           , "recipient" .= recipient
           , "timestamp" .= timestamp
           , "optin"     .= optin
           ]
  toJSON (CallbackMessagingOptinRef recipient timestamp optin) =
    object [ "recipient" .= recipient
           , "timestamp" .= timestamp
           , "optin"     .= optin
           ]
  toJSON (CallbackMessagingAccountLink sender recipient timestamp linking) =
    object [ "sender"          .= sender
           , "recipient"       .= recipient
           , "timestamp"       .= timestamp
           , "account_linking" .= linking
           ]
  toJSON (CallbackMessagingDelivery sender recipient delivery) =
    object [ "sender"    .= sender
           , "recipient" .= recipient
           , "delivery"  .= delivery
           ]
  toJSON (CallbackMessagingRead sender recipient timestamp read') =
    object [ "sender"    .= sender
           , "recipient" .= recipient
           , "timestamp" .= timestamp
           , "read"      .= read'
           ]
  toJSON (CallbackMessagingEcho sender recipient timestamp message) =
    object [ "sender"    .= sender
           , "recipient" .= recipient
           , "timestamp" .= timestamp
           , "message"   .= message
           ]
  toJSON (CallbackMessagingPayment sender recipient timestamp payment) =
    object [ "sender"    .= sender
           , "recipient" .= recipient
           , "timestamp" .= timestamp
           , "payment"   .= payment
           ]
  toJSON (CallbackMessagingCheckoutUpdate sender recipient timestamp checkout_update) =
    object [ "sender"          .= sender
           , "recipient"       .= recipient
           , "timestamp"       .= timestamp
           , "checkout_update" .= checkout_update
           ]

-- ALL MESSAGING HAS THESE TWO --
--  (Except the OptinRef one)   -
instance ToJSON CallbackSender where
  toJSON (CallbackSender ident) = object [ "id" .= ident ]

instance ToJSON CallbackRecipient where
  toJSON (CallbackRecipient ident) = object [ "id" .= ident ]
