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
import Data.Aeson.Types     (Parser)
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
import Web.Facebook.Messenger.Types.Static


-- ------------------ --
--  MESSAGING OBJECT  --
-- ------------------ --

-- The different kinds of callbacks Facebook sends through WebHook
data CallbackMessaging =
    CallbackMessagingMessage
    { cb_sender    :: CallbackSender
    , cb_recipient :: CallbackRecipient
    , cb_timestamp :: Integer
    , cb_message   :: CallbackMessage }
  | CallbackMessagingPostback
    { cb_sender    :: CallbackSender
    , cb_recipient :: CallbackRecipient
    , cb_timestamp :: Integer
    , cb_postback  :: Postback }
  | CallbackMessagingOptin
    { cb_sender    :: CallbackSender
    , cb_recipient :: CallbackRecipient
    , cb_timestamp :: Integer
    , cb_optin     :: Optin }
  | CallbackMessagingOptinRef
    { cb_recipient :: CallbackRecipient
    , cb_timestamp :: Integer
    , cb_optin_ref :: OptinRef }
  | CallbackMessagingReferral
    { cb_sender    :: CallbackSender
    , cb_recipient :: CallbackRecipient
    , cb_timestamp :: Integer
    , cb_referral  :: Referral }
  | CallbackMessagingAccountLink
    { cb_sender    :: CallbackSender
    , cb_recipient :: CallbackRecipient
    , cb_timestamp :: Integer
    , cb_account_linking :: AccountLink }
  | CallbackMessagingDelivery
    { cb_sender      :: CallbackSender
    , cb_recipient   :: CallbackRecipient
    , cb_timestamp_d :: Maybe Integer
    , cb_delivery    :: Delivery }
  | CallbackMessagingRead
    { cb_sender    :: CallbackSender
    , cb_recipient :: CallbackRecipient
    , cb_timestamp :: Integer
    , cb_read      :: ReadCallback }
  | CallbackMessagingEcho
    { cb_sender    :: CallbackSender
    , cb_recipient :: CallbackRecipient
    , cb_timestamp :: Integer
    , cb_echo_message :: Echo }
  | CallbackMessagingPayment
    { cb_sender    :: CallbackSender
    , cb_recipient :: CallbackRecipient
    , cb_timestamp :: Integer
    , cb_payment   :: Payment }
  | CallbackMessagingCheckoutUpdate
    { cb_sender    :: CallbackSender
    , cb_recipient :: CallbackRecipient
    , cb_timestamp :: Integer
    , cb_checkout_update :: CheckoutUpdate
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
  parseJSON = withObject "CallbackMessaging" $ \o -> do
    let mSender    = "sender"    `HM.lookup` o
        mRecipient = "recipient" `HM.lookup` o
        mTimestamp = "timestamp" `HM.lookup` o
    case (mRecipient, mTimestamp, mSender) of
      (_,_,Nothing) ->
        CallbackMessagingOptinRef <$> o .: "recipient"
                                  <*> o .: "timestamp"
                                  <*> o .: "optin"
      (_,Nothing,_) ->
        CallbackMessagingDelivery <$> o .: "sender"
                                  <*> o .: "recipient"
                                  <*> o .:? "timestamp"
                                  <*> o .: "delivery"
      (Just _, Just _, Just _) -> do
        sender    <- o .: "sender"    :: Parser CallbackSender
        recipient <- o .: "recipient" :: Parser CallbackRecipient
        timestamp <- o .: "timestamp" :: Parser Integer
              CallbackMessagingEcho           sender recipient timestamp <$> o .: "message"
          <|> CallbackMessagingMessage        sender recipient timestamp <$> o .: "message"
          <|> CallbackMessagingRead           sender recipient timestamp <$> o .: "read"
          <|> CallbackMessagingDelivery       sender recipient <$> o .:? "timestamp"
                                                               <*> o .: "delivery"
          <|> CallbackMessagingPostback       sender recipient timestamp <$> o .: "postback"
          <|> CallbackMessagingReferral       sender recipient timestamp <$> o .: "referral"
          <|> CallbackMessagingCheckoutUpdate sender recipient timestamp <$> o .: "checkout_update"
          <|> CallbackMessagingPayment        sender recipient timestamp <$> o .: "payment"
          <|> CallbackMessagingAccountLink    sender recipient timestamp <$> o .: "account_linking"
          <|> CallbackMessagingOptin          sender recipient timestamp <$> o .: "optin"
      _ -> fail "No recipient or "

-- ALL MESSAGING HAS THESE TWO --
--  (Except the OptinRef one)  --
instance FromJSON CallbackSender where
  parseJSON = withObject "CallbackSender" $ \o ->
    CallbackSender <$> o .: "id"

instance FromJSON CallbackRecipient where
  parseJSON = withObject "CallbackRecipient" $ \o ->
    CallbackRecipient <$> o .: "id"


instance ToJSON CallbackMessaging where
  toJSON (CallbackMessagingOptinRef recipient timestamp optin) =
    object [ "recipient" .= recipient
           , "timestamp" .= timestamp
           , "optin"     .= optin
           ]
  toJSON (CallbackMessagingDelivery sender recipient mtimestamp delivery) =
    object' [ "sender"    .=! sender
            , "recipient" .=! recipient
            , "timestamp" .=!! mtimestamp
            , "delivery"  .=! delivery
            ]
  toJSON other = object $ extra : basis
   where
    basis = [ "sender"    .= cb_sender other
            , "recipient" .= cb_recipient other
            , "timestamp" .= cb_timestamp other
            ]
    extra = case other of
      msg@CallbackMessagingMessage{}       -> "message"         .= cb_message msg
      pb@CallbackMessagingPostback{}       -> "postback"        .= cb_postback pb
      optin@CallbackMessagingOptin{}       -> "optin"           .= cb_optin optin
      ref@CallbackMessagingReferral{}      -> "referral"        .= cb_referral ref
      al@CallbackMessagingAccountLink{}    -> "account_linking" .= cb_account_linking al
      read'@CallbackMessagingRead{}        -> "read"            .= cb_read read'
      echo@CallbackMessagingEcho{}         -> "message"         .= cb_echo_message echo
      pay@CallbackMessagingPayment{}       -> "payment"         .= cb_payment pay
      up@CallbackMessagingCheckoutUpdate{} -> "checkout_update" .= cb_checkout_update up
      _ -> ("oops",String "This shouldn't be here, please report")

-- ALL MESSAGING HAS THESE TWO --
--  (Except the OptinRef one)   -
instance ToJSON CallbackSender where
  toJSON (CallbackSender ident) = object [ "id" .= ident ]

instance ToJSON CallbackRecipient where
  toJSON (CallbackRecipient ident) = object [ "id" .= ident ]
