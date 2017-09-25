{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Facebook.Messenger.Types.Callbacks.Messaging 
    ( CallbackMessaging (..)
    , CallbackSender (..)
    , CallbackRecipient (..)
    , module Web.Facebook.Messenger.Types.Callbacks.Message
    , module Web.Facebook.Messenger.Types.Callbacks.Delivery
    , module Web.Facebook.Messenger.Types.Callbacks.Read
    , module Web.Facebook.Messenger.Types.Callbacks.Echo
    , module Web.Facebook.Messenger.Types.Callbacks.Postback
    , module Web.Facebook.Messenger.Types.Callbacks.Optin
    , module Web.Facebook.Messenger.Types.Callbacks.Referral
    , module Web.Facebook.Messenger.Types.Callbacks.Payment
    , module Web.Facebook.Messenger.Types.Callbacks.CheckoutUpdate
    , module Web.Facebook.Messenger.Types.Callbacks.PreCheckout
    , module Web.Facebook.Messenger.Types.Callbacks.AccountLink
    , module Web.Facebook.Messenger.Types.Callbacks.PolicyEnforcement
    , module Web.Facebook.Messenger.Types.Callbacks.AppRoles
    , module Web.Facebook.Messenger.Types.Callbacks.PassThreadControl
    , module Web.Facebook.Messenger.Types.Callbacks.TakeThreadControl
    ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text
import Data.HashMap.Strict as HM

import Web.Facebook.Messenger.Types.Callbacks.Message
import Web.Facebook.Messenger.Types.Callbacks.Delivery
import Web.Facebook.Messenger.Types.Callbacks.Read
import Web.Facebook.Messenger.Types.Callbacks.Echo
import Web.Facebook.Messenger.Types.Callbacks.Postback
import Web.Facebook.Messenger.Types.Callbacks.Optin
import Web.Facebook.Messenger.Types.Callbacks.Referral
import Web.Facebook.Messenger.Types.Callbacks.Payment
import Web.Facebook.Messenger.Types.Callbacks.CheckoutUpdate
import Web.Facebook.Messenger.Types.Callbacks.PreCheckout
import Web.Facebook.Messenger.Types.Callbacks.AccountLink
import Web.Facebook.Messenger.Types.Callbacks.PolicyEnforcement
import Web.Facebook.Messenger.Types.Callbacks.AppRoles
import Web.Facebook.Messenger.Types.Callbacks.PassThreadControl
import Web.Facebook.Messenger.Types.Callbacks.TakeThreadControl
import Web.Facebook.Messenger.Types.Static


-- ------------------ --
--  MESSAGING OBJECT  --
-- ------------------ --

-- The different kinds of callbacks Facebook sends through WebHook
data CallbackMessaging =
  CallbackMessaging
    { sender :: Maybe CallbackSender
    , recipient :: CallbackRecipient
    , timestamp :: Maybe Integer
    , content :: CallbackContent }
    deriving (Eq, Show)


data CallbackContent =
    CMMessage Message
  | CMDelivery Delivery
  | CMRead ReadCallback
  | CMEcho Echo
  | CMPostback Postback
  | CMOptin Optin
  | CMReferral Referral
  | CMPayment Payment
  | CMCheckoutUpdate CheckoutUpdate
  | CMPreCheckOut PreCheckout
  | CMAccountLink AccountLink
  | CMPolicy PolicyEnforcement
  | CMAppRoles AppRoles
  | CMPassThread PassThread
  | CMTakeThread TakeThread
  deriving (Eq, Show)


-- ALMOST ALL MESSAGING HAS THESE TWO --
newtype CallbackSender = CallbackSender { senderId :: PSID } -- Sender user ID
  deriving (Eq, Show)

newtype CallbackRecipient = CallbackRecipient { recipientId :: PageID } -- Recipient user ID/PAGE_ID
  deriving (Eq, Show)

newtype PSID = PSID Text deriving (Eq, Show, FromJSON, ToJSON)
newtype PageID = PageID Text deriving (Eq, Show, FromJSON, ToJSON)

-- When representing a user, these IDs are page-scoped IDs (PSID). This means that the IDs of users are unique for a given page.


-- --------------------- --
--  MESSAGING INSTANCES  --
-- --------------------- --

instance FromJSON CallbackMessaging where
  parseJSON = withObject "CallbackMessaging" $ \o -> do
    CallbackMessaging <$> o .:? "sender"
                      <*> o .: "recipient"
                      <*> o .:? "timestamp"
                      <*> parseJSON (Object o)

instance FromJSON CallbackContent where
  parseJSON = withObject "CallbackContent" $ \o ->
          tryMessage o
      <|> CMDelivery <$> o .: "delivery"
      <|> CMRead <$> o .: "read"
      <|> CMPostback <$> o .: "postback"
      <|> CMOptin <$> o .: "optin"
      <|> CMReferral <$> o .: "referral"
      <|> CMPayment <$> o .: "payment"
      <|> CMCheckoutUpdate <$> o .: "checkout_update"
      <|> CMPreCheckOut <$> o .: "pre_checkout"
      <|> CMAccountLink <$> o .: "account_linking"
      <|> CMPolicy <$> o .: "policy-enforcement"
      <|> CMAppRoles <$> o .: "app_roles"
      <|> CMPassThread <$> o .: "pass_thread_control"
      <|> CMTakeThread <$> o .: "take_thread_control"
    where tryMessage o = do
              msg <- o .: "message"
              case "is_echo" `HM.lookup` msg of
                Just _ -> CMEcho <$> parseJSON (Object msg)
                _ -> CMMessage <$> parseJSON (Object msg)


-- ALL MESSAGING HAS THESE TWO --
--  (Except the OptinRef one)  --
instance FromJSON CallbackSender where
  parseJSON = withObject "CallbackSender" $ \o ->
    CallbackSender <$> o .: "id"

instance FromJSON CallbackRecipient where
  parseJSON = withObject "CallbackRecipient" $ \o ->
    CallbackRecipient <$> o .: "id"


instance ToJSON CallbackMessaging where
  toJSON (CallbackMessaging msender recipient mtimestamp content) =
      object' [ "sender"    .=!! msender
              , "recipient" .=! recipient
              , "timestamp" .=!! mtimestamp
              , mkContent content
              ]
    where
      mkContent (CMMessage cb) = "message" .=! cb
      mkContent (CMDelivery cb) = "delivery" .=! cb
      mkContent (CMRead cb) = "read" .=! cb
      mkContent (CMEcho cb) = "message" .=! cb
      mkContent (CMPostback cb) = "postback" .=! cb
      mkContent (CMOptin cb) = "optin" .=! cb
      mkContent (CMReferral cb) = "referral" .=! cb
      mkContent (CMPayment cb) = "payment" .=! cb
      mkContent (CMCheckoutUpdate cb) = "checkout_update" .=! cb
      mkContent (CMPreCheckOut cb) = "pre_checkout" .=! cb
      mkContent (CMAccountLink cb) = "account_linking" .=! cb
      mkContent (CMPolicy cb) = "policy-enforcement" .=! cb
      mkContent (CMAppRoles cb) = "app_roles" .=! cb
      mkContent (CMPassThread cb) = "pass_thread_control" .=! cb
      mkContent (CMTakeThread cb) = "take_thread_control" .=! cb


-- ALL MESSAGING HAS THESE TWO --
--  (Except the OptinRef one)   -
instance ToJSON CallbackSender where
  toJSON (CallbackSender ident) = object [ "id" .= ident ]

instance ToJSON CallbackRecipient where
  toJSON (CallbackRecipient ident) = object [ "id" .= ident ]
