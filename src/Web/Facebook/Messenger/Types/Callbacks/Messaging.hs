{-# LANGUAGE LambdaCase #-}
{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.Messaging
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

When you setup your webhook, there are 11 relevant webhook events (subscription fields) for this integration.
All fields are optional so select the fields most relevant for your experience.

(Payment-related callbacks are in beta and will only appear to developers who have access.
Request access to our beta program for payments.)

Webhooks with descriptions:
* @messages@ - /Subscribes to Message Received events/
* @message_deliveries@ - /Subscribes to Message Delivered events/
* @message_reads@ - /Subscribes to Message Read events/
* @message_echoes@ - /Subscribes to Message Echo events/
* @messaging_postbacks@ - /Subscribes to Postback Received events/
* @messaging_optins@ - /Subscribes to Plugin Opt-in events/
* @messaging_referrals@ - /Subscribes to Referral events/
* @messaging_checkout_updates@ (BETA) - /Subscribes to Checkout Update events/
* @messaging_pre_checkouts@ (BETA) - /Subscribes to PreCheckout events/
* @messaging_payments@ (BETA) - /Subscribes to Payment events/
* @messaging_account_linking@ - /Subscribes to Account Linking events/
* @messaging_policy_enforcement@ - /Subscribes to Policy Enforcement events/
* @standby@ - /Subscribes to Standby events/
* @messaging_handovers@ - /Subscribes to Thread Control events/
-}
module Web.Facebook.Messenger.Types.Callbacks.Messaging (
  -- * Webhook Entry
  CallbackMessaging (..)
  , CallbackContent (..)
  , CallbackSender (..)
  , CallbackRecipient (..)
  , PriorMessage (..)
  -- * Exported Modules
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
import Control.Monad (when)
import Data.Aeson
import Data.HashMap.Strict as HM
import Data.Text (Text)

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

-- | A webhook event with:
--
-- * the recipient (the Facebook page the message was sent to)
-- * the sender (a Page-Scoped ID of a user)
--
-- The sender is absent in a few cases (e.g. `Optin`s from a @Checkbox@ plugin callback).
--
-- The timestamp can also be absent (e.g. callbacks form the `Delivery` webhook).
data CallbackMessaging = CallbackMessaging
    { sender :: Maybe CallbackSender -- ^ the sending user's PSID
    , recipient :: CallbackRecipient -- ^ the receiving page ID
    , timestamp :: Maybe Integer -- ^ Time of callback event (epoch time in milliseconds)
    , priorMsg :: Maybe PriorMessage
    -- ^ Included in case it's the first message from a user
    -- after they received a message using the @user-ref@ from the checkbox plugin.
    , content :: CallbackContent -- ^ Content depending on the type of callback.
    } deriving (Eq, Show, Read)

-- | The different types of callbacks that could be
data CallbackContent =
    CMMessage Message -- ^ Regular message
  | CMDelivery Delivery -- ^ Delivery notification
  | CMRead ReadCallback -- ^ Message read notification
  | CMEcho Echo -- ^ Message sent on the connected page
  | CMPostback Postback -- ^ Postback callback
  | CMOptin Optin -- ^ Optin plugin callback
  | CMReferral Referral -- ^ Onboarding callback
  | CMPayment Payment -- ^ Payment callback
  | CMCheckoutUpdate CheckoutUpdate -- ^ Part of the `Payment` callback
  | CMPreCheckOut PreCheckout -- ^ Part of the `Payment` callback
  | CMAccountLink AccountLink -- ^ User linking with an external account
  | CMPolicy PolicyEnforcement -- ^ (Un\/)Block notification
  | CMAppRoles AppRoles -- ^ Part of thread control
  | CMPassThread PassThread -- ^ Part of thread control
  | CMTakeThread TakeThread -- ^ Part of thread control
  | CMMsgAccept -- ^ Customer Matching accepted
  deriving (Eq, Show, Read)


-- | The sender of the message. Sometimes this is absent because it wouldn't make sense with certain callbacks.
newtype CallbackSender =
          CallbackSender { senderId :: PSID }
  deriving (Eq, Show, Read, Ord)

-- | The receiving page of the callback.
newtype CallbackRecipient =
          CallbackRecipient { recipientId :: PageID } -- Recipient user ID/PAGE_ID
  deriving (Eq, Show, Read, Ord)

-- | Indicates the @user-ref@ this user is linked to
-- so you can link their PSID with the earlier used @user-ref@
--
-- (only uses "checkbox_plugin" at the moment)
newtype PriorMessage = PriorMessage { identifier :: Text }
  deriving (Eq, Show, Read, Ord)


-- --------------------- --
--  MESSAGING INSTANCES  --
-- --------------------- --

instance FromJSON CallbackMessaging where
  parseJSON = withObject "CallbackMessaging" $ \o ->
      CallbackMessaging <$> o .:? "sender"
                        <*> o .: "recipient"
                        <*> o .:? "timestamp"
                        <*> o .:? "prior_message"
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
      <|> tryMatch o
    where tryMessage o = do
              msg <- o .: "message"
              case "is_echo" `HM.lookup` msg of
                Just _ -> CMEcho <$> parseJSON (Object msg)
                _ -> CMMessage <$> parseJSON (Object msg)
          tryMatch o = do
              t <- o .: "message_request"
              if t == String "accept"
                then pure CMMsgAccept
                else fail "CallbackContent: \"message_request\" is not \"accept\""


-- ALL MESSAGING HAS THESE TWO --
--  (Except the OptinRef one)  --
instance FromJSON CallbackSender where
  parseJSON = withObject "CallbackSender" $ \o ->
    CallbackSender <$> o .: "id"

instance FromJSON CallbackRecipient where
  parseJSON = withObject "CallbackRecipient" $ \o ->
    CallbackRecipient <$> o .: "id"

instance FromJSON PriorMessage where
  parseJSON = withObject "PriorMessage" $ \o -> do
    source <- o .: "source"
    when (source /= String "checkbox_plugin") $
      fail "source is not \"checkbox_plugin\""
    PriorMessage <$> o .: "identifier"


instance ToJSON CallbackMessaging where
  toJSON (CallbackMessaging msender recpnt mtimestamp mpriorMsg cont) =
      object' $ [ "sender"    .=!! msender
                , "recipient" .=! recpnt
                , "timestamp" .=!! mtimestamp
                , "prior_message" .=!! mpriorMsg
                ] ++ contentPair
    where Object cbContentObj = toJSON cont
          contentPair = Just <$> HM.toList cbContentObj

instance ToJSON CallbackContent where
  toJSON = object . (:[]) . go
    where go = \case
                  CMMessage cb -> "message" .= cb
                  CMDelivery cb -> "delivery" .= cb
                  CMRead cb -> "read" .= cb
                  CMEcho cb -> "message" .= cb
                  CMPostback cb -> "postback" .= cb
                  CMOptin cb -> "optin" .= cb
                  CMReferral cb -> "referral" .= cb
                  CMPayment cb -> "payment" .= cb
                  CMCheckoutUpdate cb -> "checkout_update" .= cb
                  CMPreCheckOut cb -> "pre_checkout" .= cb
                  CMAccountLink cb -> "account_linking" .= cb
                  CMPolicy cb -> "policy-enforcement" .= cb
                  CMAppRoles cb -> "app_roles" .= cb
                  CMPassThread cb -> "pass_thread_control" .= cb
                  CMTakeThread cb -> "take_thread_control" .= cb
                  CMMsgAccept -> "message_request" .= String "accept"


-- ALL MESSAGING HAS THESE TWO --
--  (Except the OptinRef one)   -
instance ToJSON CallbackSender where
  toJSON (CallbackSender ident) = object [ "id" .= ident ]

instance ToJSON CallbackRecipient where
  toJSON (CallbackRecipient ident) = object [ "id" .= ident ]

instance ToJSON PriorMessage where
  toJSON (PriorMessage ident) =
      object [ "source" .= String "checkbox_plugin"
             , "identifier" .= ident
             ]
