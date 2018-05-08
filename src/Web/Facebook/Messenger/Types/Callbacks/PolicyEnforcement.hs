{-|
Module      : Web.Facebook.Messenger.Types.Callbacks.PolicyEnforcement
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

An app will receive this callback when a policy enforcement action is taken on the page it manages.
You can subscribe to this callback by selecting the @"messaging_policy_enforcement"@ field when setting up your webhook.

A policy enforcement will be taken on a page if it does not conform to Messenger Platform policy,
fails to meet Facebook community standards or violates Facebook Pages guidelines.
Common issues include spams, sending inappropriate messages (porn, suicide, etc), abusing tags, etc.

https://developers.facebook.com/docs/messenger-platform/reference/webhook-events/messaging_policy_enforcement
-}
module Web.Facebook.Messenger.Types.Callbacks.PolicyEnforcement
  ( PolicyEnforcement (..) )
where


import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text


-- | This callback is sent when a policy enforcement action is taken on the page it manages
data PolicyEnforcement = Block { peReason :: Text }
                       | Unblock
  deriving (Eq, Show, Read, Ord)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON PolicyEnforcement where
  toJSON Unblock = object ["action" .= String "unblock"]
  toJSON (Block reason) =
      object [ "action" .= String "block"
             , "reason" .= reason
             ]

instance FromJSON PolicyEnforcement where
  parseJSON = withObject "PolicyEnforcement" $ \o -> do
      action <- o .: "action" :: Parser Text
      case action of
        "unblock" -> pure Unblock
        "block" -> Block <$> o .: "reason"
        wat -> fail $ mappend "PolicyEnforcement: wrong string in \"action\" field: " $ show wat
