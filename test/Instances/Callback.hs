{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Instances.Callback where


import GHC.Generics

import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances()

import Instances.Request
import Instances.Static()
import Web.Facebook.Messenger


-- YOLO: ... just 3 (takes way too long otherwise)
-- and we already test everything else individually
deriving stock instance Generic Callback
instance Arbitrary Callback where
  arbitrary = Callback <$> fmap (take 3) arbitrary
  shrink = genericShrink

deriving stock instance Generic CallbackEntry
instance Arbitrary CallbackEntry where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic Message
instance Arbitrary Message where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic MessageContent
instance Arbitrary MessageContent where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic MessageText
instance Arbitrary MessageText where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic CallbackQuickReply
instance Arbitrary CallbackQuickReply where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- Only taking 5 to speed up testing
deriving stock instance Generic MessageAttachment
instance Arbitrary MessageAttachment where
  arbitrary = MessageAttachment . take 5 . getNonEmpty <$> arbitrary
  shrink = genericShrink

deriving stock instance Generic CallbackAttachment
instance Arbitrary CallbackAttachment where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic MultimediaAttachment
instance Arbitrary MultimediaAttachment where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic CallbackMultimediaPayload
instance Arbitrary CallbackMultimediaPayload where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic TemplateAttachment
instance Arbitrary TemplateAttachment where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- Taking only 5 for speed
deriving stock instance Generic CallbackTemplate
instance Arbitrary CallbackTemplate where
  arbitrary = CallbackTemplate <$> arbitrary
                               <*> fmap (take 5) arbitrary
  shrink = genericShrink

deriving stock instance Generic MessageSticker
instance Arbitrary MessageSticker where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic StickerAttachment
instance Arbitrary StickerAttachment where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic CallbackStickerPayload
instance Arbitrary CallbackStickerPayload where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- Only taking 5 to speed up testing
deriving stock instance Generic MessageLocation
instance Arbitrary MessageLocation where
  arbitrary = MessageLocation . take 5 . getNonEmpty <$> arbitrary
  shrink = genericShrink

deriving stock instance Generic CallbackLocation
instance Arbitrary CallbackLocation where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic CallbackLocationPayload
instance Arbitrary CallbackLocationPayload where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic CallbackCoordinates
instance Arbitrary CallbackCoordinates where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic CallbackMessaging
instance Arbitrary CallbackMessaging where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic CallbackContent
instance Arbitrary CallbackContent where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic CallbackSender
instance Arbitrary CallbackSender where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic CallbackRecipient
instance Arbitrary CallbackRecipient where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic PriorMessage
instance Arbitrary PriorMessage where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic AccountLink
instance Arbitrary AccountLink where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic AppRoles
instance Arbitrary AppRoles where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic CheckoutUpdate
instance Arbitrary CheckoutUpdate where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic Delivery
instance Arbitrary Delivery where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic Echo
instance Arbitrary Echo where
  arbitrary = Echo <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
  shrink = genericShrink

deriving stock instance Generic EchoContent
instance Arbitrary EchoContent where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic EchoText
instance Arbitrary EchoText where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- Also limited to 5 for speed
deriving stock instance Generic EchoAttachment
instance Arbitrary EchoAttachment where
  arbitrary = EchoAttachment . take 5 . getNonEmpty <$> arbitrary
  shrink = genericShrink

-- Also limited to 5 for speed
deriving stock instance Generic EchoButton
instance Arbitrary EchoButton where
  arbitrary = f <$> arbitrary <*> arbitrary
    where f t = EchoButton t . limitNEList 5
  shrink = genericShrink

deriving stock instance Generic EchoFallback
instance Arbitrary EchoFallback where
  arbitrary = f <$> arbitrary <*> arbitrary
    where f x = EchoFallback x . getNonEmpty
  shrink = genericShrink

deriving stock instance Generic Fallback
instance Arbitrary Fallback where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic Optin
instance Arbitrary Optin where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic PassThread
instance Arbitrary PassThread where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic RequestThread
instance Arbitrary RequestThread where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic Payment
instance Arbitrary Payment where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic Amount
instance Arbitrary Amount where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic RequestedUserInfo
instance Arbitrary RequestedUserInfo where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic PaymentCredential
instance Arbitrary PaymentCredential where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic PaymentToken
instance Arbitrary PaymentToken where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic PaymentPayPal
instance Arbitrary PaymentPayPal where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic PaymentStripe
instance Arbitrary PaymentStripe where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic DecryptedPaymentResult
instance Arbitrary DecryptedPaymentResult where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic PolicyEnforcement
instance Arbitrary PolicyEnforcement where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic Postback
instance Arbitrary Postback where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic RegularPostback
instance Arbitrary RegularPostback where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic SecondaryPostback
instance Arbitrary SecondaryPostback where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic PreCheckout
instance Arbitrary PreCheckout where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic ReadCallback
instance Arbitrary ReadCallback where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic Referral
instance Arbitrary Referral where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic RefShortLink
instance Arbitrary RefShortLink where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic RefAds
instance Arbitrary RefAds where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic RefMessengerCode
instance Arbitrary RefMessengerCode where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic RefChatPlugin
instance Arbitrary RefChatPlugin where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic TakeThread
instance Arbitrary TakeThread where
  arbitrary = genericArbitrary
  shrink = genericShrink
