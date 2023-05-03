{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Instances.Static where

import GHC.Generics

import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances()

import Web.Facebook.Messenger


------------
-- STATIC --
------------

deriving stock instance Generic PSID
instance Arbitrary PSID where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic PageID
instance Arbitrary PageID where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic AppId
instance Arbitrary AppId where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic SenderActionType
instance Arbitrary SenderActionType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic MessagingType
instance Arbitrary MessagingType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic NotificationType
instance Arbitrary NotificationType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic WebviewHeightRatioType
instance Arbitrary WebviewHeightRatioType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic AttachmentType
instance Arbitrary AttachmentType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic AirlineUpdateType
instance Arbitrary AirlineUpdateType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic ReferralSource
instance Arbitrary ReferralSource where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic ListStyle
instance Arbitrary ListStyle where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic PaymentType
instance Arbitrary PaymentType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic RequestedUserInfoType
instance Arbitrary RequestedUserInfoType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic MessageTag
instance Arbitrary MessageTag where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic AppRole
instance Arbitrary AppRole where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic AudienceType
instance Arbitrary AudienceType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic ImageAspectRatioType
instance Arbitrary ImageAspectRatioType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic WebviewShareType
instance Arbitrary WebviewShareType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic PriorMessageType
instance Arbitrary PriorMessageType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance Generic FBLocale
instance Arbitrary FBLocale where
  arbitrary = genericArbitrary
  shrink = genericShrink
