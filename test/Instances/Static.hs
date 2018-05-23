{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Instances.Static where

import GHC.Generics

import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances()

import Web.Facebook.Messenger


------------
-- STATIC --
------------

deriving instance Generic PSID
instance Arbitrary PSID where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic PageID
instance Arbitrary PageID where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AppId
instance Arbitrary AppId where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic SenderActionType
instance Arbitrary SenderActionType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic MessagingType
instance Arbitrary MessagingType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic NotificationType
instance Arbitrary NotificationType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic WebviewHeightRatioType
instance Arbitrary WebviewHeightRatioType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AttachmentType
instance Arbitrary AttachmentType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AirlineUpdateType
instance Arbitrary AirlineUpdateType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic ReferralSource
instance Arbitrary ReferralSource where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic ListStyle
instance Arbitrary ListStyle where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic PaymentType
instance Arbitrary PaymentType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RequestedUserInfoType
instance Arbitrary RequestedUserInfoType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic MessageTag
instance Arbitrary MessageTag where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AppRole
instance Arbitrary AppRole where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AudienceType
instance Arbitrary AudienceType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic ImageAspectRatioType
instance Arbitrary ImageAspectRatioType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic WebviewShareType
instance Arbitrary WebviewShareType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic PriorMessageType
instance Arbitrary PriorMessageType where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic FBLocale
instance Arbitrary FBLocale where
  arbitrary = genericArbitrary
  shrink = genericShrink
