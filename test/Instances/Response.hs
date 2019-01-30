{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Instances.Response where

import GHC.Generics

import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances()

import Web.Facebook.Messenger.Types.Responses
import Instances.Request()
import Instances.Static()


deriving instance Generic MessageResponse
instance Arbitrary MessageResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic SenderActionResponse
instance Arbitrary SenderActionResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic SuccessResponse
instance Arbitrary SuccessResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic MessageCreativeResponse
instance Arbitrary MessageCreativeResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic BroadcastMessageResponse
instance Arbitrary BroadcastMessageResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic GetProfileResponse
instance Arbitrary GetProfileResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic ErrorResponse
instance Arbitrary ErrorResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic ErrorDetails
instance Arbitrary ErrorDetails where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AttachmentUploadResponse
instance Arbitrary AttachmentUploadResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic UserProfileResponse
instance Arbitrary UserProfileResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic MessengerCodeResponse
instance Arbitrary MessengerCodeResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AccountLinkingResponse
instance Arbitrary AccountLinkingResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- | Only taking 5 to speed up testing
deriving instance Generic CheckoutUpdateResponse
instance Arbitrary CheckoutUpdateResponse where
  arbitrary = CheckoutUpdateResponse <$> fmap (take 5) arbitrary
  shrink = genericShrink

deriving instance Generic Shipping
instance Arbitrary Shipping where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic ThreadControlResponse
instance Arbitrary ThreadControlResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic ThreadOwnerResponse
instance Arbitrary ThreadOwnerResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance {-# OVERLAPPABLE #-} Generic (DataResponse a)
instance {-# OVERLAPPABLE #-} (Arbitrary a, Generic a) => Arbitrary (DataResponse a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic DomainWhitelistingResponse
instance Arbitrary DomainWhitelistingResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic SecondaryReceiverResponse
instance Arbitrary SecondaryReceiverResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic SecondaryReceiverElement
instance Arbitrary SecondaryReceiverElement where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic TagResponse
instance Arbitrary TagResponse where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic TagElement
instance Arbitrary TagElement where
  arbitrary = genericArbitrary
  shrink = genericShrink
