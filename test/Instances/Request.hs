{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Instances.Request where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes, isNothing, listToMaybe)
import GHC.Generics

import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances()

import Web.Facebook.Messenger.Types.Requests
import Instances.Static()


limitNEList :: Int -> NonEmpty a -> NonEmpty a
limitNEList i (e :| es) = e :| take (i - 1) es

--------------
-- Requests --
--------------

deriving instance Generic SendRequest
instance Arbitrary SendRequest where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic SenderActionRequest
instance Arbitrary SenderActionRequest where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RequestRecipient
instance Arbitrary RequestRecipient where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RecipientID
instance Arbitrary RecipientID where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RecipientPhone
instance Arbitrary RecipientPhone where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RecipientRef
instance Arbitrary RecipientRef where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AttachmentUploadRequest
instance Arbitrary AttachmentUploadRequest where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AccountUnlinkRequest
instance Arbitrary AccountUnlinkRequest where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic MessengerCodeRequest
instance Arbitrary MessengerCodeRequest where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic MessengerCodeRef
instance Arbitrary MessengerCodeRef where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic PassThreadControlRequest
instance Arbitrary PassThreadControlRequest where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic TakeThreadControlRequest
instance Arbitrary TakeThreadControlRequest where
  arbitrary = genericArbitrary
  shrink = genericShrink

-----------------------
-- Requests Settings --
-----------------------

deriving instance Generic Greeting
instance Arbitrary Greeting where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic GreetingSetting
instance Arbitrary GreetingSetting where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic GetStartedButton
instance Arbitrary GetStartedButton where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic PersistentMenu
instance Arbitrary PersistentMenu where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic PersistentMenuSetting
instance Arbitrary PersistentMenuSetting where
  arbitrary = PersistentMenuSetting <$> arbitrary
                                    <*> arbitrary
                                    <*> fmap (take 3) arbitrary
  shrink = genericShrink

deriving instance Generic PersistentMenuItem
instance Arbitrary PersistentMenuItem where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic PersistentMenuItemNested
instance Arbitrary PersistentMenuItemNested where
  arbitrary = PersistentMenuItemNested <$> arbitrary
                                       <*> pure []
  shrink = genericShrink

deriving instance Generic WhiteListedDomains
instance Arbitrary WhiteListedDomains where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AccountLinkingUrl
instance Arbitrary AccountLinkingUrl where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic PaymentSettings
instance Arbitrary PaymentSettings where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic TargetAudience
instance Arbitrary TargetAudience where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic TargetCountries
instance Arbitrary TargetCountries where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic HomeUrl
instance Arbitrary HomeUrl where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic ProfileRequest
instance Arbitrary ProfileRequest where
  arbitrary = genericArbitrary
  shrink = genericShrink


--------------------
-- Requests Extra --
--------------------

deriving instance Generic TemplateButton
instance Arbitrary TemplateButton where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic URLButton
instance Arbitrary URLButton where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic PostbackButton
instance Arbitrary PostbackButton where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic CallButton
instance Arbitrary CallButton where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic LogInButton
instance Arbitrary LogInButton where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic LogOutButton
instance Arbitrary LogOutButton where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- YOLO: Let's not infinitely nest GenericElements through these buttons...
deriving instance Generic ShareButton
instance Arbitrary ShareButton where
  arbitrary = pure $ ShareButton Nothing
  shrink = genericShrink

deriving instance Generic ShareContents
instance Arbitrary ShareContents where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic BuyButton
instance Arbitrary BuyButton where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic PriceObject
instance Arbitrary PriceObject where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic GenericElement
instance Arbitrary GenericElement where
  arbitrary = genericElement <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary
    where genericElement t st url da btns =
            let (bs1, bs2) = unzip (getBtns btns)
                (bb, rest') = (catMaybes bs1, catMaybes bs2)
                buy = listToMaybe bb
                takeAmount = if isNothing buy then 3 else 2
                rest = take takeAmount rest'
            in GenericElement t st url da buy rest
          getBtns [] = []
          getBtns (TBuy bb:rest) = (Just bb,Nothing) : getBtns rest
          getBtns (btn:rest) = (Nothing,Just btn) : getBtns rest
  shrink = genericShrink

deriving instance Generic DefaultAction
instance Arbitrary DefaultAction where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic TemplateAddress
instance Arbitrary TemplateAddress where
  arbitrary = genericArbitrary
  shrink = genericShrink


----------------------
-- Requests Message --
----------------------

deriving instance Generic RequestMessage
instance Arbitrary RequestMessage where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RequestMessageText
instance Arbitrary RequestMessageText where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RequestMessageAttachment
instance Arbitrary RequestMessageAttachment where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RequestQuickReply
instance Arbitrary RequestQuickReply where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RQuickReply
instance Arbitrary RQuickReply where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic LocationQuickReply
instance Arbitrary LocationQuickReply where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic PhoneNumberQuickReply
instance Arbitrary PhoneNumberQuickReply where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic EmailQuickReply
instance Arbitrary EmailQuickReply where
  arbitrary = genericArbitrary
  shrink = genericShrink


--------------------------
-- Requests Attachments --
--------------------------

deriving instance Generic RequestAttachment
instance Arbitrary RequestAttachment where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RequestMultimediaAttachment
instance Arbitrary RequestMultimediaAttachment where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RequestAttachmentTemplate
instance Arbitrary RequestAttachmentTemplate where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RequestMultimediaPayload
instance Arbitrary RequestMultimediaPayload where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RMultimediaPayload
instance Arbitrary RMultimediaPayload where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic RReusedMultimediaPayload
instance Arbitrary RReusedMultimediaPayload where
  arbitrary = genericArbitrary
  shrink = genericShrink


--------------------------
-- Requests Attachments --
--------------------------

deriving instance Generic TemplatePayload
instance Arbitrary TemplatePayload where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AirlineAirport
instance Arbitrary AirlineAirport where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AirlineFlightInfo
instance Arbitrary AirlineFlightInfo where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AirlineFlightSchedule
instance Arbitrary AirlineFlightSchedule where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AirlineBoardingPass
instance Arbitrary AirlineBoardingPass where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic BoardingPass
instance Arbitrary BoardingPass where
  arbitrary = BoardingPass <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> fmap (take 5) arbitrary
                           <*> fmap (take 5) arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
  shrink = genericShrink

deriving instance Generic AirlineQRBarCode
instance Arbitrary AirlineQRBarCode where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AirlineQRCode
instance Arbitrary AirlineQRCode where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AirlineBarCode
instance Arbitrary AirlineBarCode where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AirlineField
instance Arbitrary AirlineField where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AirlineCheckin
instance Arbitrary AirlineCheckin where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AirlineCheckinFlightInfo
instance Arbitrary AirlineCheckinFlightInfo where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AirlineCheckinFlightSchedule
instance Arbitrary AirlineCheckinFlightSchedule where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AirlineFlightUpdate
instance Arbitrary AirlineFlightUpdate where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic AirlineItinerary
instance Arbitrary AirlineItinerary where
  arbitrary = AirlineItinerary <$> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> fmap (take 4) arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
                               <*> arbitrary
  shrink = genericShrink

deriving instance Generic PassengerInfo
instance Arbitrary PassengerInfo where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic ItineraryFlightInfo
instance Arbitrary ItineraryFlightInfo where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic PassengerSegmentInfo
instance Arbitrary PassengerSegmentInfo where
  arbitrary = PassengerSegmentInfo <$> arbitrary
                                   <*> arbitrary
                                   <*> arbitrary
                                   <*> arbitrary
                                   <*> fmap (take 4) arbitrary
  shrink = genericShrink

deriving instance Generic AirlineProductInfo
instance Arbitrary AirlineProductInfo where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic PriceInfo
instance Arbitrary PriceInfo where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic ButtonTemplate
instance Arbitrary ButtonTemplate where
  arbitrary = ButtonTemplate <$> arbitrary
                             <*> fmap (limitNEList 3) arbitrary
  shrink = genericShrink

deriving instance Generic GenericTemplate
instance Arbitrary GenericTemplate where
  arbitrary = GenericTemplate <$> arbitrary
                              <*> arbitrary
                              <*> fmap (limitNEList 10) arbitrary
  shrink = genericShrink

deriving instance Generic ListTemplate
instance Arbitrary ListTemplate where
  arbitrary = go <$> arbitrary
                 <*> fmap (limitNEList 4) arbitrary
                 <*> arbitrary
    where go style es' = ListTemplate style es
            where es = case es' of
                          (e :| []) -> e :| [e]
                          _ -> es'
  shrink = genericShrink

deriving instance Generic ListElement
instance Arbitrary ListElement where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic MediaTemplate
instance Arbitrary MediaTemplate where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic MediaElement
instance Arbitrary MediaElement where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic MediaElementContent
instance Arbitrary MediaElementContent where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic OpenGraphTemplate
instance Arbitrary OpenGraphTemplate where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic OpenGraphElement
instance Arbitrary OpenGraphElement where
  arbitrary = OpenGraphElement <$> arbitrary
                               <*> fmap (take 3) arbitrary
  shrink = genericShrink

deriving instance Generic ReceiptTemplate
instance Arbitrary ReceiptTemplate where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic ReceiptElement
instance Arbitrary ReceiptElement where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic ReceiptSummary
instance Arbitrary ReceiptSummary where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic ReceiptAdjustment
instance Arbitrary ReceiptAdjustment where
  arbitrary = genericArbitrary
  shrink = genericShrink
