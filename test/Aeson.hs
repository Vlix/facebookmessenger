{-# LANGUAGE TemplateHaskell #-}
module Aeson where


import Util.Testing.Aeson
import Test.Tasty as Tasty
import Web.Facebook.Messenger

import Instances()

staticTests :: TestTree
staticTests = Tasty.testGroup "Static"
    $(mkJSONTestTrees [ ''MessagingType
                      , ''NotificationType
                      , ''SenderActionType
                      , ''MessageTag
                      , ''WebviewHeightRatioType
                      , ''WebviewShareType
                      , ''ListStyle
                      , ''ImageAspectRatioType
                      , ''AirlineUpdateType
                      , ''AttachmentType
                      , ''ReferralSource
                      , ''PaymentType
                      , ''RequestedUserInfoType
                      , ''AppRole
                      , ''AudienceType
                      , ''PriorMessageType
                      ])

requestTests :: TestTree
requestTests = Tasty.testGroup "Request"
                      -- Extra
    $(mkJSONTestTrees [ ''PriceObject
                      , ''GenericElement
                      , ''TemplateAddress
                      , ''DefaultAction
                      , ''URLButton
                      , ''PostbackButton
                      , ''CallButton
                      , ''LogInButton
                      , ''LogOutButton
                      , ''ShareContents
                      , ''ShareButton
                      , ''BuyButton
                      , ''TemplateButton

                      -- Settings
                      , ''GreetingSetting
                      , ''Greeting
                      , ''GetStartedButton
                      , ''PersistentMenuItemNested
                      , ''PersistentMenuItem
                      , ''PersistentMenuSetting
                      , ''PersistentMenu
                      , ''WhiteListedDomains
                      , ''AccountLinkingUrl
                      , ''PaymentSettings
                      , ''TargetCountries
                      , ''TargetAudience
                      , ''HomeUrl
                      , ''ProfileRequest

                      -- Templates
                      , ''ReceiptElement
                      , ''ReceiptSummary
                      , ''ReceiptAdjustment
                      , ''ReceiptTemplate
                      , ''OpenGraphElement
                      , ''OpenGraphTemplate
                      , ''MediaElement
                      , ''MediaTemplate
                      , ''ListElement
                      , ''ListTemplate
                      , ''GenericTemplate
                      , ''ButtonTemplate
                      , ''AirlineAirport
                      , ''AirlineFlightSchedule
                      , ''AirlineFlightInfo
                      , ''AirlineFlightUpdate
                      , ''AirlineCheckinFlightSchedule
                      , ''AirlineCheckinFlightInfo
                      , ''AirlineCheckin
                      , ''PassengerInfo
                      , ''ItineraryFlightInfo
                      , ''AirlineProductInfo
                      , ''PassengerSegmentInfo
                      , ''PriceInfo
                      , ''AirlineItinerary
                      , ''AirlineQRCode
                      , ''AirlineBarCode
                      , ''AirlineField
                      , ''BoardingPass
                      , ''AirlineBoardingPass
                      , ''TemplatePayload

                      -- Attachments
                      , ''RReusedMultimediaPayload
                      , ''RMultimediaPayload
                      , ''RequestMultimediaPayload
                      , ''RequestAttachmentTemplate
                      , ''RequestMultimediaAttachment
                      , ''RequestAttachment

                      -- Messages
                      , ''EmailQuickReply
                      , ''PhoneNumberQuickReply
                      , ''LocationQuickReply
                      , ''RQuickReply
                      , ''RequestQuickReply
                      , ''RequestMessageAttachment
                      , ''RequestMessageText
                      , ''RequestMessage
                      , ''ThreadControlRequest
                      , ''PassThreadControlRequest
                      , ''MessengerCodeRef
                      , ''MessengerCodeRequest
                      , ''AccountUnlinkRequest
                      , ''AttachmentUploadRequest
                      , ''RecipientName
                      , ''RecipientPhone
                      , ''RecipientRef
                      , ''RecipientID
                      , ''RequestRecipient
                      , ''SenderActionRequest
                      , ''SendRequest
                      ])

responseTests :: TestTree
responseTests = Tasty.testGroup "Response"
    $(mkJSONTestTrees [ ''AppId
                      , ''PageID
                      , ''PSID
                      , ''MessageResponse
                      , ''SenderActionResponse
                      , ''SuccessResponse
                      , ''GetProfileResponse
                      , ''ErrorDetails
                      , ''ErrorResponse
                      , ''AttachmentUploadResponse
                      , ''UserProfileResponse
                      , ''MessengerCodeResponse
                      , ''AccountLinkingResponse
                      , ''Shipping
                      , ''CheckoutUpdateResponse
                      , ''ThreadControlResponse
                      , ''DomainWhitelistingResponse
                      , ''SecondaryReceiverElement
                      , ''SecondaryReceiverResponse
                      , ''TagElement
                      , ''TagResponse
                      ])

callbackTests :: TestTree
callbackTests = Tasty.testGroup "Callback"
                      -- Thread control
    $(mkJSONTestTrees [ ''TakeThread
                      , ''PassThread

                      -- Payments
                      , ''PreCheckout
                      , ''DecryptedPaymentResult
                      , ''PaymentStripe
                      , ''PaymentPayPal
                      , ''PaymentToken
                      , ''PaymentCredential
                      , ''RequestedUserInfo
                      , ''Amount
                      , ''Payment

                      -- Postback
                      , ''RegularPostback
                      , ''SecondaryPostback
                      , ''Postback

                      -- Other
                      , ''PolicyEnforcement
                      , ''Referral
                      , ''ReadCallback
                      , ''Optin
                      , ''Delivery
                      , ''CheckoutUpdate
                      , ''AppRoles
                      , ''AccountLink

                      -- Echo
                      , ''Fallback
                      , ''EchoFallback
                      , ''EchoAttachment
                      , ''EchoText
                      , ''Echo

                      -- Messaging
                      , ''PriorMessage
                      , ''CallbackRecipient
                      , ''CallbackSender
                      , ''CallbackContent
                      , ''CallbackMessaging

                      -- Message
                      , ''CallbackCoordinates
                      , ''CallbackLocationPayload
                      , ''CallbackLocation
                      , ''MessageLocation
                      , ''CallbackStickerPayload
                      , ''StickerAttachment
                      , ''MessageSticker
                      , ''CallbackMultimediaPayload
                      , ''MultimediaAttachment
                      , ''CallbackTemplate
                      , ''TemplateAttachment
                      , ''CallbackAttachment
                      , ''MessageAttachment
                      , ''CallbackQuickReply
                      , ''MessageText
                      , ''MessageContent
                      , ''Message
                      , ''CallbackEntry
                      , ''Callback
                      ])

