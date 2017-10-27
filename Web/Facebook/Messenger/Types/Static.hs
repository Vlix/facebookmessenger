{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Web.Facebook.Messenger.Types.Static
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This module contains the following:

* Basic sum types without parameters in the FB Messenger API. (e.g. `SenderActionType`: @"mark_seen"@, @"typing_off"@ and @"typing_on"@)
* Helper functions for `FromJSON` \/ `ToJSON` instance declarations
* Type synonyms and newtypes
-}
module Web.Facebook.Messenger.Types.Static (
  -- * Sum Types

  -- ** Send API
  NotificationType (..)
  , SenderActionType (..)
  , MessageTag (..)
  , WebviewHeightRatioType (..)
  , WebviewShareType (..)
  , ListStyle (..)
  , ImageAspectRatioType (..)
  , AirlineUpdateType (..)
  -- ** Send API & Callbacks
  , AttachmentType (..)
  -- ** Other
  , ReferralSource (..)
  , PaymentType (..)
  , RequestedUserInfoType (..)
  , AppRole (..)
  , AudienceType (..)
  
  -- * Helper functions
  --
  -- | These functions are mostly convenience functions to make @JSON@ as short as possible
  -- and to make writing certain `FromJSON` instances less painful.

  -- ** ToJSON functions
  , object'
  , (.=!)
  , (.=!!)
  , mDefault
  , mEmptyList
  -- ** FromJSON functions
  , checkValue
  , withText'

  -- * Type synonyms\/newtypes
  , URL
  , AppId (..)
  , PSID (..)
  , PageID (..)
  )
where


import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (unpack, Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.HashMap.Strict as HM

-- | Text which should be formatted as a valid URL (e.g. @"https://www.example.com"@)
type URL = Text

-- | When representing a user, these IDs are page-scoped IDs (PSID).
-- This means that the IDs of users are unique for a given page.
newtype PSID = PSID Text deriving (Eq, Show, FromJSON, ToJSON)

-- | Pages have their own unique ID
newtype PageID = PageID Text deriving (Eq, Show, FromJSON, ToJSON)

-- | Newtype wrapper around Text, because the AppId is very different than anything else used as IDs in this package
newtype AppId = AppId Text deriving (Eq, Show, FromJSON, ToJSON)


-- | Helper function to avoid @`Maybe` [a]@ when an empty list doesn't have to (or shouldn't) be included in the @JSON@
mEmptyList :: ToJSON a => Text -> [a] -> Maybe Pair
mEmptyList _ [] = Nothing
mEmptyList t l  = Just $ t .= l

-- | Helper function to not include values that are the default when not included in the @JSON@
mDefault :: (Eq a, ToJSON a) => Text -> a -> a -> Maybe Pair
mDefault t a b = if a == b then Nothing else Just $ t .= b

-- | Alternative to "Aeson"'s `object` to make a @JSON object@ that might omit certain fields.
-- `Just` `Pair` will be included in the @JSON object@. `Nothing` will not.
--
-- @
-- `object'` [ "type" `.=!` `Data.Aeson.String` "image"
--         , "url" `.=!` url
--         , "title" `.=!!` mTitle
--         , `mEmptyList` "elements" elements
--         , `mDefault` "notification_type" `REGULAR` notifType 
--         ]
-- @
--
-- The above will result in the following in case @mTitle@ is `Nothing`, @elements@ is @[]@ and @notifType@ is `REGULAR`:
--
-- @
-- {
--   "type": "image",
--   "url": "http:\/\/www.example.com\/image.jpg"
-- }
-- @
--
-- Compaired to when using the regular "Aeson"'s `object`:
--
-- @
-- {
--   "type": "image",
--   "url": "http:\/\/www.example.com\/image.jpg",
--   "title": null,
--   "elements": [],
--   "notification_type": "regular"
-- }
-- @
object' :: [Maybe Pair] -> Value
object' = Object . HM.fromList . catMaybes

-- | @a `.=!!` b@ will omit the specified `Pair` in case @b@ is `Nothing`
(.=!!) :: ToJSON a => Text -> Maybe a -> Maybe Pair
(.=!!) name = fmap (name .=)

-- | Add a required `Pair` to the @JSON object@
(.=!) :: ToJSON a => Text -> a -> Maybe Pair
(.=!) name = Just . (name .=)

-- | This function checks to see if a `Value` is an `Object` and the proceeds to check
-- if a certain field has a certain value before continuing parsing the object.
-- (e.g. checking if @"type"@ is actually @"image"@ or not)
checkValue :: (FromJSON a, ToJSON a, Eq a)
           => String -- ^ /reference in case the parsing fails/
           -> Text -- ^ /field name to check/
           -> a -- ^ /value to check in that field/
           -> (Object -> Parser b) -- ^ /parser to run in case the field check succeeds/
           -> Value
           -> Parser b
checkValue fName field value f = withObject fName $ \o -> do
    typ <- o .: field
    unless (typ == value) $
     fail $ fName <> ": wrong " <> show field <> " value: " <> showJson typ
    f o
  where showJson = unpack . decodeUtf8 . toStrict . encode

-- | Shortcut function for parsing certain sum types.
--
-- @
-- instance `FromJSON` `SenderActionType` where
--   `parseJSON` = `withText'` \"SenderActionType\"
--       [("mark_seen", `MARK_SEEN`)
--       ,("typing_on", `TYPING_ON`)
--       ,("typing_off", `TYPING_OFF`)
--       ]
-- @
withText' :: String -- ^ /reference in case the parsing fails/
          -> [(Text, a)] -- ^ /lookup list of JSON String to sum type/
          -> Value
          -> Parser a
withText' s tups = withText s $ \t ->
    case lookup t tups of
      Just val -> pure val
      _ -> fail $ "Wrong String for " <> s <> ": " <> unpack t


-- | Set typing indicators or send read receipts to let users know you are processing their request.
--
-- /Typing indicators are automatically turned off after 20 seconds/
data SenderActionType =
    MARK_SEEN -- ^ Mark last message as read
  | TYPING_ON -- ^ Turn typing indicators on
  | TYPING_OFF -- ^ Turn typing indicators off
  deriving (Eq, Show, Read, Ord)

instance ToJSON SenderActionType where
  toJSON MARK_SEEN = String "mark_seen"
  toJSON TYPING_ON = String "typing_on"
  toJSON TYPING_OFF = String "typing_off"

instance FromJSON SenderActionType where
  parseJSON = withText' "SenderActionType"
      [("mark_seen", MARK_SEEN)
      ,("typing_on", TYPING_ON)
      ,("typing_off", TYPING_OFF)
      ]

-- | Push notification type
data NotificationType =
    REGULAR -- ^ sound/vibration and a phone notification
  | SILENT_PUSH -- ^ on-screen notification only
  | NO_PUSH -- ^ no notification
  deriving (Eq, Show, Read, Ord)

instance ToJSON NotificationType where
  toJSON REGULAR = String "REGULAR"
  toJSON SILENT_PUSH = String "SILENT_PUSH"
  toJSON NO_PUSH = String "NO_PUSH"

instance FromJSON NotificationType where
  parseJSON = withText' "NotificationType"
      [("REGULAR", REGULAR)
      ,("SILENT_PUSH", SILENT_PUSH)
      ,("NO_PUSH", NO_PUSH)
      ]

-- | Height of the Webview
data WebviewHeightRatioType =
    COMPACT -- ^ 50% of screen
  | TALL -- ^ 75% of screen
  | FULL -- ^ full screen
  deriving (Eq, Show, Read, Ord)

instance ToJSON WebviewHeightRatioType where
  toJSON COMPACT = String "compact"
  toJSON TALL = String "tall"
  toJSON FULL = String "full"

instance FromJSON WebviewHeightRatioType where
  parseJSON = withText' "WebviewHeightRatioType"
      [("compact", COMPACT)
      ,("tall", TALL)
      ,("full", FULL)
      ]

-- | Type of the attachment sent or received
data AttachmentType =
    IMAGE -- ^ Image type (should be @jpg@, @png@ or @gif@)
  | VIDEO -- ^ Video type (should be @mp4@?)
  | AUDIO -- ^ Audio type (should be @mp3@?)
  | FILE -- ^ File type (any plain file)
  deriving (Eq, Show, Read, Ord)

instance ToJSON AttachmentType where
  toJSON IMAGE = String "image"
  toJSON VIDEO = String "video"
  toJSON AUDIO = String "audio"
  toJSON FILE = String "file"

instance FromJSON AttachmentType where
  parseJSON = withText' "AttachmentType"
      [("image", IMAGE)
      ,("audio", AUDIO)
      ,("video", VIDEO)
      ,("file", FILE)
      ]

-- | Type of update for the @Flight Update@ template
data AirlineUpdateType =
    DELAY
  | GATE_CHANGE
  | CANCELLATION
  deriving (Eq, Show, Read, Ord)

instance ToJSON AirlineUpdateType where
  toJSON DELAY = String "delay"
  toJSON GATE_CHANGE = String "gate_change"
  toJSON CANCELLATION = String "cancellation"

instance FromJSON AirlineUpdateType where
  parseJSON = withText' "AirlineUpdateType"
      [("delay", DELAY)
      ,("gate_change", GATE_CHANGE)
      ,("cancellation", CANCELLATION)
      ]

-- | Indication from where this user was referred from
data ReferralSource =
    SHORTLINK -- ^ @m.me@ link
  | ADS -- ^ Facebook Ad
  | MESSENGER_CODE -- ^ Scanning of a Parametric Messenger Code
  | DISCOVER_TAB -- ^ Facebook Discover Tab
  deriving (Eq, Show, Read, Ord)

instance ToJSON ReferralSource where
  toJSON SHORTLINK = String "SHORTLINK"
  toJSON ADS = String "ADS"
  toJSON MESSENGER_CODE = String "MESSENGER_CODE"
  toJSON DISCOVER_TAB = String "DISCOVER_TAB"

instance FromJSON ReferralSource where
  parseJSON = withText' "ReferralSource"
      [("SHORTLINK", SHORTLINK)
      ,("ADS", ADS)
      ,("MESSENGER_CODE", MESSENGER_CODE)
      ,("DISCOVER_TAB", DISCOVER_TAB)
      ]

-- | Type of list to produce
data ListStyle =
    ListCOMPACT -- ^ All items are the same with an optional image on the right side
  | ListLARGE -- ^ Top item is more prominent and requires an image as the background of that item
  deriving (Eq, Show, Read, Ord)

instance ToJSON ListStyle where
  toJSON ListCOMPACT = String "compact"
  toJSON ListLARGE = String "large"

instance FromJSON ListStyle where
  parseJSON = withText' "ListStyle"
      [("compact", ListCOMPACT)
      ,("large", ListLARGE)
      ]

-- | The Buy Button supports fixed and flexible pricing.
-- Flexible pricing can be used when you modify pricing based on shipping.
-- When flexible pricing is declared, the Checkout dialog will render a button that the person can tap
-- to choose the shipping method. We call your webhook to get information about the shipping names and prices.
data PaymentType =
    FIXED_AMOUNT
  | FLEXIBLE_AMOUNT
  deriving (Eq, Show, Read, Ord)

instance ToJSON PaymentType where
  toJSON FIXED_AMOUNT = String "FIXED_AMOUNT"
  toJSON FLEXIBLE_AMOUNT = String "FLEXIBLE_AMOUNT"

instance FromJSON PaymentType where
  parseJSON = withText' "PaymentType"
      [("FIXED_AMOUNT", FIXED_AMOUNT)
      ,("FLEXIBLE_AMOUNT", FLEXIBLE_AMOUNT)
      ]

-- | Used in the Buy Button
--
-- Information requested from person that will render in the dialog. 
data RequestedUserInfoType =
    SHIPPING_ADDRESS -- ^ Address to send item(s) to
  | CONTACT_NAME -- ^ Name of contact
  | CONTACT_PHONE -- ^ Phone number of contact
  | CONTACT_EMAIL -- ^ Email address of contaxt
  deriving (Eq, Show, Read, Ord)

instance ToJSON RequestedUserInfoType where
  toJSON SHIPPING_ADDRESS = String "shipping_address"
  toJSON CONTACT_NAME = String "contact_name"
  toJSON CONTACT_PHONE = String "contact_phone"
  toJSON CONTACT_EMAIL = String "contact_email"

instance FromJSON RequestedUserInfoType where
  parseJSON = withText' "RequestedUserInfoType"
      [("shipping_address", SHIPPING_ADDRESS)
      ,("contact_name", CONTACT_NAME)
      ,("contact_phone", CONTACT_PHONE)
      ,("contact_email", CONTACT_EMAIL)
      ]

-- | Message tags give you the ability to send messages to a person outside of
-- the normally allowed 24-hour window for a limited number of purposes that require continual notification or updates.
-- This enables greater flexibility in how your app interacts with people,
-- as well as the types of experiences you can build on the Messenger Platform.
--
-- Please note that message tags are for sending non-promotional content only.
-- Using tags to send promotional content (ex: daily deals, coupons and discounts,
-- or sale announcements) is against Messenger Platform policy.
--
-- https://developers.facebook.com/docs/messenger-platform/send-messages/message-tags
data MessageTag =
    ACCOUNT_UPDATE -- ^ Notify the message recipient of a change to their account settings.
  | PAYMENT_UPDATE -- ^ Notify the message recipient of a payment update for an existing transaction.
  | PERSONAL_FINANCE_UPDATE -- ^ Confirm a message recipient's financial activity.
  | SHIPPING_UPDATE -- ^ Notify the message recipient of a change in shipping status for a product that has already been purchased.
  | RESERVATION_UPDATE -- ^ Notify the message recipient of updates to an existing reservation.
  | ISSUE_RESOLUTION
  -- ^ Notify the message recipient of an update to a customer service issue
  -- that was initiated in a Messenger conversation, following a transaction.
  | APPOINTMENT_UPDATE -- ^ Notify the message recipient of a change to an existing appointment.
  | GAME_EVENT -- ^ Notify the message recipient of a change in in-game user progression, global events, or a live sporting event.
  | TRANSPORTATION_UPDATE -- ^ Notify the message recipient of updates to an existing transportation reservation.
  | FEATURE_FUNCTIONALITY_UPDATE -- ^ Notify the message recipient of new features or functionality that become available in your bot.
  | TICKET_UPDATE -- ^ Notify the message recipient of updates pertaining to an event for which a person already has a ticket.
  deriving (Eq, Show, Read, Ord)

instance FromJSON MessageTag where
  parseJSON = withText' "MessageTag"
      [("ACCOUNT_UPDATE", ACCOUNT_UPDATE)
      ,("PAYMENT_UPDATE", PAYMENT_UPDATE)
      ,("PERSONAL_FINANCE_UPDATE", PERSONAL_FINANCE_UPDATE)
      ,("SHIPPING_UPDATE", SHIPPING_UPDATE)
      ,("RESERVATION_UPDATE", RESERVATION_UPDATE)
      ,("ISSUE_RESOLUTION", ISSUE_RESOLUTION)
      ,("APPOINTMENT_UPDATE", APPOINTMENT_UPDATE)
      ,("GAME_EVENT", GAME_EVENT)
      ,("TRANSPORTATION_UPDATE", TRANSPORTATION_UPDATE)
      ,("FEATURE_FUNCTIONALITY_UPDATE", FEATURE_FUNCTIONALITY_UPDATE)
      ,("TICKET_UPDATE", TICKET_UPDATE)
      ]

instance ToJSON MessageTag where
  toJSON ACCOUNT_UPDATE = String "ACCOUNT_UPDATE"
  toJSON PAYMENT_UPDATE = String "PAYMENT_UPDATE"
  toJSON PERSONAL_FINANCE_UPDATE = String "PERSONAL_FINANCE_UPDATE"
  toJSON SHIPPING_UPDATE = String "SHIPPING_UPDATE"
  toJSON RESERVATION_UPDATE = String "RESERVATION_UPDATE"
  toJSON ISSUE_RESOLUTION = String "ISSUE_RESOLUTION"
  toJSON APPOINTMENT_UPDATE = String "APPOINTMENT_UPDATE"
  toJSON GAME_EVENT = String "GAME_EVENT"
  toJSON TRANSPORTATION_UPDATE = String "TRANSPORTATION_UPDATE"
  toJSON FEATURE_FUNCTIONALITY_UPDATE = String "FEATURE_FUNCTIONALITY_UPDATE"
  toJSON TICKET_UPDATE = String "TICKET_UPDATE"

-- | An app can be assigned the roles of `PrimaryReceiver` or `SecondaryReceiver`.
data AppRole =
    PrimaryReceiver
  | SecondaryReceiver
  deriving (Eq, Show, Read, Ord)

instance FromJSON AppRole where
  parseJSON = withText' "AppRole"
      [("primary_receiver", PrimaryReceiver)
      ,("secondary_receiver", SecondaryReceiver)
      ]

instance ToJSON AppRole where
  toJSON PrimaryReceiver = String "primary_receiver"
  toJSON SecondaryReceiver = String "secondary_receiver"

-- | Which countries might see your bot appear in the Discover Tab
--
-- https://developers.facebook.com/docs/messenger-platform/reference/messenger-profile-api/target-audience
data AudienceType =
    ALL -- ^ Bot might appear in anyone's Discover Tab
  | CUSTOM -- ^ Required to provide white- or blacklisted countries
  | NONE -- ^ Bot will not appear in anyone's Discover Tab
  deriving (Eq, Show, Read, Ord)

instance FromJSON AudienceType where
  parseJSON = withText' "AudienceType"
      [("all", ALL)
      ,("custom", CUSTOM)
      ,("none", NONE)
      ]

instance ToJSON AudienceType where
  toJSON ALL = String "all"
  toJSON CUSTOM = String "custom"
  toJSON NONE = String "none"

-- | Aspect ratio used to render images specified by @"image_url"@ in (generic) element objects. Default is `HORIZONTAL`.
data ImageAspectRatioType =
    HORIZONTAL -- ^ @1\.91:1@ aspect ratio
  | SQUARE -- ^ @1:1@ aspect ratio
  deriving (Eq, Show, Read, Ord)

instance FromJSON ImageAspectRatioType where
  parseJSON = withText' "ImageAspectRatioType"
      [("horizontal", HORIZONTAL)
      ,("square", SQUARE)
      ]

instance ToJSON ImageAspectRatioType where
  toJSON HORIZONTAL = String "horizontal"
  toJSON SQUARE = String "square"

-- | Whether to show or hide the share button used in webview windows
data WebviewShareType = SHOW
                      | HIDE
  deriving (Eq, Show, Read, Ord)

instance FromJSON WebviewShareType where
  parseJSON = withText' "WebviewShareType"
        [("show", SHOW)
        ,("hide", HIDE)
        ]

instance ToJSON WebviewShareType where
  toJSON SHOW = String "show"
  toJSON HIDE = String "hide"
