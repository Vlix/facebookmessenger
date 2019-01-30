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
  MessagingType (..)
  , NotificationType (..)
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
  , PriorMessageType (..)
  , FBLocale (..)
  -- * Type synonyms\/newtypes
  , URL
  , AppId (..)
  , PSID (..)
  , PageID (..)
  )
where


import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text, pack, unpack)
import Text.Read (readEither)

import Web.Facebook.Messenger.Internal

-- | Text which should be formatted as a valid URL (e.g. @"https://www.example.com"@)
type URL = Text

-- | When representing a user, these IDs are page-scoped IDs (PSID).
-- This means that the IDs of users are unique for a given page.
newtype PSID = PSID Text
  deriving (Eq, Show, Read, Ord, FromJSON, ToJSON)

-- | Pages have their own unique ID
newtype PageID = PageID Text
  deriving (Eq, Show, Read, Ord, FromJSON, ToJSON)

-- | Newtype wrapper around Text, because the AppId is very different than anything else used as IDs in this package
newtype AppId = AppId Text
  deriving (Eq, Show, Read, Ord, FromJSON, ToJSON)

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

-- | The @messaging_type@ property identifies the messaging type
-- of the message being sent, and is a more explicit way to ensure
-- bots are complying with policies for specific messaging types
-- and respecting people's preferences.
--
-- https://developers.facebook.com/docs/messenger-platform/send-messages#messaging_types
data MessagingType =
      RESPONSE -- ^ Response to a user message
    | UPDATE -- ^ Self-initiated update
    | MESSAGE_TAG -- ^ Message with tag sent outside the 24+1 messaging window
  deriving (Eq, Show, Read, Ord)

instance FromJSON MessagingType where
  parseJSON = withText' "MessagingType"
      [("RESPONSE", RESPONSE)
      ,("UPDATE",UPDATE)
      ,("MESSAGE_TAG",MESSAGE_TAG)
      ]

instance ToJSON MessagingType where
  toJSON RESPONSE = String "RESPONSE"
  toJSON UPDATE = String "UPDATE"
  toJSON MESSAGE_TAG = String "MESSAGE_TAG"

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
  | CUSTOMER_CHAT_PLUGIN -- ^ Facebook Chat Plugin
  deriving (Eq, Show, Read, Ord)

instance ToJSON ReferralSource where
  toJSON SHORTLINK = String "SHORTLINK"
  toJSON ADS = String "ADS"
  toJSON MESSENGER_CODE = String "MESSENGER_CODE"
  toJSON DISCOVER_TAB = String "DISCOVER_TAB"
  toJSON CUSTOMER_CHAT_PLUGIN = String "CUSTOMER_CHAT_PLUGIN"

instance FromJSON ReferralSource where
  parseJSON = withTextCI "ReferralSource"
      [("SHORTLINK", SHORTLINK)
      ,("ADS", ADS)
      ,("MESSENGER_CODE", MESSENGER_CODE)
      ,("DISCOVER_TAB", DISCOVER_TAB)
      ,("CUSTOMER_CHAT_PLUGIN", CUSTOMER_CHAT_PLUGIN)
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
  parseJSON = withTextCI "ListStyle"
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
  | APPLICATION_UPDATE -- ^ Notify the message recipient of a change to their account settings.
  | APPOINTMENT_UPDATE -- ^ Notify the message recipient of a change to an existing appointment.
  | COMMUNITY_ALERT -- ^ Notify the message recipient of emergency or utility alerts, or issue a safety check in your community.
  | CONFIRMED_EVENT_REMINDER -- ^ Send the message recipient reminders of a scheduled event which a person is going to attend.
  | FEATURE_FUNCTIONALITY_UPDATE -- ^ Notify the message recipient of new features or functionality that become available in your bot.
  | GAME_EVENT -- ^ Notify the message recipient of a change in in-game user progression, global events, or a live sporting event.
  | ISSUE_RESOLUTION
  -- ^ Notify the message recipient of an update to a customer service issue
  -- that was initiated in a Messenger conversation, following a transaction.
  | NON_PROMOTIONAL_SUBSCRIPTION
  -- ^ Send non-promotional messages under the News, Productivity, and Personal Trackers categories
  -- described in the Messenger Platform's subscription messaging policy.
  -- You can apply for access to use this tag under the Page Settings > Messenger Platform.
  | PAIRING_UPDATE -- ^ Notify the message recipient that a pairing has been identified based on a prior request.
  | PAYMENT_UPDATE -- ^ Notify the message recipient of a payment update for an existing transaction.
  | PERSONAL_FINANCE_UPDATE -- ^ Confirm a message recipient's financial activity.
  | RESERVATION_UPDATE -- ^ Notify the message recipient of updates to an existing reservation.
  | SHIPPING_UPDATE -- ^ Notify the message recipient of a change in shipping status for a product that has already been purchased.
  | TICKET_UPDATE -- ^ Notify the message recipient of updates pertaining to an event for which a person already has a ticket.
  | TRANSPORTATION_UPDATE -- ^ Notify the message recipient of updates to an existing transportation reservation.
  deriving (Eq, Show, Read, Ord)

instance FromJSON MessageTag where
  parseJSON = withText' "MessageTag"
      [("ACCOUNT_UPDATE", ACCOUNT_UPDATE)
      ,("APPLICATION_UPDATE", APPLICATION_UPDATE)
      ,("APPOINTMENT_UPDATE", APPOINTMENT_UPDATE)
      ,("COMMUNITY_ALERT", COMMUNITY_ALERT)
      ,("CONFIRMED_EVENT_REMINDER", CONFIRMED_EVENT_REMINDER)
      ,("FEATURE_FUNCTIONALITY_UPDATE", FEATURE_FUNCTIONALITY_UPDATE)
      ,("GAME_EVENT", GAME_EVENT)
      ,("ISSUE_RESOLUTION", ISSUE_RESOLUTION)
      ,("NON_PROMOTIONAL_SUBSCRIPTION", NON_PROMOTIONAL_SUBSCRIPTION)
      ,("PAIRING_UPDATE", PAIRING_UPDATE)
      ,("PAYMENT_UPDATE", PAYMENT_UPDATE)
      ,("PERSONAL_FINANCE_UPDATE", PERSONAL_FINANCE_UPDATE)
      ,("RESERVATION_UPDATE", RESERVATION_UPDATE)
      ,("SHIPPING_UPDATE", SHIPPING_UPDATE)
      ,("TICKET_UPDATE", TICKET_UPDATE)
      ,("TRANSPORTATION_UPDATE", TRANSPORTATION_UPDATE)
      ]

instance ToJSON MessageTag where
  toJSON ACCOUNT_UPDATE = String "ACCOUNT_UPDATE"
  toJSON APPLICATION_UPDATE = String "APPLICATION_UPDATE"
  toJSON APPOINTMENT_UPDATE = String "APPOINTMENT_UPDATE"
  toJSON COMMUNITY_ALERT = String "COMMUNITY_ALERT"
  toJSON CONFIRMED_EVENT_REMINDER = String "CONFIRMED_EVENT_REMINDER"
  toJSON FEATURE_FUNCTIONALITY_UPDATE = String "FEATURE_FUNCTIONALITY_UPDATE"
  toJSON GAME_EVENT = String "GAME_EVENT"
  toJSON ISSUE_RESOLUTION = String "ISSUE_RESOLUTION"
  toJSON NON_PROMOTIONAL_SUBSCRIPTION = String "NON_PROMOTIONAL_SUBSCRIPTION"
  toJSON PAIRING_UPDATE = String "PAIRING_UPDATE"
  toJSON PAYMENT_UPDATE = String "PAYMENT_UPDATE"
  toJSON PERSONAL_FINANCE_UPDATE = String "PERSONAL_FINANCE_UPDATE"
  toJSON RESERVATION_UPDATE = String "RESERVATION_UPDATE"
  toJSON SHIPPING_UPDATE = String "SHIPPING_UPDATE"
  toJSON TICKET_UPDATE = String "TICKET_UPDATE"
  toJSON TRANSPORTATION_UPDATE = String "TRANSPORTATION_UPDATE"

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

-- | Whether to show or hide the share button used in webview windows
data PriorMessageType = CheckBoxPlugin
                      | CustomerMatching
  deriving (Eq, Show, Read, Ord)

instance FromJSON PriorMessageType where
  parseJSON = withText' "PriorMessageType"
        [("checkbox_plugin", CheckBoxPlugin)
        ,("customer_matching", CustomerMatching)
        ]

instance ToJSON PriorMessageType where
  toJSON CheckBoxPlugin = String "checkbox_plugin"
  toJSON CustomerMatching = String "customer_matching"

-- | All possible locales that FB supports
data FBLocale =
    FBaf_ZA | FBar_AR | FBas_IN | FBaz_AZ
  | FBbe_BY | FBbg_BG | FBbn_IN | FBbr_FR | FBbs_BA
  | FBca_ES | FBcb_IQ | FBco_FR | FBcs_CZ | FBcx_PH | FBcy_GB
  | FBda_DK | FBde_DE
  | FBel_GR | FBen_GB | FBen_US | FBet_EE | FBen_UD | FBes_LA | FBes_ES | FBeu_ES
  | FBfa_IR | FBff_NG | FBfi_FI | FBfo_FO | FBfr_CA | FBfr_FR | FBfy_NL
  | FBga_IE | FBgl_ES | FBgn_PY | FBgu_IN
  | FBha_NG | FBhe_IL | FBhi_IN | FBhr_HR | FBhu_HU | FBhy_AM
  | FBid_ID | FBis_IS | FBit_IT
  | FBja_JP | FBja_KS | FBjv_ID
  | FBka_GE | FBkk_KZ | FBkm_KH | FBko_KR | FBku_TR | FBkn_IN
  | FBlv_LV | FBlt_LT
  | FBmg_MG | FBmk_MK | FBml_IN | FBmn_MN | FBmr_IN | FBms_MY | FBmt_MT | FBmy_MM
  | FBnb_NO | FBne_NP | FBnn_NO | FBnl_BE | FBnl_NL
  | FBor_IN
  | FBpa_IN | FBpl_PL | FBps_AF | FBpt_BR | FBpt_PT
  | FBqz_MM
  | FBro_RO | FBru_RU | FBrw_RW
  | FBsc_IT | FBsi_LK | FBsk_SK | FBsl_SI | FBso_SO | FBsq_AL | FBsr_RS | FBsv_SE | FBsw_KE | FBsz_PL
  | FBta_IN | FBte_IN | FBth_TH | FBtg_TJ | FBtl_PH | FBtr_TR | FBtz_MA
  | FBuk_UA | FBur_PK | FBuz_UZ
  | FBvi_VN
  | FBzh_CN | FBzh_HK | FBzh_TW
  deriving (Ord, Eq, Enum)

instance Show FBLocale where
  show FBaf_ZA = "af_ZA"
  show FBar_AR = "ar_AR"
  show FBas_IN = "as_IN"
  show FBaz_AZ = "az_AZ"
  show FBbe_BY = "be_BY"
  show FBbg_BG = "bg_BG"
  show FBbn_IN = "bn_IN"
  show FBbr_FR = "br_FR"
  show FBbs_BA = "bs_BA"
  show FBca_ES = "ca_ES"
  show FBcb_IQ = "cb_IQ"
  show FBco_FR = "co_FR"
  show FBcs_CZ = "cs_CZ"
  show FBcx_PH = "cx_PH"
  show FBcy_GB = "cy_GB"
  show FBda_DK = "da_DK"
  show FBde_DE = "de_DE"
  show FBel_GR = "el_GR"
  show FBen_GB = "en_GB"
  show FBen_US = "en_US"
  show FBet_EE = "et_EE"
  show FBen_UD = "en_UD"
  show FBes_LA = "es_LA"
  show FBes_ES = "es_ES"
  show FBeu_ES = "eu_ES"
  show FBfa_IR = "fa_IR"
  show FBff_NG = "ff_NG"
  show FBfi_FI = "fi_FI"
  show FBfo_FO = "fo_FO"
  show FBfr_CA = "fr_CA"
  show FBfr_FR = "fr_FR"
  show FBfy_NL = "fy_NL"
  show FBga_IE = "ga_IE"
  show FBgl_ES = "gl_ES"
  show FBgn_PY = "gn_PY"
  show FBgu_IN = "gu_IN"
  show FBha_NG = "ha_NG"
  show FBhe_IL = "he_IL"
  show FBhi_IN = "hi_IN"
  show FBhr_HR = "hr_HR"
  show FBhu_HU = "hu_HU"
  show FBhy_AM = "hy_AM"
  show FBid_ID = "id_ID"
  show FBis_IS = "is_IS"
  show FBit_IT = "it_IT"
  show FBja_JP = "ja_JP"
  show FBja_KS = "ja_KS"
  show FBjv_ID = "jv_ID"
  show FBka_GE = "ka_GE"
  show FBkk_KZ = "kk_KZ"
  show FBkm_KH = "km_KH"
  show FBko_KR = "ko_KR"
  show FBku_TR = "ku_TR"
  show FBkn_IN = "kn_IN"
  show FBlv_LV = "lv_LV"
  show FBlt_LT = "lt_LT"
  show FBmg_MG = "mg_MG"
  show FBmk_MK = "mk_MK"
  show FBml_IN = "ml_IN"
  show FBmn_MN = "mn_MN"
  show FBmr_IN = "mr_IN"
  show FBms_MY = "ms_MY"
  show FBmt_MT = "mt_MT"
  show FBmy_MM = "my_MM"
  show FBnb_NO = "nb_NO"
  show FBne_NP = "ne_NP"
  show FBnn_NO = "nn_NO"
  show FBnl_BE = "nl_BE"
  show FBnl_NL = "nl_NL"
  show FBor_IN = "or_IN"
  show FBpa_IN = "pa_IN"
  show FBpl_PL = "pl_PL"
  show FBps_AF = "ps_AF"
  show FBpt_BR = "pt_BR"
  show FBpt_PT = "pt_PT"
  show FBqz_MM = "qz_MM"
  show FBro_RO = "ro_RO"
  show FBru_RU = "ru_RU"
  show FBrw_RW = "rw_RW"
  show FBsc_IT = "sc_IT"
  show FBsi_LK = "si_LK"
  show FBsk_SK = "sk_SK"
  show FBsl_SI = "sl_SI"
  show FBso_SO = "so_SO"
  show FBsq_AL = "sq_AL"
  show FBsr_RS = "sr_RS"
  show FBsw_KE = "sw_KE"
  show FBsz_PL = "sz_PL"
  show FBsv_SE = "sv_SE"
  show FBta_IN = "ta_IN"
  show FBte_IN = "te_IN"
  show FBth_TH = "th_TH"
  show FBtg_TJ = "tg_TJ"
  show FBtl_PH = "tl_PH"
  show FBtr_TR = "tr_TR"
  show FBtz_MA = "tz_MA"
  show FBuk_UA = "uk_UA"
  show FBur_PK = "ur_PK"
  show FBuz_UZ = "uz_UZ"
  show FBvi_VN = "vi_VN"
  show FBzh_CN = "zh_CN"
  show FBzh_HK = "zh_HK"
  show FBzh_TW = "zh_TW"

instance Read FBLocale where
  readsPrec _ x =
    let (locale,rest) = splitAt 5 x
    in if length locale /= 5
        then []
        else case locale of
                "af_ZA" -> [(FBaf_ZA,rest)]
                "ar_AR" -> [(FBar_AR,rest)]
                "as_IN" -> [(FBas_IN,rest)]
                "az_AZ" -> [(FBaz_AZ,rest)]
                "be_BY" -> [(FBbe_BY,rest)]
                "bg_BG" -> [(FBbg_BG,rest)]
                "bn_IN" -> [(FBbn_IN,rest)]
                "br_FR" -> [(FBbr_FR,rest)]
                "bs_BA" -> [(FBbs_BA,rest)]
                "ca_ES" -> [(FBca_ES,rest)]
                "cb_IQ" -> [(FBcb_IQ,rest)]
                "co_FR" -> [(FBco_FR,rest)]
                "cs_CZ" -> [(FBcs_CZ,rest)]
                "cx_PH" -> [(FBcx_PH,rest)]
                "cy_GB" -> [(FBcy_GB,rest)]
                "da_DK" -> [(FBda_DK,rest)]
                "de_DE" -> [(FBde_DE,rest)]
                "el_GR" -> [(FBel_GR,rest)]
                "en_GB" -> [(FBen_GB,rest)]
                "en_US" -> [(FBen_US,rest)]
                "et_EE" -> [(FBet_EE,rest)]
                "en_UD" -> [(FBen_UD,rest)]
                "es_LA" -> [(FBes_LA,rest)]
                "es_ES" -> [(FBes_ES,rest)]
                "eu_ES" -> [(FBeu_ES,rest)]
                "fa_IR" -> [(FBfa_IR,rest)]
                "ff_NG" -> [(FBff_NG,rest)]
                "fi_FI" -> [(FBfi_FI,rest)]
                "fo_FO" -> [(FBfo_FO,rest)]
                "fr_CA" -> [(FBfr_CA,rest)]
                "fr_FR" -> [(FBfr_FR,rest)]
                "fy_NL" -> [(FBfy_NL,rest)]
                "ga_IE" -> [(FBga_IE,rest)]
                "gl_ES" -> [(FBgl_ES,rest)]
                "gn_PY" -> [(FBgn_PY,rest)]
                "gu_IN" -> [(FBgu_IN,rest)]
                "ha_NG" -> [(FBha_NG,rest)]
                "he_IL" -> [(FBhe_IL,rest)]
                "hi_IN" -> [(FBhi_IN,rest)]
                "hr_HR" -> [(FBhr_HR,rest)]
                "hu_HU" -> [(FBhu_HU,rest)]
                "hy_AM" -> [(FBhy_AM,rest)]
                "id_ID" -> [(FBid_ID,rest)]
                "is_IS" -> [(FBis_IS,rest)]
                "it_IT" -> [(FBit_IT,rest)]
                "ja_JP" -> [(FBja_JP,rest)]
                "ja_KS" -> [(FBja_KS,rest)]
                "jv_ID" -> [(FBjv_ID,rest)]
                "ka_GE" -> [(FBka_GE,rest)]
                "kk_KZ" -> [(FBkk_KZ,rest)]
                "km_KH" -> [(FBkm_KH,rest)]
                "ko_KR" -> [(FBko_KR,rest)]
                "ku_TR" -> [(FBku_TR,rest)]
                "kn_IN" -> [(FBkn_IN,rest)]
                "lv_LV" -> [(FBlv_LV,rest)]
                "lt_LT" -> [(FBlt_LT,rest)]
                "mg_MG" -> [(FBmg_MG,rest)]
                "mk_MK" -> [(FBmk_MK,rest)]
                "ml_IN" -> [(FBml_IN,rest)]
                "mn_MN" -> [(FBmn_MN,rest)]
                "mr_IN" -> [(FBmr_IN,rest)]
                "ms_MY" -> [(FBms_MY,rest)]
                "mt_MT" -> [(FBmt_MT,rest)]
                "my_MM" -> [(FBmy_MM,rest)]
                "nb_NO" -> [(FBnb_NO,rest)]
                "ne_NP" -> [(FBne_NP,rest)]
                "nn_NO" -> [(FBnn_NO,rest)]
                "nl_BE" -> [(FBnl_BE,rest)]
                "nl_NL" -> [(FBnl_NL,rest)]
                "or_IN" -> [(FBor_IN,rest)]
                "pa_IN" -> [(FBpa_IN,rest)]
                "pl_PL" -> [(FBpl_PL,rest)]
                "ps_AF" -> [(FBps_AF,rest)]
                "pt_BR" -> [(FBpt_BR,rest)]
                "pt_PT" -> [(FBpt_PT,rest)]
                "qz_MM" -> [(FBqz_MM,rest)]
                "ro_RO" -> [(FBro_RO,rest)]
                "ru_RU" -> [(FBru_RU,rest)]
                "rw_RW" -> [(FBrw_RW,rest)]
                "sc_IT" -> [(FBsc_IT,rest)]
                "si_LK" -> [(FBsi_LK,rest)]
                "sk_SK" -> [(FBsk_SK,rest)]
                "sl_SI" -> [(FBsl_SI,rest)]
                "so_SO" -> [(FBso_SO,rest)]
                "sq_AL" -> [(FBsq_AL,rest)]
                "sr_RS" -> [(FBsr_RS,rest)]
                "sw_KE" -> [(FBsw_KE,rest)]
                "sz_PL" -> [(FBsz_PL,rest)]
                "sv_SE" -> [(FBsv_SE,rest)]
                "ta_IN" -> [(FBta_IN,rest)]
                "te_IN" -> [(FBte_IN,rest)]
                "th_TH" -> [(FBth_TH,rest)]
                "tg_TJ" -> [(FBtg_TJ,rest)]
                "tl_PH" -> [(FBtl_PH,rest)]
                "tr_TR" -> [(FBtr_TR,rest)]
                "tz_MA" -> [(FBtz_MA,rest)]
                "uk_UA" -> [(FBuk_UA,rest)]
                "ur_PK" -> [(FBur_PK,rest)]
                "uz_UZ" -> [(FBuz_UZ,rest)]
                "vi_VN" -> [(FBvi_VN,rest)]
                "zh_CN" -> [(FBzh_CN,rest)]
                "zh_HK" -> [(FBzh_HK,rest)]
                "zh_TW" -> [(FBzh_TW,rest)]
                _ -> []

instance ToJSON FBLocale where
  toJSON = String . pack . show

instance FromJSON FBLocale where
  parseJSON = withText "FBLocale" $ \t -> go t $ readEither $ unpack t
    where go :: Text -> Either String FBLocale -> Parser FBLocale
          go t = either wat pure
            where wat = fail . mappend ("Unsupported locale (" `mappend` unpack t `mappend` "): ")
