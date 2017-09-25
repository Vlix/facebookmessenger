module Web.Facebook.Messenger.Types.Static where


import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (unpack,Text)
import qualified Data.HashMap.Strict as HM

-- | Helper function to avoid `Maybe [a]`s
mEmptyList :: ToJSON a => Text -> [a] -> Maybe Pair
mEmptyList _ [] = Nothing
mEmptyList t l  = Just $ t .= l

-- | Helper function to avoid `Maybe Bool`s
-- (first Bool is what Nothing would default to)
mBool :: Text -> Bool -> Bool -> Maybe Pair
mBool t a b = if a == b then Nothing else Just $ t .= b

object' :: [Maybe Pair] -> Value
object' = Object . HM.fromList . catMaybes

(.=!!) :: ToJSON a => Text -> Maybe a -> Maybe Pair
_    .=!! Nothing = Nothing
name .=!! (Just v) = Just $ name .= v

(.=!) :: ToJSON a => Text -> a -> Maybe Pair
name .=! value = Just $ name .= value


withText' :: String -> [(Text, a)] -> Value -> Parser a
withText' s tups = withText s $ \t ->
    case lookup t tups of
      Just val -> pure val
      _ -> fail $ "Wrong String for " <> s <> ": " <> unpack t


data SenderActionType =
    MARK_SEEN  -- Mark last message as read
  | TYPING_ON  -- Turn typing indicators on
  | TYPING_OFF -- Turn typing indicators off
                   -- Typing indicators are automatically turned off after 20 seconds
  deriving (Eq, Show)

instance ToJSON SenderActionType where
  toJSON MARK_SEEN = String "mark_seen"
  toJSON TYPING_ON = String "typing_on"
  toJSON TYPING_OFF = String "typing_off"

instance FromJSON SenderActionType where
  parseJSON = withText' "SenderActionType"
      [("mark_seen", MARK_SEEN)
      ,("typing_on", TYPING_ON)
      ,("typing_off", TYPING_OFF)]


data NotificationType =
    REGULAR     -- Emits a sound/vibration and a phone notification
  | SILENT_PUSH -- Emits a phone notification
  | NO_PUSH     -- Emits neither
  deriving (Eq, Show)

instance ToJSON NotificationType where
  toJSON REGULAR = String "REGULAR"
  toJSON SILENT_PUSH = String "SILENT_PUSH"
  toJSON NO_PUSH = String "NO_PUSH"

instance FromJSON NotificationType where
  parseJSON = withText' "NotificationType"
      [("REGULAR", REGULAR)
      ,("SILENT_PUSH", SILENT_PUSH)
      ,("NO_PUSH", NO_PUSH)]


data WebViewHeightRatioType =
    COMPACT
  | TALL
  | FULL
  deriving (Eq, Show)

instance ToJSON WebViewHeightRatioType where
  toJSON COMPACT = String "compact"
  toJSON TALL = String "tall"
  toJSON FULL = String "full"

instance FromJSON WebViewHeightRatioType where
  parseJSON = withText' "WebViewHeightRatioType"
      [("compact", COMPACT)
      ,("tall", TALL)
      ,("full", FULL)]


data AttachmentType =
    IMAGE
  | VIDEO
  | AUDIO
  | FILE
  | LOCATION
  | FALLBACK
  deriving (Eq, Show)

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
      ,("file", FILE)]


data AirlineTravelClassType =
    ECONOMY
  | BUSINESS
  | FIRST_CLASS
  deriving (Eq, Show)

instance ToJSON AirlineTravelClassType where
  toJSON ECONOMY = String "economy"
  toJSON BUSINESS = String "business"
  toJSON FIRST_CLASS = String "first_class"

instance FromJSON AirlineTravelClassType where
  parseJSON = withText' "AirlineTravelClassType"
      [("economy", ECONOMY)
      ,("business", BUSINESS)
      ,("first_class", FIRST_CLASS)]


data AirlineUpdateType =
    DELAY
  | GATE_CHANGE
  | CANCELLATION
  deriving (Eq, Show)

instance ToJSON AirlineUpdateType where
  toJSON DELAY = String "delay"
  toJSON GATE_CHANGE = String "gate_change"
  toJSON CANCELLATION = String "cancellation"

instance FromJSON AirlineUpdateType where
  parseJSON = withText' "AirlineUpdateType"
      [("delay", DELAY)
      ,("gate_change", GATE_CHANGE)
      ,("cancellation", CANCELLATION)]


data ReferralSource =
    SHORTLINK
  | ADS
  | MESSENGER_CODE
  | DISCOVER_TAB
  deriving (Eq, Show)

instance ToJSON ReferralSource where
  toJSON ADS = String "ADS"
  toJSON SHORTLINK = String "SHORTLINK"
  toJSON MESSENGER_CODE = String "MESSENGER_CODE"
  toJSON DISCOVER_TAB = String "DISCOVER_TAB"

instance FromJSON ReferralSource where
  parseJSON = withText' "ReferralSource"
      [("ADS", ADS)
      ,("SHORTLINK", SHORTLINK)
      ,("MESSENGER_CODE", MESSENGER_CODE)
      ,("DISCOVER_TAB", DISCOVER_TAB)
      ]


data ListStyle =
    ListCOMPACT
  | ListLARGE
  deriving (Eq, Show)

instance ToJSON ListStyle where
  toJSON ListCOMPACT = String "compact"
  toJSON ListLARGE = String "large"

instance FromJSON ListStyle where
  parseJSON = withText' "ListStyle"
      [("compact", ListCOMPACT)
      ,("large", ListLARGE)]


data PaymentType =
    FIXED_AMOUNT
  | FLEXIBLE_AMOUNT
  deriving (Eq, Show)

instance ToJSON PaymentType where
  toJSON FIXED_AMOUNT = String "FIXED_AMOUNT"
  toJSON FLEXIBLE_AMOUNT = String "FLEXIBLE_AMOUNT"

instance FromJSON PaymentType where
  parseJSON = withText' "PaymentType"
      [("FIXED_AMOUNT", FIXED_AMOUNT)
      ,("FLEXIBLE_AMOUNT", FLEXIBLE_AMOUNT)]


data RequestedUserInfoType =
    SHIPPING_ADDRESS
  | CONTACT_NAME
  | CONTACT_PHONE
  | CONTACT_EMAIL
  deriving (Eq, Show)

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
      ,("contact_email", CONTACT_EMAIL)]


data MessageTag =
    SHIPPING_UPDATE
  | RESERVATION_UPDATE
  | ISSUE_RESOLUTION
  | APPOINTMENT_UPDATE
  | GAME_EVENT
  | TRANSPORTATION_UPDATE
  | FEATURE_FUNCTIONALITY_UPDATE
  | TICKET_UPDATE
  deriving (Eq, Show)

instance FromJSON MessageTag where
  parseJSON = withText' "MessageTag"
      [("SHIPPING_UPDATE", SHIPPING_UPDATE)
      ,("RESERVATION_UPDATE", RESERVATION_UPDATE)
      ,("ISSUE_RESOLUTION", ISSUE_RESOLUTION)
      ,("APPOINTMENT_UPDATE", APPOINTMENT_UPDATE)
      ,("GAME_EVENT", GAME_EVENT)
      ,("TRANSPORTATION_UPDATE", TRANSPORTATION_UPDATE)
      ,("FEATURE_FUNCTIONALITY_UPDATE", FEATURE_FUNCTIONALITY_UPDATE)
      ,("TICKET_UPDATE", TICKET_UPDATE)]

instance ToJSON MessageTag where
  toJSON SHIPPING_UPDATE = String $ "SHIPPING_UPDATE"
  toJSON RESERVATION_UPDATE = String $ "RESERVATION_UPDATE"
  toJSON ISSUE_RESOLUTION = String $ "ISSUE_RESOLUTION"
  toJSON APPOINTMENT_UPDATE = String $ "APPOINTMENT_UPDATE"
  toJSON GAME_EVENT = String $ "GAME_EVENT"
  toJSON TRANSPORTATION_UPDATE = String $ "TRANSPORTATION_UPDATE"
  toJSON FEATURE_FUNCTIONALITY_UPDATE = String $ "FEATURE_FUNCTIONALITY_UPDATE"
  toJSON TICKET_UPDATE = String $ "TICKET_UPDATE"


data AppRole =
    PrimaryReceiver
  | SecondaryReceiver
  deriving (Eq, Show)

instance FromJSON AppRole where
  parseJSON = withText' "AppRole"
      [("primary_receiver", PrimaryReceiver)
      ,("secondary_receiver", SecondaryReceiver)]

instance ToJSON AppRole where
  toJSON PrimaryReceiver = String $ "primary_receiver"
  toJSON SecondaryReceiver = String $ "secondary_receiver"
