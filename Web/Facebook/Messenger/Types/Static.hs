module Web.Facebook.Messenger.Types.Static where


import Data.Aeson
import Data.Aeson.Types     (typeMismatch,Pair)
import Data.Maybe           (catMaybes)
import Data.Monoid          ((<>))
import Data.Text            (unpack,Text)
import Data.HashMap.Strict  as HM

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
name .=!! Nothing  = Nothing
name .=!! (Just v) = Just $ name .= v

(.=!) :: ToJSON a => Text -> a -> Maybe Pair
name .=! value = Just $ name .= value



data SenderActionType = MARK_SEEN  -- Mark last message as read
                      | TYPING_ON  -- Turn typing indicators on
                      | TYPING_OFF -- Turn typing indicators off
                   -- Typing indicators are automatically turned off after 20 seconds
  deriving (Eq, Show)

data NotificationType = REGULAR     -- Emits a sound/vibration and a phone notification
                      | SILENT_PUSH -- Emits a phone notification
                      | NO_PUSH     -- Emits neither
  deriving (Eq, Show)

data WebViewHeightRatioType = COMPACT
                            | TALL
                            | FULL
  deriving (Eq, Show)

data AttachmentType = IMAGE
                    | VIDEO
                    | AUDIO
                    | FILE
  deriving (Eq, Show)

data AirlineTravelClassType = ECONOMY
                            | BUSINESS
                            | FIRST_CLASS
  deriving (Eq, Show)

data AirlineUpdateType = DELAY
                       | GATE_CHANGE
                       | CANCELLATION
  deriving (Eq, Show)

data ReferralSource = SHORTLINK
  deriving (Eq, Show)

data ListStyle = ListCOMPACT
               | ListLARGE
  deriving (Eq, Show)

data PaymentType = FIXED_AMOUNT
                 | FLEXIBLE_AMOUNT
  deriving (Eq, Show)

data RequestedUserInfoType = SHIPPING_ADDRESS
                           | CONTACT_NAME
                           | CONTACT_PHONE
                           | CONTACT_EMAIL
  deriving (Eq, Show)



-- JSON instances

instance ToJSON SenderActionType where
  toJSON MARK_SEEN  = String "mark_seen"
  toJSON TYPING_ON  = String "typing_on"
  toJSON TYPING_OFF = String "typing_off"

instance FromJSON SenderActionType where
  parseJSON (String "mark_seen")  = pure MARK_SEEN
  parseJSON (String "typing_on")  = pure TYPING_ON
  parseJSON (String "typing_off") = pure TYPING_OFF
  parseJSON (String wat)          = fail $ "Wrong String for SenderActionType: " <> unpack wat
  parseJSON wat = typeMismatch "SenderActionType" wat


instance ToJSON NotificationType where
  toJSON REGULAR     = String "REGULAR"
  toJSON SILENT_PUSH = String "SILENT_PUSH"
  toJSON NO_PUSH     = String "NO_PUSH"

instance FromJSON NotificationType where
  parseJSON (String "REGULAR")     = pure REGULAR
  parseJSON (String "SILENT_PUSH") = pure SILENT_PUSH
  parseJSON (String "NO_PUSH")     = pure NO_PUSH
  parseJSON (String wat)           = fail $ "Wrong String for NotificationType: " <> unpack wat
  parseJSON wat = typeMismatch "NotificationType" wat


instance ToJSON WebViewHeightRatioType where
  toJSON COMPACT = String "compact"
  toJSON TALL    = String "tall"
  toJSON FULL    = String "full"

instance FromJSON WebViewHeightRatioType where
  parseJSON (String "compact") = pure COMPACT
  parseJSON (String "tall")    = pure TALL
  parseJSON (String "full")    = pure FULL
  parseJSON (String wat)       = fail $ "Wrong String for WebViewHeightRatioType: " <> unpack wat
  parseJSON wat = typeMismatch "WebViewHeightRatioType" wat


instance ToJSON AttachmentType where
  toJSON IMAGE = String "image"
  toJSON VIDEO = String "video"
  toJSON AUDIO = String "audio"
  toJSON FILE  = String "file"

instance FromJSON AttachmentType where
  parseJSON (String "image") = pure IMAGE
  parseJSON (String "audio") = pure AUDIO
  parseJSON (String "video") = pure VIDEO
  parseJSON (String "file")  = pure FILE
  parseJSON (String wat)     = fail $ "Wrong String for AttachmentType: " <> unpack wat
  parseJSON wat = typeMismatch "AttachmentType" wat


instance ToJSON AirlineTravelClassType where
  toJSON ECONOMY     = String "economy"
  toJSON BUSINESS    = String "business"
  toJSON FIRST_CLASS = String "first_class"

instance FromJSON AirlineTravelClassType where
  parseJSON (String "economy")     = pure ECONOMY
  parseJSON (String "business")    = pure BUSINESS
  parseJSON (String "first_class") = pure FIRST_CLASS
  parseJSON (String wat)           = fail $ "Wrong String for AirlineTravelClassType: " <> unpack wat
  parseJSON wat = typeMismatch "AirlineTravelClassType" wat


instance ToJSON AirlineUpdateType where
  toJSON DELAY        = String "delay"
  toJSON GATE_CHANGE  = String "gate_change"
  toJSON CANCELLATION = String "cancellation"

instance FromJSON AirlineUpdateType where
  parseJSON (String "delay")        = pure DELAY
  parseJSON (String "gate_change")  = pure GATE_CHANGE
  parseJSON (String "cancellation") = pure CANCELLATION
  parseJSON (String wat)            = fail $ "Wrong String for AirlineUpdateType: " <> unpack wat
  parseJSON wat = typeMismatch "AirlineUpdateType" wat


instance ToJSON ReferralSource where
  toJSON SHORTLINK  = String "SHORTLINK"

instance FromJSON ReferralSource where
  parseJSON (String "SHORTLINK")  = pure SHORTLINK
  parseJSON (String wat)          = fail $ "Wrong String for ReferralSource: " <> unpack wat
  parseJSON wat = typeMismatch "ReferralSource" wat


instance ToJSON ListStyle where
  toJSON ListCOMPACT = String "compact"
  toJSON ListLARGE   = String "large"

instance FromJSON ListStyle where
  parseJSON (String "compact") = pure ListCOMPACT
  parseJSON (String "large")   = pure ListLARGE
  parseJSON (String wat)       = fail $ "Wrong String for ListStyle: " <> unpack wat
  parseJSON wat = typeMismatch "ListStyle" wat


instance ToJSON PaymentType where
  toJSON FIXED_AMOUNT    = String "FIXED_AMOUNT"
  toJSON FLEXIBLE_AMOUNT = String "FLEXIBLE_AMOUNT"

instance FromJSON PaymentType where
  parseJSON (String "FIXED_AMOUNT")    = pure FIXED_AMOUNT
  parseJSON (String "FLEXIBLE_AMOUNT") = pure FLEXIBLE_AMOUNT
  parseJSON (String wat)               = fail $ "Wrong String for PaymentType: " <> unpack wat
  parseJSON wat = typeMismatch "PaymentType" wat


instance ToJSON RequestedUserInfoType where
  toJSON SHIPPING_ADDRESS = String "shipping_address"
  toJSON CONTACT_NAME     = String "contact_name"
  toJSON CONTACT_PHONE    = String "contact_phone"
  toJSON CONTACT_EMAIL    = String "contact_email"

instance FromJSON RequestedUserInfoType where
  parseJSON (String "shipping_address") = pure SHIPPING_ADDRESS
  parseJSON (String "contact_name")     = pure CONTACT_NAME
  parseJSON (String "contact_phone")    = pure CONTACT_PHONE
  parseJSON (String "contact_email")    = pure CONTACT_EMAIL
  parseJSON (String wat) = fail $ "Wrong String for RequestedUserInfoType: " <> unpack wat
  parseJSON wat          = typeMismatch "RequestedUserInfoType" wat
