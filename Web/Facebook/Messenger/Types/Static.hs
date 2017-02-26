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
_    .=!! Nothing  = Nothing
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
  parseJSON = withText "SenderActionType" $ \s ->
    case s of
      "mark_seen"  -> pure MARK_SEEN
      "typing_on"  -> pure TYPING_ON
      "typing_off" -> pure TYPING_OFF
      wat          -> fail $ "Wrong String for SenderActionType: " <> unpack wat


instance ToJSON NotificationType where
  toJSON REGULAR     = String "REGULAR"
  toJSON SILENT_PUSH = String "SILENT_PUSH"
  toJSON NO_PUSH     = String "NO_PUSH"

instance FromJSON NotificationType where
  parseJSON = withText "NotificationType" $ \s ->
    case s of
      "REGULAR"     -> pure REGULAR
      "SILENT_PUSH" -> pure SILENT_PUSH
      "NO_PUSH"     -> pure NO_PUSH
      wat           -> fail $ "Wrong String for NotificationType: " <> unpack wat


instance ToJSON WebViewHeightRatioType where
  toJSON COMPACT = String "compact"
  toJSON TALL    = String "tall"
  toJSON FULL    = String "full"

instance FromJSON WebViewHeightRatioType where
  parseJSON = withText "WebViewHeightRatioType" $ \s ->
    case s of
      "compact" -> pure COMPACT
      "tall"    -> pure TALL
      "full"    -> pure FULL
      wat       -> fail $ "Wrong String for WebViewHeightRatioType: " <> unpack wat


instance ToJSON AttachmentType where
  toJSON IMAGE = String "image"
  toJSON VIDEO = String "video"
  toJSON AUDIO = String "audio"
  toJSON FILE  = String "file"

instance FromJSON AttachmentType where
  parseJSON = withText "AttachmentType" $ \s ->
    case s of
      "image" -> pure IMAGE
      "audio" -> pure AUDIO
      "video" -> pure VIDEO
      "file"  -> pure FILE
      wat     -> fail $ "Wrong String for AttachmentType: " <> unpack wat


instance ToJSON AirlineTravelClassType where
  toJSON ECONOMY     = String "economy"
  toJSON BUSINESS    = String "business"
  toJSON FIRST_CLASS = String "first_class"

instance FromJSON AirlineTravelClassType where
  parseJSON = withText "AirlineTravelClassType" $ \s ->
    case s of
      "economy"     -> pure ECONOMY
      "business"    -> pure BUSINESS
      "first_class" -> pure FIRST_CLASS
      wat           -> fail $ "Wrong String for AirlineTravelClassType: " <> unpack wat


instance ToJSON AirlineUpdateType where
  toJSON DELAY        = String "delay"
  toJSON GATE_CHANGE  = String "gate_change"
  toJSON CANCELLATION = String "cancellation"

instance FromJSON AirlineUpdateType where
  parseJSON = withText "AirlineUpdateType" $ \s ->
    case s of
      "delay"        -> pure DELAY
      "gate_change"  -> pure GATE_CHANGE
      "cancellation" -> pure CANCELLATION
      wat            -> fail $ "Wrong String for AirlineUpdateType: " <> unpack wat


instance ToJSON ReferralSource where
  toJSON SHORTLINK  = String "SHORTLINK"

instance FromJSON ReferralSource where
  parseJSON = withText "ReferralSource" $ \s ->
    case s of
      "SHORTLINK"  -> pure SHORTLINK
      wat          -> fail $ "Wrong String for ReferralSource: " <> unpack wat


instance ToJSON ListStyle where
  toJSON ListCOMPACT = String "compact"
  toJSON ListLARGE   = String "large"

instance FromJSON ListStyle where
  parseJSON = withText "ListStyle" $ \s ->
    case s of
      "compact" -> pure ListCOMPACT
      "large"   -> pure ListLARGE
      wat       -> fail $ "Wrong String for ListStyle: " <> unpack wat


instance ToJSON PaymentType where
  toJSON FIXED_AMOUNT    = String "FIXED_AMOUNT"
  toJSON FLEXIBLE_AMOUNT = String "FLEXIBLE_AMOUNT"

instance FromJSON PaymentType where
  parseJSON = withText "PaymentType" $ \s ->
    case s of
      "FIXED_AMOUNT"    -> pure FIXED_AMOUNT
      "FLEXIBLE_AMOUNT" -> pure FLEXIBLE_AMOUNT
      wat               -> fail $ "Wrong String for PaymentType: " <> unpack wat


instance ToJSON RequestedUserInfoType where
  toJSON SHIPPING_ADDRESS = String "shipping_address"
  toJSON CONTACT_NAME     = String "contact_name"
  toJSON CONTACT_PHONE    = String "contact_phone"
  toJSON CONTACT_EMAIL    = String "contact_email"

instance FromJSON RequestedUserInfoType where
  parseJSON = withText "RequestedUserInfoType" $ \s ->
    case s of
      "shipping_address" -> pure SHIPPING_ADDRESS
      "contact_name"     -> pure CONTACT_NAME
      "contact_phone"    -> pure CONTACT_PHONE
      "contact_email"    -> pure CONTACT_EMAIL
      wat                -> fail $ "Wrong String for RequestedUserInfoType: " <> unpack wat
