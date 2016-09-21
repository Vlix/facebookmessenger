module Web.Facebook.Messenger.Types.Static where


import Data.Aeson
import Data.Aeson.Types     (typeMismatch)
import Data.Monoid          ((<>))

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

data AttachmentType = IMAGE | VIDEO | AUDIO | FILE
  deriving (Eq, Show)

data AirlineTravelClassType = ECONOMY | BUSINESS | FIRST_CLASS
  deriving (Eq, Show)

data AirlineUpdateType = DELAY | GATE_CHANGE | CANCELLATION
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
    parseJSON (String wat)          = fail $ "Wrong String for SenderActionType: " <> show wat
    parseJSON wat = typeMismatch "SenderActionType" wat


instance ToJSON NotificationType where
    toJSON REGULAR     = String "REGULAR"
    toJSON SILENT_PUSH = String "SILENT_PUSH"
    toJSON NO_PUSH     = String "NO_PUSH"

instance FromJSON NotificationType where
    parseJSON (String "REGULAR")     = pure REGULAR
    parseJSON (String "SILENT_PUSH") = pure SILENT_PUSH
    parseJSON (String "NO_PUSH")     = pure NO_PUSH
    parseJSON (String wat)           = fail $ "Wrong String for NotificationType: " <> show wat
    parseJSON wat = typeMismatch "NotificationType" wat


instance ToJSON WebViewHeightRatioType where
    toJSON COMPACT = String "compact"
    toJSON TALL    = String "tall"
    toJSON FULL    = String "full"

instance FromJSON WebViewHeightRatioType where
    parseJSON (String "compact") = pure COMPACT
    parseJSON (String "tall")    = pure TALL
    parseJSON (String "full")    = pure FULL
    parseJSON (String wat)       = fail $ "Wrong String for WebViewHeightRatioType: " <> show wat
    parseJSON wat = typeMismatch "WebViewHeightRatioType" wat


instance ToJSON AttachmentType where
    toJSON IMAGE    = String "image"
    toJSON VIDEO    = String "video"
    toJSON AUDIO    = String "audio"
    toJSON FILE     = String "file"

instance FromJSON AttachmentType where
    parseJSON (String "image")    = pure IMAGE
    parseJSON (String "audio")    = pure AUDIO
    parseJSON (String "video")    = pure VIDEO
    parseJSON (String "file")     = pure FILE
    parseJSON (String wat)        = fail $ "Wrong String for AttachmentType: " <> show wat
    parseJSON wat = typeMismatch "AttachmentType" wat


instance ToJSON AirlineTravelClassType where
    toJSON ECONOMY     = String "economy"
    toJSON BUSINESS    = String "business"
    toJSON FIRST_CLASS = String "first_class"

instance FromJSON AirlineTravelClassType where
    parseJSON (String "economy")     = pure ECONOMY
    parseJSON (String "business")    = pure BUSINESS
    parseJSON (String "first_class") = pure FIRST_CLASS
    parseJSON (String wat)           = fail $ "Wrong String for AirlineTravelClassType: " <> show wat
    parseJSON wat = typeMismatch "AirlineTravelClassType" wat


instance ToJSON AirlineUpdateType where
    toJSON DELAY        = String "delay"
    toJSON GATE_CHANGE  = String "gate_change"
    toJSON CANCELLATION = String "cancellation"

instance FromJSON AirlineUpdateType where
    parseJSON (String "delay")        = pure DELAY
    parseJSON (String "gate_change")  = pure GATE_CHANGE
    parseJSON (String "cancellation") = pure CANCELLATION
    parseJSON (String wat)            = fail $ "Wrong String for AirlineUpdateType: " <> show wat
    parseJSON wat = typeMismatch "AirlineUpdateType" wat
