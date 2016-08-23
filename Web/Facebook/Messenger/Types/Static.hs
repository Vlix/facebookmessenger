module Web.Facebook.Messenger.Types.Static where


import Data.Aeson
import Data.Aeson.Types     (typeMismatch)

data FBRequestSenderActionType = MARK_SEEN  -- Mark last message as read
                               | TYPING_ON  -- Turn typing indicators on
                               | TYPING_OFF -- Turn typing indicators off
                            -- Typing indicators are automatically turned off after 20 seconds
  deriving (Eq, Show)

data FBRequestNotificationType = REGULAR     -- Emits a sound/vibration and a phone notification
                               | SILENT_PUSH -- Emits a phone notification
                               | NO_PUSH     -- Emits neither
  deriving (Eq, Show)

data FBAttachmentType = IMAGE | VIDEO | AUDIO | FILE
  deriving (Eq, Show)

data FBAirlineTravelClassType = ECONOMY | BUSINESS | FIRST_CLASS
  deriving (Eq, Show)

data FBAirlineUpdateType = DELAY | GATE_CHANGE | CANCELLATION
  deriving (Eq, Show)


-- JSON instances

instance ToJSON FBRequestSenderActionType where
    toJSON MARK_SEEN  = String "mark_seen"
    toJSON TYPING_ON  = String "typing_on"
    toJSON TYPING_OFF = String "typing_off"

instance FromJSON FBRequestSenderActionType where
    parseJSON (String "mark_seen")  = pure MARK_SEEN
    parseJSON (String "typing_on")  = pure TYPING_ON
    parseJSON (String "typing_off") = pure TYPING_OFF
    parseJSON wat = typeMismatch "FBRequestSenderActionType" wat


instance ToJSON FBRequestNotificationType where
    toJSON REGULAR     = String "REGULAR"
    toJSON SILENT_PUSH = String "SILENT_PUSH"
    toJSON NO_PUSH     = String "NO_PUSH"

instance FromJSON FBRequestNotificationType where
    parseJSON (String "REGULAR")     = pure REGULAR
    parseJSON (String "SILENT_PUSH") = pure SILENT_PUSH
    parseJSON (String "NO_PUSH")     = pure NO_PUSH
    parseJSON wat = typeMismatch "FBRequestNotificationType" wat


instance ToJSON FBAttachmentType where
    toJSON IMAGE    = String "image"
    toJSON VIDEO    = String "video"
    toJSON AUDIO    = String "audio"
    toJSON FILE     = String "file"

instance FromJSON FBAttachmentType where
    parseJSON (String "image")    = pure IMAGE
    parseJSON (String "audio")    = pure AUDIO
    parseJSON (String "video")    = pure VIDEO
    parseJSON (String "file")     = pure FILE
    parseJSON wat = typeMismatch "FBAttachmentType" wat


instance ToJSON FBAirlineTravelClassType where
    toJSON ECONOMY     = String "economy"
    toJSON BUSINESS    = String "business"
    toJSON FIRST_CLASS = String "first_class"

instance FromJSON FBAirlineTravelClassType where
    parseJSON (String "economy")     = pure ECONOMY
    parseJSON (String "business")    = pure BUSINESS
    parseJSON (String "first_class") = pure FIRST_CLASS
    parseJSON (String _)             = fail "expected FBAirlineTravelClassType String"
    parseJSON wat = typeMismatch "FBAirlineTravelClassType" wat


instance ToJSON FBAirlineUpdateType where
    toJSON DELAY        = String "delay"
    toJSON GATE_CHANGE  = String "gate_change"
    toJSON CANCELLATION = String "cancellation"

instance FromJSON FBAirlineUpdateType where
    parseJSON (String "delay")        = pure DELAY
    parseJSON (String "gate_change")  = pure GATE_CHANGE
    parseJSON (String "cancellation") = pure CANCELLATION
    parseJSON (String _)              = fail "expected FBAirlineUpdateType String"
    parseJSON wat = typeMismatch "FBAirlineUpdateType" wat
