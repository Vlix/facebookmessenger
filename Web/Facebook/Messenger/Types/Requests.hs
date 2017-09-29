{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Web.Facebook.Messenger.Types.Requests
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This module contains most of the requests to the Facebook Messenger API.

TODO: add URLs of FB documentation to all requests if possible
-}
module Web.Facebook.Messenger.Types.Requests (
  -- * Facebook Messenger API Requests
  --
  -- | Most requests are sent to the following URL (or a variation thereof):
  -- 
  -- @https://graph.facebook.com/v2.6/me/@__{some method}__@?access_token=\<PAGE_ACCESS_TOKEN\>@__(&{some extra field(s)}={some value(s)})__

  -- ** Send API Request
  --
  -- | POST to @.../messages?access_token=\<PAGE_ACCESS_TOKEN\>@
  SendRequest (..)
  , SenderActionRequest (..)
  , recipientID
  , recipientPhone
  , recipientRef
  , RequestRecipient (..)
  , RecipientID (..)
  , RecipientPhone (..)
  , RecipientRef (..)
  -- ** Attachment Upload API
  --
  -- | POST to @.../message_attachments?access_token=\<PAGE_ACCESS_TOKEN\>@
  , AttachmentUploadRequest (..)
  -- ** Account Unlink Endpoint
  --
  -- | POST to @.../unlink_accounts?access_token=\<PAGE_ACCESS_TOKEN\>@
  , AccountUnlinkRequest (..)
  -- ** Messenger Code API
  --
  -- | POST to @.../messenger_codes?access_token=\<PAGE_ACCESS_TOKEN\>@
  , MessengerCodeRequest (..)
  , MessengerCodeRef (..)
  -- ** Handover Protocol
  --
  -- | /Pass Thread/: POST to @.../pass_thread_control?access_token=\<PAGE_ACCESS_TOKEN\>@
  -- /Take Thread/: POST to @.../take_thread_control?access_token=\<PAGE_ACCESS_TOKEN\>@
  , PassThreadControlRequest (..)
  , AppId (..)
  , TakeThreadControlRequest (..)
  -- * Exported modules
  , module Web.Facebook.Messenger.Types.Requests.Extra
  , module Web.Facebook.Messenger.Types.Requests.Message
  , module Web.Facebook.Messenger.Types.Requests.Settings
  , module Web.Facebook.Messenger.Types.Requests.Templates
  , module Web.Facebook.Messenger.Types.Static
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text, unpack)


import Web.Facebook.Messenger.Types.Requests.Extra
import Web.Facebook.Messenger.Types.Requests.Message
import Web.Facebook.Messenger.Types.Requests.Settings
import Web.Facebook.Messenger.Types.Requests.Templates
import Web.Facebook.Messenger.Types.Static

-- ============================== --
--      SEND MESSAGE REQUEST      --
-- ============================== --

-- | TODO: Explanation
--
-- N.B. Only generic template messages can be sent with tags other than `ISSUE_RESOLUTION`.
-- `ISSUE_RESOLUTION` tag can be used with either generic template messages or text messages.
data SendRequest = SendRequest
    { srRecipient :: RequestRecipient -- Recipient object
    , srMessage :: RequestMessage -- Message object
    , srNotificationType :: NotificationType -- Optional; by default, messages will be REGULAR push notification type
    , srTag :: Maybe MessageTag -- Optional; to be used if you have a valid reason to send a message outside of the 24+1 window
    } deriving (Eq, Show)

data SenderActionRequest = SenderActionRequest
    { sarRecipient :: RequestRecipient -- Recipient object
    , sarSenderAction :: SenderActionType -- Message state: TYPING_ON, TYPING_OFF, MARK_SEEN
    } deriving (Eq, Show)

-- POST request to ---> https://graph.facebook.com/v2.6/me/message_attachments?access_token=\<PAGE_ACCESS_TOKEN\>
-- This API is used to upload a file to FB once, and after just use the attachment_id to send it to other users
data AttachmentUploadRequest = AttachmentUploadRequest
    { aurType :: AttachmentType -- Type of file to be uploaded
    , aurUrl :: URL -- URL of the file to upload
    } deriving (Eq, Show)

-- | 
data RequestRecipient = RID RecipientID
                      | RPhone RecipientPhone
                      | RRef RecipientRef
  deriving (Eq, Show)

recipientID :: Text -> RequestRecipient
recipientID = RID . RecipientID

-- | Identifying a user by their Page-Scoped ID. 
-- This means that the IDs are unique per user per page.
newtype RecipientID = RecipientID { recipId :: Text } -- (PS)ID of recipient
  deriving (Eq, Show)

recipientPhone :: Text -> RequestRecipient
recipientPhone = RPhone . RecipientPhone

-- | Identifying a user by their phone number. Only for USA users/developers.
--
-- Format: +1 (555) 857-6309
newtype RecipientPhone = RecipientPhone { recipPhone :: Text }
  deriving (Eq, Show)

recipientRef :: Text -> RequestRecipient
recipientRef = RRef . RecipientRef

-- | Identifying a user by the @user_ref@ you receive when the user enters the bot through a referral.
--
-- You can call the Send API to start messaging the user using the @user_ref@ field in recipient.
-- Note that this field is the same as the unique @user_ref@ param used in rendering the plugin and used in confirming the opt-in.
newtype RecipientRef = RecipientRef { recipRef :: Text }
  deriving (Eq, Show)


-- | POST request to ---> https://graph.facebook.com/v2.6/me/unlink_accounts?access_token=\<PAGE_ACCESS_TOKEN\>
data AccountUnlinkRequest = AccountUnlinkRequest { aurPSID :: Text }
  deriving (Eq, Show)


-- | POST request to ---> https://graph.facebook.com/v2.6/me/messenger_codes?access_token=\<PAGE_ACCESS_TOKEN\>
data MessengerCodeRequest = MessengerCodeRequest
    { mcrImageSize :: Maybe Int -- ^ between 100px - 2000px (default == 1000)
    , mcrData :: Maybe MessengerCodeRef
    } deriving (Eq, Show)

newtype MessengerCodeRef = MessengerCodeRef { mcRef :: Text } -- ^ max 250 char [a-zA-Z0-9+/=-.:_]
  deriving (Eq, Show)

-- | POST request to ---> https://graph.facebook.com/v2.6/me/pass_thread_control?access_token=\<PAGE_ACCESS_TOKEN\>
data PassThreadControlRequest = PassThreadControlRequest
    { pcrRecipient :: RecipientID
    , pcrTargetAppId :: AppId
    , pcrMetaData :: Maybe Text
    } deriving (Eq, Show)

-- | POST request to ---> https://graph.facebook.com/v2.6/me/pass_thread_control?access_token=\<PAGE_ACCESS_TOKEN\>
data TakeThreadControlRequest = TakeThreadControlRequest
    { tcrRecipient :: RecipientID
    , tcrMetaData :: Maybe Text
    } deriving (Eq, Show)

-- | Newtype wrapper around Text, because the AppId is very different than anything else used as IDs in this package
newtype AppId = AppId Text deriving (Eq, Show, FromJSON, ToJSON)

-- ------------------------ --
--  SEND MESSAGE INSTANCES  --
-- ------------------------ --

instance ToJSON SendRequest where
  toJSON (SendRequest recipient message notification_type tag) =
      object' [ "recipient" .=! recipient
              , "message" .=! message
              , mDefault "notification_type" REGULAR notification_type
              , "tag" .=!! tag
              ]

instance ToJSON SenderActionRequest where
  toJSON (SenderActionRequest recipient saction) =
      object [ "recipient" .= recipient
             , "sender_action" .= saction
             ]

instance ToJSON AccountUnlinkRequest where
  toJSON (AccountUnlinkRequest psid) = object ["psid" .= psid]

instance ToJSON RequestRecipient where
  toJSON (RID x) = toJSON x
  toJSON (RRef x) = toJSON x
  toJSON (RPhone x) = toJSON x

instance ToJSON RecipientID where
  toJSON (RecipientID ident) = object [ "id" .= ident ]

instance ToJSON RecipientRef where
  toJSON (RecipientRef userRef) = object [ "user_ref" .= userRef]

instance ToJSON RecipientPhone where
  toJSON (RecipientPhone phone) = object [ "phone" .= phone ]

instance ToJSON AttachmentUploadRequest where
  toJSON (AttachmentUploadRequest typ url) =
      object ["message" .= msgObj]
    where msgObj = object ["attachment" .= uploadObj]
          uploadObj = object [ "type" .= typ
                             , "payload" .= payloadObj
                             ]
          payloadObj = object [ "is_reusable" .= Bool True
                              , "url" .= url
                              ]

instance ToJSON MessengerCodeRequest where
  toJSON (MessengerCodeRequest imgSize ref) =
      object' [ "type" .=! String "standard"
              , "image_size" .=!! imgSize
              , "data" .=!! ref
              ]

instance ToJSON MessengerCodeRef where
  toJSON x = object ["ref" .= mcRef x]

instance ToJSON PassThreadControlRequest where
  toJSON (PassThreadControlRequest recip appid metadata) =
      object' [ "recipient" .=! recip
              , "target_app_id" .=! appid
              , "metadata" .=!! metadata
              ]

instance ToJSON TakeThreadControlRequest where
  toJSON (TakeThreadControlRequest recip metadata) =
      object' [ "recipient" .=! recip
              , "metadata" .=!! metadata
              ]



instance FromJSON SendRequest where
  parseJSON = withObject "SendRequest" $ \o ->
      SendRequest <$> o .: "recipient"
                  <*> o .: "message"
                  <*> o .:? "notification_type" .!= REGULAR
                  <*> o .:? "tag"

instance FromJSON SenderActionRequest where
  parseJSON = withObject "SenderActionRequest" $ \o ->
      SenderActionRequest <$> o .: "recipient"
                          <*> o .: "sender_action"

instance FromJSON AccountUnlinkRequest where
  parseJSON = withObject "AccountUnlinkRequest" $ \o ->
      AccountUnlinkRequest <$> o .: "psid"

instance FromJSON RequestRecipient where
  parseJSON = withObject "RequestRecipient" $ \o ->
        RID <$> parseJSON (Object o)
    <|> RRef <$> parseJSON (Object o)
    <|> RPhone <$> parseJSON (Object o)

instance FromJSON RecipientID where
  parseJSON = withObject "RecipientID" $ \o ->
      RecipientID <$> o .: "id"

instance FromJSON RecipientRef where
  parseJSON = withObject "RecipientRef" $ \o ->
      RecipientRef <$> o .: "user_ref"

instance FromJSON RecipientPhone where
  parseJSON = withObject "RecipientPhone" $ \o ->
      RecipientPhone <$> o .: "phone"

instance FromJSON AttachmentUploadRequest where
  parseJSON = withObject "AttachmentUploadRequest" $ \o -> do
      msg <- o .: "message"
      att <- msg .: "attachment"
      typ <- att .: "type"
      pl <- att .: "payload"
      url <- pl .: "url"
      pure $ AttachmentUploadRequest typ url

instance FromJSON MessengerCodeRequest where
  parseJSON = withObject "MessengerCodeRequest" $ \o -> do
      typ <- o .: "type" :: Parser Text
      unless (typ == "standard") $
        fail $ "MessengerCodeRequest: \"type\" value not \"standard\": " `mappend` unpack typ
      MessengerCodeRequest <$> o .:? "image_size"
                           <*> o .:? "data"

instance FromJSON MessengerCodeRef where
  parseJSON = withObject "MessengerCodeRef" $ \o ->
      MessengerCodeRef <$> o .: "ref"

instance FromJSON PassThreadControlRequest where
  parseJSON = withObject "PassThreadControlRequest" $ \o ->
      PassThreadControlRequest <$> o .: "recipient"
                               <*> o .: "target_app_id"
                               <*> o .:? "metadata"

instance FromJSON TakeThreadControlRequest where
  parseJSON = withObject "TakeThreadControlRequest" $ \o ->
      TakeThreadControlRequest <$> o .: "recipient"
                               <*> o .:? "metadata"
