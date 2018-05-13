{-|
Module      : Web.Facebook.Messenger.Types.Requests
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This module contains most of the requests to the Facebook Messenger API.
-}
module Web.Facebook.Messenger.Types.Requests (
  -- * Facebook Messenger API Requests
  --
  -- | Most requests are sent to the following URL (or a variation thereof):
  --
  -- @https:\/\/graph.facebook.com\/v2.12\/me\/@__{some method}__@?access_token=\<PAGE_ACCESS_TOKEN\>@__(&{some optional fields}={some values})__

  -- ** Send API Request
  --
  -- | POST to @.../messages?access_token=\<PAGE_ACCESS_TOKEN\>@
  sendRequest
  , sendRequestTag
  , SendRequest (..)
  , SenderActionRequest (..)
  , recipientID
  , recipientPhone
  , recipientRef
  , RequestRecipient (..)
  , RecipientID (..)
  , RecipientRef (..)
  , RecipientPhone (..)
  , RecipientName (..)
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
  -- |
  -- * /Pass Thread/: POST to @.../pass_thread_control?access_token=\<PAGE_ACCESS_TOKEN\>@
  -- * /Request Thread/: POST to @.../request_thread_control?access_token=\<PAGE_ACCESS_TOKEN\>@
  -- * /Take Thread/: POST to @.../take_thread_control?access_token=\<PAGE_ACCESS_TOKEN\>@
  , PassThreadControlRequest (..)
  , ThreadControlRequest (..)
  -- * Exported modules
  , module Web.Facebook.Messenger.Types.Requests.Extra
  , module Web.Facebook.Messenger.Types.Requests.Message
  , module Web.Facebook.Messenger.Types.Requests.Settings
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
import Web.Facebook.Messenger.Types.Static

-- ============================== --
--      SEND MESSAGE REQUEST      --
-- ============================== --

-- | The Send API is the main API used to send messages to users.
-- `SendRequest`s are used to interact with this API.
--
-- Some things to consider:
--
-- * When your app is in Development Mode, the Send API will only work for admins, developers and testers of the app.
-- After your app is approved for the @"pages_messaging"@ permission and is public, it will work for the general public.
-- * Only generic template messages can be sent with tags other than `ISSUE_RESOLUTION`.
-- `ISSUE_RESOLUTION` tag can be used with either generic template messages or text messages.
--
-- @https://developers.facebook.com/docs/messenger-platform/reference/send-api@
data SendRequest = SendRequest
    { srRecipient :: RequestRecipient -- ^ Recipient of the message
    , srMessage :: RequestMessage -- ^ Contents of the message
    , srMessagingType :: MessagingType -- ^ Type of message ('RESPONSE', 'UPDATE' or 'MESSAGE_TAG')
    , srNotificationType :: NotificationType -- ^ Optional; by default, messages will be a `REGULAR` push notification type
    , srTag :: Maybe MessageTag -- ^ Optional; to be used if you have a valid reason to send a message outside of the 24+1 window
    } deriving (Eq, Show, Read, Ord)

-- | Shortcut constructor for a default `SendRequest`
-- (no `MessageTag` and with a `REGULAR` `NotificationType`
-- and 'RESPONSE' 'MessagingType')
sendRequest :: RequestRecipient -> RequestMessage -> SendRequest
sendRequest recipient msg = SendRequest recipient msg RESPONSE REGULAR Nothing

-- | Shortcut constructor for a default `SendRequest` with a message tag.
-- (`REGULAR` `NotificationType`)
sendRequestTag :: RequestRecipient -> RequestMessage -> MessageTag -> SendRequest
sendRequestTag recipient msg = SendRequest recipient msg MESSAGE_TAG REGULAR . Just

-- | Set typing indicators or send read receipts using the Send API, to let users know you are processing their request.
--
-- @https://developers.facebook.com/docs/messenger-platform/send-messages/sender-actions@
data SenderActionRequest = SenderActionRequest
    { sarRecipient :: RequestRecipient -- ^ Recipient of the Sender Action
    , sarSenderAction :: SenderActionType -- ^ `TYPING_ON` \/ `TYPING_OFF` \/ `MARK_SEEN`
    } deriving (Eq, Show, Read, Ord)

-- | The Attachment Upload API allows you to upload assets that can be sent in messages at a later time.
-- This allows you to avoid the need to upload commonly used files multiple times.
--
-- After uploading just use the @"attachment_id"@ when sending it to users.
--
-- @https://developers.facebook.com/docs/messenger-platform/reference/attachment-upload-api@
data AttachmentUploadRequest = AttachmentUploadRequest
    { aurType :: AttachmentType -- ^ Type of file to be uploaded
    , aurUrl :: URL -- ^ URL of the file to upload
    } deriving (Eq, Show, Read, Ord)

-- | The recipient of a message.
data RequestRecipient = RID RecipientID -- ^ Facebook Page-Scoped ID
                      | RPhone RecipientPhone -- ^ Phone-number (requires specific permission. US only)
                      | RRef RecipientRef
                      -- ^ Ref to use when a user enters your bot through the Checkbox Plugin.\
                      -- After a successful response, switch to using the received PSID.
  deriving (Eq, Show, Read, Ord)

-- | Constructor for making a @regular PSID@ `RequestRecipient`
recipientID :: Text -> RequestRecipient
recipientID = RID . RecipientID

-- | Identifying a user by their Page-Scoped ID.
-- This means that the IDs are unique per user per page.
newtype RecipientID = RecipientID { recipId :: Text }
  deriving (Eq, Show, Read, Ord)

-- | Constructor for making a @Phone number@ `RequestRecipient`.
-- This is only for US based users and your bot needs the @"pages_messaging_phone_number"@ permission.
--
-- https://developers.facebook.com/docs/messenger-platform/identity/customer-matching
recipientPhone :: Text -> Maybe RecipientName -> RequestRecipient
recipientPhone number = RPhone . RecipientPhone number

-- | Identifying a user by their phone number. Only for USA users/developers.
--
-- Format: @+1 (555) 857-6309@
data RecipientPhone = RecipientPhone
  { recipPhone :: Text
  , recipName :: Maybe RecipientName
  } deriving (Eq, Show, Read, Ord)

-- | Name provided if possible when matching customers using phone numbers.
data RecipientName = RecipientName
  { rnFirstName :: Text
  , rnLastName :: Text
  } deriving (Eq, Show, Read, Ord)

-- | Constructor for making a @"user_ref"@ `RequestRecipient`.
-- Use this only when a user enters your bot through the @Checkbox plugin@
--
-- https://developers.facebook.com/docs/messenger-platform/discovery/checkbox-plugin
recipientRef :: Text -> RequestRecipient
recipientRef = RRef . RecipientRef

-- | Identifying a user by the @user_ref@ you receive when the user enters the bot through a referral.
--
-- You can call the Send API to start messaging the user using the @user_ref@ field in recipient.
-- Note that this field is the same as the unique @user_ref@ param used in rendering the plugin and used in confirming the opt-in.
newtype RecipientRef = RecipientRef { recipRef :: Text }
  deriving (Eq, Show, Read, Ord)


-- | In case you want to programmatically unlink someone from an account
--
-- Bottom of: @https://developers.facebook.com/docs/messenger-platform/identity/account-linking@
newtype AccountUnlinkRequest = AccountUnlinkRequest { aurPSID :: Text }
  deriving (Eq, Show, Read, Ord)


-- | This request will be responded to with a URI to an image of the Messenger Code.
--
-- @https://developers.facebook.com/docs/messenger-platform/discovery/messenger-codes@
data MessengerCodeRequest = MessengerCodeRequest
    { mcrImageSize :: Maybe Int -- ^ between 100px - 2000px (default == 1000)
    , mcrData :: Maybe MessengerCodeRef -- ^ Optional custom parameter to add to the code (for analytics or UX purposes)
    } deriving (Eq, Show, Read, Ord)

-- | Optional custom parameter. 250 char limit @[a-zA-Z0-9+/=-.:_]@
newtype MessengerCodeRef = MessengerCodeRef { mcRef :: Text }
  deriving (Eq, Show, Read, Ord)

-- | Part of the handover protocol, pass thread control allows you to pass thread control from your app to another app.
-- The app that will receive thread ownership will receive a @"pass_thread_control"@ webhook event.
--
-- @https://developers.facebook.com/docs/messenger-platform/reference/handover-protocol/pass-thread-control@
data PassThreadControlRequest = PassThreadControlRequest
    { pcrRecipient :: RecipientID -- ^ User who's thread is passed to another app
    , pcrTargetAppId :: AppId
    -- ^ The app ID of the Secondary Receiver to pass thread control to.
    -- (Required if the Primary Receiver is passing thread control.)
    -- To pass thread control to the Page inbox, use app ID __@263902037430900@__.
    , pcrMetaData :: Maybe Text -- ^ Metadata passed to the receiving app in the @"pass_thread_control"@ webhook event.
    } deriving (Eq, Show, Read, Ord)

-- | Part of the Handover Protocol, take/request thread control allows the Primary Receiver app
-- to take/request control of a specific thread from a Secondary Receiver app.
-- The Secondary Receiver app will receive a @"take_thread_control" or "request_thread_control"@
-- webhook event when it loses thread control or is requested to be passed thread control.
--
-- * @https://developers.facebook.com/docs/messenger-platform/handover-protocol/request-thread-control@
-- * @https://developers.facebook.com/docs/messenger-platform/handover-protocol/take-thread-control@
data ThreadControlRequest = ThreadControlRequest
    { tcrRecipient :: RecipientID -- ^ User who's thread is taken control over or of which control is requested
    , tcrMetaData :: Maybe Text -- ^ Metadata passed back to the secondary app in the webhook event.
    } deriving (Eq, Show, Read, Ord)


-- ------------------------ --
--  SEND MESSAGE INSTANCES  --
-- ------------------------ --

instance ToJSON SendRequest where
  toJSON (SendRequest recpnt message msgType notification_type tag) =
      object' [ "recipient" .=! recpnt
              , "message" .=! message
              , "messaging_type" .=! msgType
              , mDefault "notification_type" REGULAR notification_type
              , "tag" .=!! tag
              ]

instance ToJSON SenderActionRequest where
  toJSON (SenderActionRequest recpnt saction) =
      object [ "recipient" .= recpnt
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
  toJSON (RecipientPhone phone mName) =
      object' [ "phone" .=! phone
              , "name" .=!! mName
              ]

instance ToJSON RecipientName where
  toJSON (RecipientName fName lName) =
      object [ "first_name" .= fName
             , "last_name" .= lName
             ]


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
  toJSON (PassThreadControlRequest recpnt appid metadata) =
      object' [ "recipient" .=! recpnt
              , "target_app_id" .=! appid
              , "metadata" .=!! metadata
              ]

instance ToJSON ThreadControlRequest where
  toJSON (ThreadControlRequest recpnt metadata) =
      object' [ "recipient" .=! recpnt
              , "metadata" .=!! metadata
              ]



instance FromJSON SendRequest where
  parseJSON = withObject "SendRequest" $ \o ->
      SendRequest <$> o .: "recipient"
                  <*> o .: "message"
                  <*> o .: "messaging_type"
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
                     <*> o .:? "name"

instance FromJSON RecipientName where
  parseJSON = withObject "RecipientName" $ \o ->
      RecipientName <$> o .: "first_name"
                    <*> o .: "last_name"

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

instance FromJSON ThreadControlRequest where
  parseJSON = withObject "ThreadControlRequest" $ \o ->
      ThreadControlRequest <$> o .: "recipient"
                               <*> o .:? "metadata"
