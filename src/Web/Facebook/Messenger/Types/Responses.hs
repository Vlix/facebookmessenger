{-|
Module      : Web.Facebook.Messenger.Types.Responses
Copyright   : (c) Felix Paulusma, 2016
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : semi-experimental

This module contains most of the responses of the Facebook Messenger API.
-}
module Web.Facebook.Messenger.Types.Responses (
  -- * Common Responses
  -- ** Sender API Responses
  MessageResponse (..)
  , SenderActionResponse (..)
  -- ** Messenger Profile API Responses
  , SuccessResponse (..)
  , GetProfileResponse (..)
  -- ** Error Response
  , ErrorResponse (..)
  , ErrorDetails (..)
  -- * Other Responses
  -- ** Attachment Upload API Response
  , AttachmentUploadResponse (..)
  -- ** User Profile API Response
  , UserProfileResponse (..)
  -- ** Messenger Code API Response
  , MessengerCodeResponse (..)
  -- ** Account Linking API Response
  , AccountLinkingResponse (..)
  -- ** Check-out Update Callback Response
  , CheckoutUpdateResponse (..)
  , Shipping (..)
  -- ** Thread Control Response
  , ThreadControlResponse (..)
  -- ** Other Responses
  , DataResponse (..)
  , DomainWhitelistingResponse
  , SecondaryReceiverResponse
  , SecondaryReceiverElement (..)
  , TagResponse
  , TagElement (..)
  )
where


import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (catMaybes)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, unpack)

import Web.Facebook.Messenger.Types.Requests (AppId)
import Web.Facebook.Messenger.Types.Requests.Extra (PriceObject)
import Web.Facebook.Messenger.Types.Requests.Settings (ProfileRequest (..))
import Web.Facebook.Messenger.Types.Static


-- -------------------- --
--  FACEBOOK RESPONSES  --
-- -------------------- --

-- | This is a response to a standard Send API Request.
data MessageResponse = MessageResponse
    { mrRecipientId :: PSID -- Unique ID for the user
    , mrMessageId :: Text -- Unique ID for the message
    , mrAttachmentId :: Maybe Text
    -- ^ Please note that this ID is private and only the page that originally sent the attachment can reuse it.
    } deriving (Eq, Show, Read, Ord)

-- | This is a response to a Sender Action returning the Page-Scoped ID of the receiving user.
newtype SenderActionResponse =
          SenderActionResponse { sarRecipientId :: PSID }
  deriving (Eq, Show, Read, Ord)

-- | This is a response from the Attachement Upload API.
-- `Text` is a unique ID for the reusable attachment.
newtype AttachmentUploadResponse =
          AttachmentUploadResponse {aurAttachmentId :: Text}
  deriving (Eq, Show, Read, Ord)

-- | This is a standard Error response
newtype ErrorResponse = ErrorResponse { erError :: ErrorDetails }
  deriving (Eq, Show, Ord)

-- | Specifics pertaining to the `Error`
data ErrorDetails = ErrorDetails
    { erMessage :: Text -- ^ Readable explanation of the error
    , erType :: Text -- ^ Category of the error
    , erCode :: Int -- ^ Error code
    , erErrorSubcode :: Maybe Int -- ^ Error subcode
    , erFbtraceId :: Maybe Text -- ^ Back-end trace code for FB to help in debugging
    } deriving (Eq, Ord)

-- | This is a response to a `ProfileRequest` or an `AccountUnlinkRequest`
--
-- * `Text` should be \"success\" for a `ProfileRequest`.
-- * `Text` should be \"unlink account success\" for a `AccountUnlinkRequest`.
newtype SuccessResponse =
          SuccessResponse { res_result :: Text }
  deriving (Eq, Show, Read, Ord)

-- | This is a response to a User Profile API request
data UserProfileResponse = UserProfileResponse
    { uprFirstName :: Maybe Text -- ^ First Name
    , uprLastName :: Maybe Text -- ^ Last Name
    , uprProfilePic :: Maybe URL -- ^ URL to profile pic
    , uprLocale :: Maybe Text -- ^ format: en_US
    , uprTimezone :: Maybe Int  -- ^ GMT +/- Int (e.g @-7@)
    , uprGender :: Maybe Text -- ^ \"male\" or \"female\"
    , uprIsPaymentEnabled :: Maybe Bool -- ^ Is the user eligible to receive messenger platform payment messages
    , uprLastAdReferral :: Maybe Text -- ^ Last ad the user was referred by
    } deriving (Eq, Show, Read, Ord)

-- | This is a response to a PSID retrieval request
--
-- @
--   https://graph.facebook.com/v2.6/me?access_token=<PAGE_ACCESS_TOKEN>\
--     &fields=recipient \
--     &account_linking_token=<ACCOUNT_LINKING_TOKEN>
-- @
data AccountLinkingResponse = AccountLinkingResponse
    { alrId :: PageID -- ^ Page ID
    , alrRecipient :: PSID -- ^ PSID of the user
    } deriving (Eq, Show, Read, Ord)

-- | This is the response to be used when receiving a `CheckoutUpdate` callback
--
-- __THIS SHOULD BE PUT IN THE RESPONSE TO THE__ `CheckoutUpdate` __CALLBACK WITH THE__ @200 OK@
newtype CheckoutUpdateResponse =
          CheckoutUpdateResponse { cuShipping :: [Shipping] }
  deriving (Eq, Show, Read, Ord)

-- | These are the shipping options for the CheckoutUpdateResponse
data Shipping = Shipping
    { sOptionId :: Text
    , sOptionTitle :: Text
    , sPriceList :: [PriceObject]
    } deriving (Eq, Show, Read, Ord)

-- | When successful returns a `True`
newtype ThreadControlResponse =
          ThreadControlResponse { threadSuccess :: Bool }
  deriving (Eq, Show, Read, Ord)

-- | Response of a GET request to
--
-- @https://graph.facebook.com/v2.6/me/messenger_profile?fields=<PROPERTIES_LIST>&access_token=<PAGE_ACCESS_TOKEN>@
--
-- where \<PROPERTIES_LIST\> is a comma-seperated list of the following strings (without quotation marks):
--
-- * "account_linking_url"
-- * "persistent_menu"
-- * "get_started"
-- * "greeting"
-- * "whitelisted_domains"
-- * "payment_settings"
-- * "target_audience"
-- * "home_url"
newtype GetProfileResponse =
          GetProfileResponse { profileResponse :: ProfileRequest }
  deriving (Eq, Show, Read, Ord)

-- | Generic wrapper around objects with a @\"data\"@ field containing an array of something.
newtype DataResponse a = DataResponse { resData :: [a] }
  deriving (Eq, Show, Read, Ord)

-- | This is the response of a GET request for whitelisted domains
type DomainWhitelistingResponse = DataResponse Text

-- | This is the response of a GET request for secondary receivers
type SecondaryReceiverResponse = DataResponse SecondaryReceiverElement

-- | A certain app that's a second receiver at that moment
data SecondaryReceiverElement = SecondaryReceiverElement
    { srId :: Maybe AppId -- ^ The App ID of a second receiver app
    , srName :: Maybe Text -- ^ The name of the App
    } deriving (Eq, Show, Read, Ord)

-- | This is the response of a GET request for retrieving tags
type TagResponse = DataResponse TagElement

-- | Describes a certain `MessageTag`
data TagElement = TagElement
    { teTag :: MessageTag -- ^ Which `MessageTag`
    , teDescription :: Text -- ^ Description of said `MessageTag`
    } deriving (Eq, Show, Read, Ord)

-- | This is the response of a POST request to the Messenger Code API.
-- The URL is from where you can download your Messenger Code.
--
-- __This is not a permanent URI__; you should download and cache the image as soon as possible.
--
-- Tip: You can replace the image in the center with any image you want. It won't affect the functionality of the code.
newtype MessengerCodeResponse = MessengerCodeResponse { mcUri :: URL }
  deriving (Eq, Show, Read, Ord)


-- -------------------- --
--  RESPONSE INSTANCES  --
-- -------------------- --

instance FromJSON MessageResponse where
  parseJSON = withObject "MessageResponse" $ \o ->
      MessageResponse <$> o .: "recipient_id"
                      <*> o .: "message_id"
                      <*> o .:? "attachment_id"

instance FromJSON SenderActionResponse where
  parseJSON = withObject "SenderActionResponse" $ \o ->
      SenderActionResponse <$> o .: "recipient_id"

instance FromJSON AttachmentUploadResponse where
  parseJSON = withObject "AttachmentUploadResponse" $ \o ->
      AttachmentUploadResponse <$> o .: "attachment_id"

instance FromJSON ErrorResponse where
  parseJSON = withObject "ErrorResponse" $ \o ->
      ErrorResponse <$> o .: "error"

instance FromJSON ErrorDetails where
  parseJSON = withObject "ErrorDetails" $ \o ->
      ErrorDetails <$> o .: "message"
                    <*> o .: "type"
                    <*> o .: "code"
                    <*> o .:? "error_subcode"
                    <*> o .:? "fbtrace_id"

-- SHOW INSTANCE OF ERROR RESPONSE --
-- SHOW INSTANCE OF ERROR RESPONSE --
instance Show ErrorDetails where
  show (ErrorDetails msg typ code subcode trace) =
      "Facebook Error Response: \"" ++ unpack msg ++ "\""
         ++ " - Error code" ++ slashSub ++ ": " ++ show code ++ showSub
         ++ " - Error type: " ++ unpack typ ++ maybetrace
    where
      (slashSub,showSub) = maybe ("","") makeTup subcode
      makeTup x  = ("/subcode"," / " ++ show x)
      maybetrace = maybe "" ((++) " >>> Trace ID: " . unpack) trace
-- SHOW INSTANCE OF ERROR RESPONSE --
-- SHOW INSTANCE OF ERROR RESPONSE --

instance FromJSON SuccessResponse where
  parseJSON = withObject "SuccessResponse" $ \o ->
      SuccessResponse <$> o .: "result"

instance FromJSON UserProfileResponse where
  parseJSON = withObject "UserProfileResponse" $ \o -> do
      ad <- o .:? "last_ad_referral" :: Parser (Maybe Object)
      let setAd = ad >>= HM.lookup "ad_id" >>= go
      UserProfileResponse <$> o .:? "first_name"
                      <*> o .:? "last_name"
                      <*> o .:? "profile_pic"
                      <*> o .:? "locale"
                      <*> o .:? "timezone"
                      <*> o .:? "gender"
                      <*> o .:? "is_payment_enabled"
                      <*> pure setAd
    where go x = case x of
                  String s -> Just s
                  _ -> Nothing

instance FromJSON AccountLinkingResponse where
  parseJSON = withObject "AccountLinkingResponse" $ \o ->
      AccountLinkingResponse <$> o .: "id"
                             <*> o .: "recipient"

instance FromJSON CheckoutUpdateResponse where
  parseJSON = withObject "CheckoutUpdateResponse" $ \o ->
      CheckoutUpdateResponse <$> o .: "shipping"

instance FromJSON Shipping where
  parseJSON = withObject "Shipping" $ \o ->
      Shipping <$> o .: "option_id"
               <*> o .: "option_title"
               <*> o .: "price_list"

instance FromJSON ThreadControlResponse where
  parseJSON = withObject "ThreadControlResponse" $ \o ->
      ThreadControlResponse <$> o .: "success"

instance FromJSON GetProfileResponse where
  parseJSON = withObject "GetProfileResponse" $ \o -> do
      x <- o .: "data" :: Parser [Object]
      case x of
        (obj:_) -> GetProfileResponse <$> parseJSON (Object obj)
        _ -> fail "received empty data array"

instance FromJSON a => FromJSON (DataResponse a) where
  parseJSON = withObject "DataResponse" $ \o ->
      DataResponse <$> o .: "data"

instance FromJSON SecondaryReceiverElement where
  parseJSON = withObject "SecondaryReceiverElement" $ \o ->
      SecondaryReceiverElement <$> o .:? "id"
                        <*> o .:? "name"

instance FromJSON TagElement where
  parseJSON = withObject "TagElement" $ \o ->
      TagElement <$> o .: "tag"
                 <*> o .: "description"

instance FromJSON MessengerCodeResponse where
  parseJSON = withObject "MessengerCodeResponse" $ \o ->
      MessengerCodeResponse <$> o .: "uri"



instance ToJSON MessageResponse where
  toJSON (MessageResponse recipId msgId attId) =
      object' [ "recipient_id" .=! recipId
              , "message_id" .=! msgId
              , "attachment_id" .=!! attId
              ]

instance ToJSON SenderActionResponse where
  toJSON (SenderActionResponse recipId) =
      object ["recipient_id" .= recipId]

instance ToJSON AttachmentUploadResponse where
  toJSON (AttachmentUploadResponse attId) =
      object ["attachment_id" .= attId]

instance ToJSON ErrorResponse where
  toJSON (ErrorResponse err) = object ["error" .= err]

instance ToJSON ErrorDetails where
  toJSON (ErrorDetails msg typ code subcode trace) =
      object' [ "message" .=! msg
              , "type" .=! typ
              , "code" .=! code
              , "error_subcode" .=!! subcode
              , "fbtrace_id" .=!! trace
              ]

instance ToJSON SuccessResponse where
  toJSON (SuccessResponse result) = object ["result" .= result]

instance ToJSON UserProfileResponse where
  toJSON (UserProfileResponse fn ln pp loc tz gen pay lastAd) =
      object' [ "first_name" .=!! fn
              , "last_name" .=!! ln
              , "profile_pic" .=!! pp
              , "locale" .=!! loc
              , "timezone" .=!! tz
              , "gender" .=!! gen
              , "is_payment_enabled" .=!! pay
              , "last_ad_referral" .=!! fmap mkAd lastAd
              ]
    where mkAd adId = object [ "source" .= toJSON ADS
                             , "type" .= String "OPEN_THREAD"
                             , "ad_id" .= adId
                             ]

instance ToJSON AccountLinkingResponse where
  toJSON (AccountLinkingResponse ident recipient) =
      object [ "id" .= ident
             , "recipient" .= recipient
             ]

instance ToJSON CheckoutUpdateResponse where
  toJSON (CheckoutUpdateResponse shipping) =
      object ["shipping" .= shipping]

instance ToJSON Shipping where
  toJSON (Shipping ident title list) =
      object [ "option_id" .= ident
             , "option_title" .= title
             , "price_list" .= list
             ]

instance ToJSON ThreadControlResponse where
  toJSON (ThreadControlResponse b) = object ["success" .= b]

instance ToJSON GetProfileResponse where
  toJSON (GetProfileResponse pr) = object [ "data" .= [go] ]
    where go = toJSON $ object $ catMaybes allFields
          allFields = [ greet
                      , start
                      , persist
                      , whitelist
                      , accountLink
                      , paySettings
                      , audience
                      , homeUrl
                      ]
          greet = (.=) "greeting" <$> prGreeting pr
          start = (.=) "get_started" <$> prGetStarted pr
          persist = (.=) "persistent_menu" <$> prPersistentMenu pr
          whitelist = (.=) "whitelisted_domains" <$> prWhitelistedDomains pr
          accountLink = (.=) "account_linking_url" <$> prAccountLinkingUrl pr
          paySettings = (.=) "payment_settings" <$> prPaymentSettings pr
          audience = (.=) "target_audience" <$> prTargetAudience pr
          homeUrl = (.=) "home_url" <$> prHomeUrl pr

instance ToJSON a => ToJSON (DataResponse a) where
  toJSON (DataResponse d) = object ["data" .= d]

instance ToJSON SecondaryReceiverElement where
  toJSON (SecondaryReceiverElement ident name) =
      object' [ "id" .=!! ident
              , "name" .=!! name
              ]

instance ToJSON TagElement where
  toJSON (TagElement tag desc) =
      object [ "tag" .= tag
             , "description" .= desc
             ]

instance ToJSON MessengerCodeResponse where
  toJSON (MessengerCodeResponse uri) = object ["uri" .= uri]

{-
--------------------- old error codes ---------------------
Internal Errors
Code    Message
2       Send message failure. Internal server error

Rate Limited Errors
Code    Message
4       Application request limit reached
4       Too many send requests to phone numbers

Bad Parameter Errors
Code    Message
100     Invalid fbid.
100     No matching user found

Access Token Errors
Code    Message
190     Invalid OAuth access token.

Permission Errors
Code    Message
200     This person isn't receiving messages from you right now.
200     Cannot message users who are not admins, developers or testers of the app until pages_messaging permission is reviewed and the app is live.
200     Cannot message users who are not admins, developers or testers of the app until pages_messaging_phone_number permission is reviewed and the app is live.
200     Requires phone matching access fee to be paid by this page unless the recipient user is an admin, developer, or tester of the app.

User Block Errors
Code    Message
551     This person isn't receiving messages from you right now

Account Linking Errors
Code    Message
10303   Invalid account_linking_token
--------------------- old error codes ---------------------



Internal Errors
Code  Subcode Message
1200  --      Temporary send message failure. Please try again later.

Limit Errors
Code  Subcode Message
4     2018022 Too many send requests to phone numbers
100   2018109 Attachment size exceeds allowable limit
613   --      Calls to this API have exceeded the rate limit

Bad Parameter Errors
Code  Subcode Message
100   --      Invalid fbid.
100   2018001 No matching user found
(100  2018032 Invalid Data [like sending en empty string])

Access Token Errors
Code  Message
190   Invalid OAuth access token.

Permission Errors

Permission errors can occur for multiple reasons but generally fit into two main categories:
A specific permission hasn't been approved
The user hasn't opted-in to receiving messages from the page by using a Messenger entry point, or has deleted the conversation thread.

Code  Subcode Message
10    2018065 This message is sent outside of allowed window. You need page_messaging_subscriptions permission to be able to do it.
10    2018108 This Person Cannot Receive Messages: This person isn't receiving messages from you right now.
200   1545041 Message Not Sent: This person isn't receiving messages from you right now.
200/10 2018028 Cannot message users who are not admins, developers or testers of the app until pages_messaging permission is reviewed and the app is live.
200   2018027 Cannot message users who are not admins, developers or testers of the app until pages_messaging_phone_number permission is reviewed and the app is live.
200   2018021 Requires phone matching access fee to be paid by this page unless the recipient user is an admin, developer, or tester of the app.

Account-Linking Errors
Code  Message
10303 Invalid account_linking_token

-}