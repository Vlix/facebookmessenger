module Web.Facebook.Messenger.Types.Responses where


import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)


-- -------------------- --
--  FACEBOOK RESPONSES  --
-- -------------------- --

-- | This is a response to a standard Send API Request
data MessageResponse =
    MessageResponse
    { res_message_recipient_id  :: Text -- Unique ID for the user
    , res_message_message_id    :: Text -- Unique ID for the message
    , res_message_attachment_id :: Maybe Text -- Unique ID for the reusable attachment
    } deriving (Eq, Show)

data SenderActionResponse =
  SenderActionResponse
    { sar_message_recipient_id :: Text } -- Unique ID for the user 
  deriving (Eq, Show)

-- | This is a standard Error response
newtype ErrorRes = ErrorRes { res_error :: ErrorResponse } 
  deriving (Eq, Show)

data ErrorResponse = ErrorResponse
    { error_message    :: Text
    , error_type       :: Text
    , error_code       :: Int
    --, error_data :: Maybe Text
    , error_fbtrace_id :: Maybe Text
    }
  deriving Eq

-- | This is a response to the Thread Settings requests
newtype SuccessResponse = SuccessResponse { res_result :: Text } -- At successful request
  deriving (Eq, Show)

-- | This is a response to User Profile Reference requests
data UserAPIResponse = UserAPIResponse
    { userapi_first_name  :: Maybe Text -- First Name
    , userapi_last_name   :: Maybe Text -- Last Name
    , userapi_profile_pic :: Maybe Text -- URL to profile pic
    , userapi_locale      :: Maybe Text -- format: en_US
    , userapi_timezone    :: Maybe Int  -- GMT +/- Int 
    , userapi_gender      :: Maybe Text
    }
  deriving (Eq, Show)

-- | This is a response to an Account Linking request
data AccountLinkingResponse =
    AccountLinkingResponse
    { linking_id        :: Text
    , linking_recipient :: Text
    } deriving (Eq, Show)

-- SHOW INSTANCE OF ERROR RESPONSE --
instance Show ErrorResponse where
    show (ErrorResponse msg typ code traceid) =
        "Facebook Error Response: \"" ++ show msg ++ "\" - Error code: " ++ show code ++ " - Error type: " ++ show typ ++ maybetrace traceid
      where
        maybetrace Nothing      = ""
        maybetrace (Just ident) = " >>> Trace ID: " ++ show ident
-- SHOW INSTANCE OF ERROR RESPONSE --


-- -------------------- --
--  RESPONSE INSTANCES  --
-- -------------------- --

instance FromJSON MessageResponse where
    parseJSON (Object o) = MessageResponse <$> o .: "recipient_id"
                                           <*> o .: "message_id"
                                           <*> o .:? "attachment_id"
    parseJSON wat = typeMismatch "MessageResponse" wat

instance FromJSON SenderActionResponse where
    parseJSON (Object o) = SenderActionResponse <$> o .: "recipient_id"
    parseJSON wat = typeMismatch "SenderActionResponse" wat

instance FromJSON ErrorRes where
    parseJSON (Object o) = ErrorRes <$> o .: "error"
    parseJSON wat        = typeMismatch "ErrorResponse" wat

instance FromJSON ErrorResponse where
    parseJSON (Object o) = ErrorResponse <$> o .: "message"
                                         <*> o .: "type"
                                         <*> o .: "code"
                                         <*> o .:? "fbtrace_id"
    parseJSON wat = typeMismatch "ErrorResponse" wat

instance FromJSON SuccessResponse where
    parseJSON (Object o) = SuccessResponse <$> o .: "result"
    parseJSON wat = typeMismatch "SuccessResponse" wat

instance FromJSON UserAPIResponse where
    parseJSON (Object o) = UserAPIResponse <$> o .:? "first_name"
                                           <*> o .:? "last_name"
                                           <*> o .:? "profile_pic"
                                           <*> o .:? "locale"
                                           <*> o .:? "timezone"
                                           <*> o .:? "gender"
    parseJSON wat = typeMismatch "UserAPIResponse" wat

instance FromJSON AccountLinkingResponse where
    parseJSON (Object o) = AccountLinkingResponse <$> o .: "id"
                                                  <*> o .: "recipient"
    parseJSON wat = typeMismatch "AccountLinkingResponse" wat


instance ToJSON MessageResponse where
    toJSON (MessageResponse recipient_id message_id attachment_id) = object [ "recipient_id"  .= recipient_id
                                                                            , "message_id"    .= message_id
                                                                            , "attachment_id" .= attachment_id
                                                                            ]

instance ToJSON SenderActionResponse where
    toJSON (SenderActionResponse recipient_id) = object [ "recipient_id" .= recipient_id ]

instance ToJSON ErrorRes where
    toJSON (ErrorRes err) = object [ "error" .= err ]

instance ToJSON ErrorResponse where
    toJSON (ErrorResponse message typ code fbtrace_id) = object [ "message"    .= message
                                                                , "type"       .= typ
                                                                , "code"       .= code
                                                                , "fbtrace_id" .= fbtrace_id
                                                                ]

instance ToJSON SuccessResponse where
    toJSON (SuccessResponse result) = object [ "result" .= result ]

instance ToJSON UserAPIResponse where
    toJSON (UserAPIResponse first_name last_name profile_pic locale timezone gender) =
        object [ "first_name"  .= first_name
               , "last_name"   .= last_name
               , "profile_pic" .= profile_pic
               , "locale"      .= locale
               , "timezone"    .= timezone
               , "gender"      .= gender
               ]

instance ToJSON AccountLinkingResponse where
    toJSON (AccountLinkingResponse ident recipient) = object [ "id"        .= ident
                                                             , "recipient" .= recipient
                                                             ]

{-

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

-}