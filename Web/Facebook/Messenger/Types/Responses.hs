module Web.Facebook.Messenger.Types.Responses where


import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)


-- -------------------- --
--  FACEBOOK RESPONSES  --
-- -------------------- --

data FBMessageResponse =
    FBMessageResponse
    { fbres_message_recipient_id :: Text -- Unique ID for the user
    , fbres_message_message_id   :: Text -- Unique ID for the message
    }
  | FBSenderActionResponse
    { fbres_message_recipient_id :: Text } -- Unique ID for the user 
  deriving (Eq, Show)

newtype FacebookErrorResponse = FacebookErrorResponse { fbres_message_error :: FBErrorResponse } 
  deriving (Eq, Show)

data FBErrorResponse = FBErrorResponse
    { fbres_error_message    :: Text
    , fbres_error_type       :: Text
    , fbres_error_code       :: Int
    --, fbres_error_error_data :: Maybe Text
    , fbres_error_fbtrace_id :: Maybe Text
    }
  deriving Eq

newtype FBSuccessResponse = FBSuccessResponse { fbres_settings_result :: Text } -- At successful request
  deriving (Eq, Show)

data FacebookUserAPIResponse = FacebookUserAPIResponse
    { fbres_userapi_first_name  :: Maybe Text -- First Name
    , fbres_userapi_last_name   :: Maybe Text -- Last Name
    , fbres_userapi_profile_pic :: Maybe Text -- URL to profile pic
    , fbres_userapi_locale      :: Maybe Text -- format: en_US
    , fbres_userapi_timezone    :: Maybe Int  -- GMT +/- Int 
    , fbres_userapi_gender      :: Maybe Text
    }
  deriving (Eq, Show)


-- SHOW INSTANCE OF ERROR RESPONSE --
instance Show FBErrorResponse where
    show (FBErrorResponse msg typ code traceid) =
        "Facebook Error Response: \"" ++ show msg ++ "\" - Error code: " ++ show code ++ " - Error type: " ++ show typ ++ maybetrace traceid
      where
        maybetrace Nothing      = ""
        maybetrace (Just ident) = " >>> Trace ID: " ++ show ident
-- SHOW INSTANCE OF ERROR RESPONSE --

-- -------------------- --
--  RESPONSE INSTANCES  --
-- -------------------- --

instance FromJSON FBMessageResponse where
    parseJSON (Object o) = FBMessageResponse <$> o .: "recipient_id"
                                             <*> o .: "message_id"
                       <|> FBSenderActionResponse <$> o .: "recipient_id"
    parseJSON wat = typeMismatch "FBMessageResponse" wat

instance FromJSON FacebookErrorResponse where
    parseJSON (Object o) = FacebookErrorResponse <$> o .: "error"
    parseJSON wat        = typeMismatch "FacebookErrorResponse" wat

instance FromJSON FBErrorResponse where
    parseJSON (Object o) = FBErrorResponse <$> o .: "message"
                                           <*> o .: "type"
                                           <*> o .: "code"
                                           <*> o .:? "fbtrace_id"
    parseJSON wat = typeMismatch "FBErrorResponse" wat

instance FromJSON FBSuccessResponse where
    parseJSON (Object o) = FBSuccessResponse <$> o .: "result"
    parseJSON wat = typeMismatch "FBSuccessResponse" wat

instance FromJSON FacebookUserAPIResponse where
    parseJSON (Object o) = FacebookUserAPIResponse <$> o .:? "first_name"
                                                   <*> o .:? "last_name"
                                                   <*> o .:? "profile_pic"
                                                   <*> o .:? "locale"
                                                   <*> o .:? "timezone"
                                                   <*> o .:? "gender"
    parseJSON wat = typeMismatch "FacebookUserAPIResponse" wat

instance ToJSON FBMessageResponse where
    toJSON (FBMessageResponse recipient_id message_id) = object [ "recipient_id" .= recipient_id
                                                                , "message_id" .= message_id
                                                                ]
    toJSON (FBSenderActionResponse recipient_id) = object [ "recipient_id" .= recipient_id ]

instance ToJSON FBErrorResponse where
    toJSON (FBErrorResponse message typ code fbtrace_id) = object [ "message" .= message
                                                                  , "type" .= typ
                                                                  , "code" .= code
                                                                  , "fbtrace_id" .= fbtrace_id
                                                                  ]

instance ToJSON FBSuccessResponse where
    toJSON (FBSuccessResponse result) = object [ "result" .= result ]

instance ToJSON FacebookUserAPIResponse where
    toJSON (FacebookUserAPIResponse first_name last_name profile_pic locale timezone gender) =
        object [ "first_name" .= first_name
               , "last_name" .= last_name
               , "profile_pic" .= profile_pic
               , "locale" .= locale
               , "timezone" .= timezone
               , "gender" .= gender
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