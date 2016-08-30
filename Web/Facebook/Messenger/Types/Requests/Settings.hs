module Web.Facebook.Messenger.Types.Requests.Settings where

import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)


-- ---------------------- --
--  SEND SETTING REQUEST  --
-- ---------------------- --

-- POST request to --->  https://graph.facebook.com/v2.6/me/thread_settings?access_token=PAGE_ACCESS_TOKEN
data SettingsRequest =
  GreetingText
    { greeting :: SettingsGreeting }
  | GetStartedButton
    { getstarted :: [GetStartedButtonPayload] } -- can only be one (for some reason it's an array...)
  | PersistentMenu
    { persistent :: [PersistentMenuItem] } -- limited to 5 (title limit 30 for persistent menu)
  deriving (Eq, Show)

newtype SettingsGreeting =
    SettingsGreeting { greeting_text :: Text } -- Greeting text (UTF8 160 char limit)
  deriving (Eq, Show)

newtype GetStartedButtonPayload =
    GetStartedButtonPayload { getstarted_payload :: Text } -- This data will be sent back to you via webhook.
  deriving (Eq, Show)

data PersistentMenuItem = PersistentMenuItemURL { menuitem_title :: Text -- 30 char limit
                                                , menuitem_url   :: Text }
                                            -- This URL is opened in a mobile browser when the button is tapped
                        | PersistentMenuItemPostback { menuitem_title :: Text -- 30 char limimt
                                                     , menuitem_payload :: Text }
                                            -- This data will be sent back to you via webhook (1000 char limit)
  deriving (Eq, Show)


-- ------------------------ --
--  SEND SETTING INSTANCES  --
-- ------------------------ --

instance ToJSON SettingsRequest where
    toJSON (GreetingText greeting) = object [ "setting_type" .= String "greeting"
                                            , "greeting" .= greeting
                                            ]
    toJSON (GetStartedButton calls) = object [ "setting_type" .= String "call_to_actions"
                                             , "thread_state" .= String "new_thread"
                                             , "call_to_actions" .= calls
                                             ]
    toJSON (PersistentMenu calls) = object [ "setting_type" .= String "call_to_actions"
                                           , "thread_state" .= String "existing_thread"
                                           , "call_to_actions" .= calls
                                           ]

instance ToJSON SettingsGreeting where
    toJSON (SettingsGreeting txt) = object [ "text" .= txt ]

instance ToJSON GetStartedButtonPayload where
    toJSON (GetStartedButtonPayload payload) = object [ "payload" .= payload ]


instance ToJSON PersistentMenuItem where
    toJSON (PersistentMenuItemURL title url) = object [ "type" .= String "web_url"
                                                      , "title" .= title
                                                      , "url" .= url ]
    toJSON (PersistentMenuItemPostback title payload) = object [ "type" .= String "postback"
                                                               , "title" .= title
                                                               , "payload" .= payload ]


instance FromJSON SettingsRequest where
    parseJSON (Object o) = GreetingText <$> o .: "greeting"
                       <|> GetStartedButton <$> o .: "call_to_actions"
                       <|> PersistentMenu <$> o .: "call_to_actions"
    parseJSON wat = typeMismatch "SettingsRequest" wat

instance FromJSON SettingsGreeting where
    parseJSON (Object o) = SettingsGreeting <$> o .: "text"
    parseJSON wat        = typeMismatch "SettingsGreeting" wat

instance FromJSON GetStartedButtonPayload where
    parseJSON (Object o) = GetStartedButtonPayload <$> o .: "payload"
    parseJSON wat        = typeMismatch "GetStartedButtonPayload" wat

instance FromJSON PersistentMenuItem where
    parseJSON (Object o) = PersistentMenuItemURL <$> o .: "title"
                                                 <*> o .: "url"
                       <|> PersistentMenuItemPostback <$> o .: "title"
                                                      <*> o .: "payload"
    parseJSON wat = typeMismatch "PersistentMenuItem" wat
