module Web.Facebook.Messenger.Types.Requests.Settings where

import Control.Applicative  ((<|>))
import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)


-- ---------------------- --
--  SEND SETTING REQUEST  --
-- ---------------------- --

-- POST request to --->  https://graph.facebook.com/v2.6/me/thread_settings?access_token=PAGE_ACCESS_TOKEN
data FBSettingsRequest =
  FBGreetingTextRequest
    { fbsettings_greeting :: FBSettingsGreeting }
  | FBGetStartedButtonRequest
    { fbsettings_getstarted :: [FBGetStartedButtonPayload] } -- can only be one (for some reason it's an array...)
  | FBPersistentMenuRequest
    { fbsettings_persistent :: [FBSettingsMenuItem] } -- limited to 5 (title limit 30 for persistent menu)
  deriving (Eq, Show)

newtype FBSettingsGreeting =
    FBSettingsGreeting { fbsettings_greeting_text :: Text } -- Greeting text (UTF8 160 char limit)
  deriving (Eq, Show)

newtype FBGetStartedButtonPayload =
    FBGetStartedButtonPayload { fbsettings_getstarted_payload :: Text } -- This data will be sent back to you via webhook.
  deriving (Eq, Show)

data FBSettingsMenuItem = FBSettingsMenuItemURL { fbsettings_menuitem_weburl_title :: Text -- 30 char limit
                                                , fbsettings_menuitem_weburl_url   :: Text }
                                            -- This URL is opened in a mobile browser when the button is tapped
                        | FBSettingsMenuItemPostback { fbsettings_menuitem_postback_title :: Text -- 30 char limimt
                                                     , fbsettings_menuitem_postback_payload :: Text }
                                            -- This data will be sent back to you via webhook (1000 char limit)
  deriving (Eq, Show)


-- ------------------------ --
--  SEND SETTING INSTANCES  --
-- ------------------------ --

instance ToJSON FBSettingsRequest where
    toJSON (FBGreetingTextRequest greeting) = object [ "setting_type" .= String "greeting"
                                                     , "greeting" .= greeting
                                                     ]
    toJSON (FBGetStartedButtonRequest calls) = object [ "setting_type" .= String "call_to_actions"
                                                      , "thread_state" .= String "new_thread"
                                                      , "call_to_actions" .= calls
                                                      ]
    toJSON (FBPersistentMenuRequest calls) = object [ "setting_type" .= String "call_to_actions"
                                                    , "thread_state" .= String "existing_thread"
                                                    , "call_to_actions" .= calls
                                                    ]

instance ToJSON FBSettingsGreeting where
    toJSON (FBSettingsGreeting txt) = object [ "text" .= txt ]

instance ToJSON FBGetStartedButtonPayload where
    toJSON (FBGetStartedButtonPayload payload) = object [ "payload" .= payload ]


instance ToJSON FBSettingsMenuItem where
    toJSON (FBSettingsMenuItemURL title url) = object [ "type" .= String "web_url"
                                                      , "title" .= title
                                                      , "url" .= url ]
    toJSON (FBSettingsMenuItemPostback title payload) = object [ "type" .= String "postback"
                                                               , "title" .= title
                                                               , "payload" .= payload ]


instance FromJSON FBSettingsRequest where
    parseJSON (Object o) = FBGreetingTextRequest <$> o .: "greeting"
                       <|> FBGetStartedButtonRequest <$> o .: "call_to_actions"
                       <|> FBPersistentMenuRequest <$> o .: "call_to_actions"
    parseJSON wat = typeMismatch "FBSettingsRequest" wat

instance FromJSON FBSettingsGreeting where
    parseJSON (Object o) = FBSettingsGreeting <$> o .: "text"
    parseJSON wat        = typeMismatch "FBSettingsGreeting" wat

instance FromJSON FBGetStartedButtonPayload where
    parseJSON (Object o) = FBGetStartedButtonPayload <$> o .: "payload"
    parseJSON wat        = typeMismatch "FBGetStartedButtonPayload" wat

instance FromJSON FBSettingsMenuItem where
    parseJSON (Object o) = FBSettingsMenuItemURL <$> o .: "title"
                                                 <*> o .: "url"
                       <|> FBSettingsMenuItemPostback <$> o .: "title"
                                                      <*> o .: "payload"
    parseJSON wat = typeMismatch "FBSettingsMenuItem" wat
