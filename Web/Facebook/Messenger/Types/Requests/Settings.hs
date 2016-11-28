module Web.Facebook.Messenger.Types.Requests.Settings where


import Control.Applicative  ((<|>))
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)
import Data.Text
import Data.HashMap.Strict  as HM


-- ---------------------- --
--  SEND SETTING REQUEST  --
-- ---------------------- --

type Domain = Text

-- | POST request to --->  https://graph.facebook.com/v2.6/me/thread_settings?access_token=PAGE_ACCESS_TOKEN
data SettingsRequest =
  GreetingText
    { greeting :: SettingsGreeting }
  | GetStartedButton
    { getstarted :: [GetStartedButtonPayload] } -- can only be one (for some reason it's an array...)
  | PersistentMenu
    { persistent :: [PersistentMenuItem] } -- limited to 5 (title limit 30 for persistent menu)
  | DomainWhitelistingAdd
    { whitelisted_domains :: [Domain] } -- Up to 10 domains allowed. A list of domains being used with URL Buttons and Messenger Extensions. All domains must be valid and use https.
  | DomainWhitelistingRemove
    { whitelisted_domains :: [Domain] }
  | AccountLinkingUrl
    { account_linking_url :: Text } -- URL to the account linking OAuth flow
  | AccountUnlinking -- Except use the DELETE method for this one
  | PaymentPrivacy
    { payment_privacy_url :: Text } -- This will appear in FB's payment dialogs and people will be able to view these terms.
  | PaymentPublicKey
    { payment_public_key :: Text } -- This is used to encrypt sensitive payment data sent to you.
  | PaymentAddTesters
    { payment_testers :: [Text] } -- A list of page scoped user id to be added as payment testers.
  | PaymentRemoveTesters
    { payment_testers :: [Text] } -- A list of page scoped user id to be added as payment testers.
  deriving (Eq, Show)

-- | Greeting text (UTF8 160 char limit)
newtype SettingsGreeting = SettingsGreeting { greeting_text :: Text }
  deriving (Eq, Show)

-- | This data will be sent back to you via webhook.
newtype GetStartedButtonPayload = GetStartedButtonPayload { getstarted_payload :: Text }
  deriving (Eq, Show)

data PersistentMenuItem =
  PersistentMenuItemURL { menuitem_title :: Text -- 30 char limit
                        , menuitem_url   :: Text }
-- This URL is opened in a mobile browser when the button is tapped
  | PersistentMenuItemPostback { menuitem_title   :: Text -- 30 char limimt
                               , menuitem_payload :: Text }
-- This data will be sent back to you via webhook (1000 char limit)
  deriving (Eq, Show)


-- ------------------------ --
--  SEND SETTING INSTANCES  --
-- ------------------------ --

instance ToJSON SettingsRequest where
  toJSON (GreetingText greeting) =
    object [ "setting_type" .= String "greeting"
           , "greeting"     .= greeting
           ]
  toJSON (GetStartedButton calls) =
    object [ "setting_type"    .= String "call_to_actions"
           , "thread_state"    .= String "new_thread"
           , "call_to_actions" .= calls
           ]
  toJSON (PersistentMenu calls) =
    object [ "setting_type"    .= String "call_to_actions"
           , "thread_state"    .= String "existing_thread"
           , "call_to_actions" .= calls
           ]
  toJSON (DomainWhitelistingAdd domains) =
    object [ "setting_type"        .= String "domain_whitelisting"
           , "whitelisted_domains" .= domains
           , "domain_action_type"  .= String "add"
           ]
  toJSON (DomainWhitelistingRemove domains) =
    object [ "setting_type"        .= String "domain_whitelisting"
           , "whitelisted_domains" .= domains
           , "domain_action_type"  .= String "remove"
           ]
  toJSON (AccountLinkingUrl url) =
    object [ "setting_type"        .= String "account_linking"
           , "account_linking_url" .= url
           ]
  toJSON AccountUnlinking = object [ "setting_type" .= String "account_linking" ]
  toJSON (PaymentPrivacy url) =
    object [ "setting_type"        .= String "payment"
           , "payment_privacy_url" .= url
           ]
  toJSON (PaymentPublicKey key) =
    object [ "setting_type"       .= String "payment"
           , "payment_public_key" .= key
           ]
  toJSON (PaymentAddTesters testers) =
    object [ "setting_type"            .= String "payment"
           , "payment_dev_mode_action" .= String "ADD"
           , "payment_testers"         .= testers
           ]
  toJSON (PaymentRemoveTesters testers) =
    object [ "setting_type"            .= String "payment"
           , "payment_dev_mode_action" .= String "REMOVE"
           , "payment_testers"         .= testers
           ]

instance ToJSON SettingsGreeting where
  toJSON (SettingsGreeting txt) = object [ "text" .= txt ]

instance ToJSON GetStartedButtonPayload where
  toJSON (GetStartedButtonPayload payload) = object [ "payload" .= payload ]


instance ToJSON PersistentMenuItem where
  toJSON (PersistentMenuItemURL title url) =
    object [ "type"  .= String "web_url"
           , "title" .= title
           , "url"   .= url
           ]
  toJSON (PersistentMenuItemPostback title payload) =
    object [ "type"    .= String "postback"
           , "title"   .= title
           , "payload" .= payload
           ]

instance FromJSON SettingsRequest where
  parseJSON (Object o) =
    case (mThreadState,mDomainAction,mSettingType) of
      (Just (String "new_thread"),_,_)      -> GetStartedButton <$> o .: "call_to_actions"
      (Just (String "existing_thread"),_,_) -> PersistentMenu <$> o .: "call_to_actions"
      (_,Just (String "add"),_)             -> DomainWhitelistingAdd <$> o .: "whitelisted_domains"
      (_,Just (String "remove"),_)           -> DomainWhitelistingRemove <$> o .: "whitelisted_domains"
      (_,_,Just (String "greeting"))        -> GreetingText <$> o .: "greeting"
      (_,_,Just (String "account_linking")) -> AccountLinkingUrl <$> o .: "account_linking_url"
                                           <|> pure AccountUnlinking
      (_,_,Just (String "payment"))         ->
        case HM.lookup "payment_dev_mode_action" o of
          Just (String "ADD")    -> PaymentAddTesters <$> o .: "payment_testers"
          Just (String "REMOVE") -> PaymentRemoveTesters <$> o .: "payment_testers"
          _ -> PaymentPrivacy <$> o .: "payment_privacy_url"
           <|> PaymentPublicKey <$> o .: "payment_public_key"
    where mThreadState  = HM.lookup "thread_state" o
          mDomainAction = HM.lookup "domain_action_type" o
          mSettingType  = HM.lookup "setting_type" o
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
