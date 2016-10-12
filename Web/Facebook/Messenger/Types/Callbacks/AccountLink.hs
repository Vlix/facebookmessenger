module Web.Facebook.Messenger.Types.Callbacks.AccountLink where


import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)


-- -------------------------- --
--  ACCOUNT LINKING CALLBACK  --
-- -------------------------- --

data AccountLink = AccountLink
    { account_status :: AccountLinkStatus -- LINKED or UNLINKED
    , account_code   :: Maybe Text -- Value of pass-through authorization_code provided in the Linking Account flow
    }
  deriving (Eq, Show)

data AccountLinkStatus = LINKED | UNLINKED
  deriving (Eq, Show)

-- --------------------------- --
--  ACCOUNT LINKING INSTANCES  --
-- --------------------------- --

instance FromJSON AccountLink where
    parseJSON (Object o) = AccountLink <$> o .: "status"
                                       <*> o .:? "authorization_code"
    parseJSON wat = typeMismatch "AccountLink" wat

instance FromJSON AccountLinkStatus where
    parseJSON (String "linked")   = pure LINKED
    parseJSON (String "unlinked") = pure UNLINKED
    parseJSON wat = typeMismatch "AccountLinkStatus" wat


instance ToJSON AccountLink where
    toJSON (AccountLink status code) = object [ "status"             .= status
                                              , "authorization_code" .= code
                                              ]

instance ToJSON AccountLinkStatus where
    toJSON LINKED   = String "linked"
    toJSON UNLINKED = String "unlinked"
