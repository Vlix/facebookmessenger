module Web.Facebook.Messenger.Types.Callbacks.AccountLink where


import Data.Text
import Data.Aeson
import Data.Aeson.Types     (typeMismatch)


-- -------------------------- --
--  ACCOUNT LINKING CALLBACK  --
-- -------------------------- --

data FBCallbackAccountLink = FBCallbackAccountLink
    { fbcb_account_status :: FBCallbackAccountLinkStatus -- LINKED or UNLINKED
    , fbcb_account_code   :: Maybe Text -- Value of pass-through authorization_code provided in the Linking Account flow
    }

data FBCallbackAccountLinkStatus = LINKED | UNLINKED
    deriving Show

-- --------------------------- --
--  ACCOUNT LINKING INSTANCES  --
-- --------------------------- --

instance FromJSON FBCallbackAccountLink where
    parseJSON (Object o) = FBCallbackAccountLink <$> o .: "status"
                                                 <*> o .:? "authorization_code"
    parseJSON wat = typeMismatch "FBCallbackAccountLink" wat

instance FromJSON FBCallbackAccountLinkStatus where
    parseJSON (String "linked")   = pure LINKED
    parseJSON (String "unlinked") = pure UNLINKED
    parseJSON wat = typeMismatch "FBCallbackAccountLinkStatus" wat


instance ToJSON FBCallbackAccountLink where
    toJSON (FBCallbackAccountLink status code) = object [ "status" .= status
                                                        , "authorization_code" .= code
                                                        ]

instance ToJSON FBCallbackAccountLinkStatus where
    toJSON LINKED   = String "linked"
    toJSON UNLINKED = String "unlinked"
