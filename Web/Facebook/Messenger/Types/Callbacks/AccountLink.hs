module Web.Facebook.Messenger.Types.Callbacks.AccountLink where


import           Data.Text
import           Data.Aeson
import           Data.Aeson.Types     (typeMismatch)
import qualified Data.HashMap.Strict  as HM

-- -------------------------- --
--  ACCOUNT LINKING CALLBACK  --
-- -------------------------- --

-- Value of pass-through authorization_code provided in the Linking Account flow
data AccountLink = AccountLink { account_code :: Maybe Text }
                 | AccountUnlink

  deriving (Eq, Show)

-- --------------------------- --
--  ACCOUNT LINKING INSTANCES  --
-- --------------------------- --

instance FromJSON AccountLink where
  parseJSON (Object o) = case HM.lookup "status" o of
    Just "linked"   -> AccountLink <$> o .:? "authorization_code"
    Just "unlinked" -> pure AccountUnlink
    Just wat -> fail $ "Unexpected status value in AccountLink object: " `mappend` show wat
  parseJSON wat = typeMismatch "AccountLink" wat


instance ToJSON AccountLink where
  toJSON (AccountLink code) = object [ "status"             .= String "linked"
                                     , "authorization_code" .= code
                                     ]
  toJSON AccountUnlink = object [ "status" .= String "unlinked"]
