module Web.Facebook.Messenger.Types.Callbacks.AccountLink where


import           Data.Aeson
import qualified Data.HashMap.Strict  as HM
import           Data.Text

import Web.Facebook.Messenger.Types.Static


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
  parseJSON = withObject "AccountLink" $ \o ->
    case HM.lookup "status" o of
      Just "linked"   -> AccountLink <$> o .:? "authorization_code"
      Just "unlinked" -> pure AccountUnlink
      Just wat -> fail $ "Unexpected status value in AccountLink object: " `mappend` show wat
      _ -> fail "No status field in AccountLink object."


instance ToJSON AccountLink where
  toJSON (AccountLink code) = object' [ "status"             .=! String "linked"
                                      , "authorization_code" .=!! code
                                      ]
  toJSON AccountUnlink = object [ "status" .= String "unlinked"]
