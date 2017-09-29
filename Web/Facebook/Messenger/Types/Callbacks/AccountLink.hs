module Web.Facebook.Messenger.Types.Callbacks.AccountLink
  ( AccountLink (..) )
where


import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import Data.Text

import Web.Facebook.Messenger.Types.Static


-- -------------------------- --
--  ACCOUNT LINKING CALLBACK  --
-- -------------------------- --

-- Value of pass-through authorization_code provided in the Linking Account flow
data AccountLink = AccountLink { authCode :: Maybe Text }
                 | AccountUnlink
  deriving (Eq, Show)

-- --------------------------- --
--  ACCOUNT LINKING INSTANCES  --
-- --------------------------- --

instance FromJSON AccountLink where
  parseJSON = withObject "AccountLink" $ \o -> do
    status <- o .: "status" :: Parser Text
    case status of
      "unlinked" -> pure AccountUnlink
      "linked" -> AccountLink <$> o .:? "authorization_code"
      wat -> fail $ "AccountLink: unexpected \"status\" string: " `mappend` unpack wat


instance ToJSON AccountLink where
  toJSON AccountUnlink = object ["status" .= String "unlinked"]
  toJSON (AccountLink code) =
      object' [ "status" .=! String "linked"
              , "authorization_code" .=!! code
              ]
