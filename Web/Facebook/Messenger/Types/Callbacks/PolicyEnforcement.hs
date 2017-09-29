module Web.Facebook.Messenger.Types.Callbacks.PolicyEnforcement
  ( PolicyEnforcement (..) )
where


import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import Data.Text


-- | This callback is sent when a policy enforcement action is taken on the page it manages
data PolicyEnforcement = Block { peReason :: Text }
                       | Unblock
  deriving (Eq, Show)


-- --------------------------- --
--  CHECKOUT UPDATE INSTANCES  --
-- --------------------------- --

instance ToJSON PolicyEnforcement where
  toJSON Unblock = object ["action" .= String "unblock"]
  toJSON (Block reason) =
      object [ "action" .= String "block"
             , "reason" .= reason
             ]

instance FromJSON PolicyEnforcement where
  parseJSON = withObject "PolicyEnforcement" $ \o -> do
      action <- o .: "action" :: Parser Text
      case action of
        "unblock" -> pure Unblock
        "block" -> Block <$> o .: "reason"
        wat -> fail $ mappend "PolicyEnforcement: wrong string in \"action\" field: " $ show wat
