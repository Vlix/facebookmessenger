module Web.Facebook.Messenger.Types.Callbacks.PolicyEnforcement where


import Data.Aeson
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
  parseJSON = withObject "PolicyEnforcement" $ \o ->
      case HM.lookup "action" o of
        Just (String "unblock") -> pure Unblock
        Just (String "block") -> Block <$> o .: "reason"
        Just (String wat) -> fail $ mappend "PolicyEnforcement: wrong string in \"action\" field: " $ show wat
        Just wat -> fail $ mappend "PolicyEnforcement: wrong type in \"action\" field: " $ show wat
        Nothing -> fail "PolicyEnforcement: missing \"action\" field"
