module Test.JSONSpec where

-- External modules
import           Data.Proxy
import           Test.Aeson.GenericSpecs
import           Test.Hspec

-- Local modules
import           Server                  (Admin, BasicUser, Moderator, User)

spec :: Spec
spec = do
  describe "roundtrip tests" $ do
    roundtripADTSpecs (Proxy @User)
    roundtripSpecs (Proxy @BasicUser)
    roundtripSpecs (Proxy @Moderator)
    roundtripSpecs (Proxy @Admin)
