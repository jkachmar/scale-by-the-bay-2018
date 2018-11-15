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
    roundtripAndGoldenADTSpecs (Proxy @User)
    roundtripAndGoldenSpecs (Proxy @BasicUser)
    roundtripAndGoldenSpecs (Proxy @Moderator)
    roundtripAndGoldenSpecs (Proxy @Admin)
