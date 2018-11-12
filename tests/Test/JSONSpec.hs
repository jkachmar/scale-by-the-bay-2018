module Test.JSONSpec where

-- External modules
import           Data.Proxy
import           Test.Aeson.GenericSpecs
import           Test.Hspec

-- Local modules
import           Server                  (Admin, BasicUser, Moderator, User)

spec :: Spec
spec = do
  describe "Round Trip JSON tests" $ do
    context "for the User data type" $
      roundtripADTSpecs (Proxy @User)

    context "for the BasicUser data type" $
      roundtripADTSpecs (Proxy @BasicUser)

    context "for the Moderator data type" $
      roundtripADTSpecs (Proxy @Moderator)

    context "for the Admin data type" $
      roundtripADTSpecs (Proxy @Admin)
