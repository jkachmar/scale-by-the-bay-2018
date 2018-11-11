module Test.JSONSpec where

import           Data.Proxy
import           Test.Aeson.GenericSpecs
import           Test.Hspec

--
import           Server                  (Admin, BasicUser, Moderator, User)

spec :: Spec
spec = do
  describe "Round Trip JSON tests" $ do
    context "for the User data type"      $ roundtripADTSpecs (Proxy @User)
    context "for the BasicUser data type" $ roundtripSpecs (Proxy @BasicUser)
    context "for the Moderator data type" $ roundtripSpecs (Proxy @Moderator)
    context "for the Admin data type"     $ roundtripSpecs (Proxy @Admin)
