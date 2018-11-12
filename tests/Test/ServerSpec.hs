module Test.ServerSpec where

-- -- External modules
-- import           Data.Proxy
-- import           Servant.API.Generic
-- import           Servant.QuickCheck
-- import           Servant.Server.Generic
import           Test.Hspec

-- -- Internal modules
-- import           Server

spec :: Spec
spec = describe "the server" $ do
  it "doesn't do anything!" $ do
    True `shouldBe` True

  -- it "follows best practices" $ do
  --   let api    = genericApi (Proxy @Routes)
  --       server = genericServer routeHandlers
  --   withServantServer api (pure server) $ \baseUrl ->
  --     serverSatisfies api baseUrl defaultArgs $
  --           not500
  --       <%> onlyJsonObjects
  --       <%> notAllowedContainsAllowHeader
  --       <%> mempty
