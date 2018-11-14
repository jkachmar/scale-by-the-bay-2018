module Server where

-- Miscellaneous external modules
import           Control.Monad                        (when)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8           as LBS8
import           Data.Proxy
import           Data.Text                            (Text)
import           Data.Time
import           GHC.Generics
import           Lens.Micro                           (mapped, (&), (.~), (?~))
import           System.Directory                     (doesFileExist,
                                                       removeFile)
import           Text.Pretty.Simple

-- Random data generation modules
import           Generic.Random                       (genericArbitraryRec,
                                                       genericArbitraryU,
                                                       uniform, withBaseCase)
import           Test.QuickCheck                      (Arbitrary (..), Gen,
                                                       generate)
import           Test.QuickCheck.Arbitrary.ADT        (ToADTArbitrary)
import           Test.QuickCheck.Instances            ()

-- Web server modules
import           Data.Swagger                         hiding
                                                       (SchemaOptions (..))
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.API
import           Servant.API.Generic
import           Servant.Server                       (Server, serve)
import           Servant.Server.Generic
import           Servant.Swagger                      (toSwagger)
import           Servant.Swagger.UI.ReDoc

--------------------------------------------------------------------------------
-- Application data model
--------------------------------------------------------------------------------
data User
  = Admin'     !Admin
  | Moderator' !Moderator
  | BasicUser' !BasicUser
  deriving (Eq, Generic, Show)

data Admin
  = Admin
  { aUsername     :: !Text
  , aPassword     :: !Text
  , aEmailAddress :: !Text
  , aActive       :: !Bool
  , aCreatedAt    :: !UTCTime
  , aUpdatedAt    :: !UTCTime
  , aPromotedBy   :: !(Maybe Admin)
  } deriving (Eq, Generic, Show)

data Moderator
  = Moderator
  { mUsername     :: !Text
  , mPassword     :: !Text
  , mEmailAddress :: !Text
  , mGoverns      :: ![Text]
  , mActive       :: !Bool
  , mCreatedAt    :: !UTCTime
  , mUpdatedAt    :: !UTCTime
  , mPromotedBy   :: !Admin
  } deriving (Eq, Generic, Show)

data BasicUser
  = BasicUser
  { buUsername     :: !Text
  , buPassword     :: !Text
  , buEmailAddress :: !Text
  , buActive       :: !Bool
  , buCreatedAt    :: !UTCTime
  , buUpdatedAt    :: !UTCTime
  } deriving (Eq, Generic, Show)

--------------------------------------------------------------------------------
-- Arbitrary data generation typeclass instances
--------------------------------------------------------------------------------
instance Arbitrary User where
  arbitrary :: Gen User
  arbitrary = genericArbitraryU

-- `ToADTArbitrary` creates arbitrary values for every constructor in an ADT
instance ToADTArbitrary User

instance Arbitrary BasicUser where
  arbitrary :: Gen BasicUser
  arbitrary = genericArbitraryU

instance Arbitrary Moderator where
  arbitrary :: Gen Moderator
  arbitrary = genericArbitraryU

instance Arbitrary Admin where
  arbitrary :: Gen Admin
  arbitrary = genericArbitraryRec uniform `withBaseCase` baseAdminCase
    where
      baseAdminCase :: Gen Admin
      baseAdminCase = Admin
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> pure Nothing

--------------------------------------------------------------------------------
-- JSON serialization/deserialization typeclass instances
--------------------------------------------------------------------------------
instance FromJSON User
instance ToJSON User

instance FromJSON BasicUser
instance ToJSON BasicUser

instance FromJSON Moderator
instance ToJSON Moderator

instance FromJSON Admin
instance ToJSON Admin

--------------------------------------------------------------------------------
-- Modified JSON serialization/deserialization typeclass instances
--------------------------------------------------------------------------------
idiomaticJsonOptions :: (String -> String) -> Options
idiomaticJsonOptions f = defaultOptions
  { constructorTagModifier = camelTo2 '_' . filter (not . (== '\''))
  , fieldLabelModifier = camelTo2 '_' . f
  , sumEncoding = ObjectWithSingleField
  , tagSingleConstructors = True
  }

-- instance FromJSON User where
--   parseJSON = genericParseJSON $ (idiomaticJsonOptions id)
--     { sumEncoding = UntaggedValue
--     }

-- instance ToJSON User where
--   toJSON = genericToJSON (idiomaticJsonOptions id)
--     { sumEncoding = UntaggedValue
--     }

-- instance FromJSON BasicUser where
--   parseJSON = genericParseJSON (idiomaticJsonOptions (drop 2))

-- instance ToJSON BasicUser where
--   toJSON = genericToJSON (idiomaticJsonOptions (drop 2))

-- instance FromJSON Moderator where
--   parseJSON = genericParseJSON (idiomaticJsonOptions (drop 1))

-- instance ToJSON Moderator where
--   toJSON = genericToJSON (idiomaticJsonOptions (drop 1))

-- instance FromJSON Admin where
--   parseJSON = genericParseJSON (idiomaticJsonOptions (drop 1))

-- instance ToJSON Admin where
--   toJSON = genericToJSON (idiomaticJsonOptions (drop 1))

--------------------------------------------------------------------------------
-- Routing definitions
--------------------------------------------------------------------------------
data UserRoutes route
  = UserRoutes
  -- Equivalent to a "/users" route that accepts GET requests and returns JSON
  { getUsers :: route
      :- Summary "Get a list of all users."
      :> Description "Get a list of all users currently registered with this \
                     \service."
      :> "users" :> Get '[JSON] [User]

  -- Equivalent to a "/users/basic" route that accepts GET requests and returns
  -- JSON
  , getBasicUsers :: route
      :- Summary "Get a list of all basic users."
      :> Description "Get a list of all users currently registered with this \
                     \service who are not moderators or administrators."
      :> "users" :> "basic" :> Get '[JSON] [BasicUser]

  -- Equivalent to a "/users/moderators" route that accepts GET requests and
  -- returns JSON
  , getModerators :: route
      :- Summary "Get a list of all moderators."
      :> Description "Get a list of all users classified as moderators \
                     \currently registered with this service."
      :> "users" :> "moderators" :> Get '[JSON] [Moderator]

  -- Equivalent to a "/users/admins" route that accepts GET requests and
  -- returns JSON
  , getAdmins :: route
      :- Summary "Get a list of all admins."
      :> Description "Get a list of all users classified as administrators \
                     \currently registered with this service."
      :> "users" :> "admins" :> Get '[JSON] [Admin]
  } deriving Generic

userRouteHandlers :: UserRoutes AsServer
userRouteHandlers = UserRoutes
  { getUsers      = liftIO $ generate arbitrary
  , getBasicUsers = liftIO $ generate arbitrary
  , getModerators = liftIO $ generate arbitrary
  , getAdmins     = liftIO $ generate arbitrary
  }

data Routes route
  = Routes
  { v1Routes :: route :- "v1" :> (ToServantApi UserRoutes)
  } deriving Generic

routeHandlers :: Routes AsServer
routeHandlers = Routes
  { v1Routes = genericServer userRouteHandlers
  }

--------------------------------------------------------------------------------
-- Swagger schema generation
--------------------------------------------------------------------------------
exampleAdmin :: Admin
exampleAdmin =
  let timestamp = UTCTime (fromGregorian 2018 1 1) 0
  in Admin
    { aUsername = "cvogt"
    , aPassword = "123456"
    , aEmailAddress = "cvogt@example.com"
    , aActive = True
    , aCreatedAt = timestamp
    , aUpdatedAt = timestamp
    , aPromotedBy = Nothing
    }

exampleModerator :: Moderator
exampleModerator =
  let timestamp = UTCTime (fromGregorian 2018 2 1) 0
  in Moderator
    { mUsername = "jkachmar"
    , mPassword = "123456"
    , mEmailAddress = "jkachmar@example.com"
    , mGoverns = ["Practical Haskell Demystified"]
    , mActive = True
    , mCreatedAt = timestamp
    , mUpdatedAt = timestamp
    , mPromotedBy = exampleAdmin
    }

exampleBasicUser :: BasicUser
exampleBasicUser =
  let timestamp = UTCTime (fromGregorian 2018 3 1) 0
  in BasicUser
    { buUsername = "serf"
    , buPassword = "123456"
    , buEmailAddress = "serf@example.com"
    , buActive = True
    , buCreatedAt = timestamp
    , buUpdatedAt = timestamp
    }

instance ToSchema User where
  declareNamedSchema user =
    let schemaOptions = fromAesonOptions $ idiomaticJsonOptions id
    in genericDeclareNamedSchema schemaOptions user
      & mapped.schema.description ?~ "An administrator, moderator, or basic \
                                     \ user within this service."
      & mapped.schema.example ?~ toJSON (Admin' exampleAdmin)

instance ToSchema Admin where
  declareNamedSchema admin =
    let schemaOptions = fromAesonOptions $ idiomaticJsonOptions (drop 1)
    in genericDeclareNamedSchema schemaOptions admin
      & mapped.schema.description ?~ "An administrator within this service."
      & mapped.schema.example ?~ toJSON exampleAdmin

instance ToSchema Moderator where
  declareNamedSchema moderator =
    let schemaOptions = fromAesonOptions $ idiomaticJsonOptions (drop 1)
    in genericDeclareNamedSchema schemaOptions moderator
      & mapped.schema.description ?~ "A moderator within this service."
      & mapped.schema.example ?~ toJSON exampleModerator

instance ToSchema BasicUser where
  declareNamedSchema basicUser =
    let schemaOptions = fromAesonOptions $ idiomaticJsonOptions (drop 2)
    in genericDeclareNamedSchema schemaOptions basicUser
      & mapped.schema.description ?~ "A basic user within this service."
      & mapped.schema.example ?~ toJSON exampleBasicUser

serviceSwagger :: Swagger
serviceSwagger = toSwagger (genericApi $ Proxy @Routes)
  & info.title       .~ "Scale by the Bay"
  & info.version     .~ "1.0.0"
  & info.license     ?~ ("Apache-2.0" & url ?~
                         URL "https://www.apache.org/licenses/LICENSE-2.0")
  & info.description ?~ "Hello, Scale by the Bay! This is small demo that \
                        \demonstrates how one might go about sketching out  \
                        \a data model for some idea in Haskell and \
                        \incrementally turning it into a real, functioning web \
                        \service."

type API
  =    ToServantApi Routes
  :<|> SwaggerSchemaUI "docs" "resources/swagger.json"

apiHandler :: Server API
apiHandler =
       genericServer routeHandlers
  :<|> redocSchemaUIServer serviceSwagger

--------------------------------------------------------------------------------
runServer :: IO ()
runServer = do
  let relativePath = "resources/swagger.json"
  swaggerExists <- doesFileExist relativePath
  when swaggerExists $ removeFile relativePath
  LBS8.writeFile relativePath $ encodePretty serviceSwagger
  run 8080 . logStdoutDev $ serve (Proxy @API) apiHandler

--------------------------------------------------------------------------------
arbitraryUserJson :: IO LBS8.ByteString
arbitraryUserJson = do
  arbitraryUser :: User <- generate arbitrary
  pure (encodePretty arbitraryUser)

printArbitraryEncodedUser :: IO ()
printArbitraryEncodedUser = do
  encodedUser <- arbitraryUserJson
  LBS8.putStrLn encodedUser

printArbitraryDecodedUser :: IO ()
printArbitraryDecodedUser = do
  encodedUser <- arbitraryUserJson
  pPrint (decode encodedUser :: Maybe User)
