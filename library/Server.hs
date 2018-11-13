module Server where

-- Miscellaneous external modules
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Text                            (Text)
import           Data.Time
import           GHC.Generics
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
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.API
import           Servant.API.Generic
import           Servant.Server.Generic

--------------------------------------------------------------------------------
-- Application data model
--------------------------------------------------------------------------------
-- | A user within the service, who must be one of the `Admin`, `Moderator` or
-- | `BasicUser` types.
data User
  = Admin'     !Admin
  | Moderator' !Moderator
  | BasicUser' !BasicUser
  deriving (Eq, Generic, Show)

-- | An administrator of the service, containing an optional field for the
-- | `Admin` who promoted them.
-- |
-- | Note that `Admin` is a recursive data structure! `Admin`s may have either
-- | been promoted by another `Admin`, or created with administrator status
-- | when the server was initialized.
data Admin
  = Admin
  { aUsername     :: !Text
  , aPassword     :: !Text
  , aEmailAddress :: !Text
  , aPromotedBy   :: !(Maybe Admin)
  , aActive       :: !Bool
  , aCreatedAt    :: !UTCTime
  , aUpdatedAt    :: !UTCTime
  } deriving (Eq, Generic, Show)

-- | A moderator within the service, containing a list of sub-communities that
-- | they are responsible for governing, as well as a record of the `Admin` who
-- | promoted them to moderatorship.
data Moderator
  = Moderator
  { mUsername     :: !Text
  , mPassword     :: !Text
  , mEmailAddress :: !Text
  , mGoverns      :: ![Text]
  , mPromotedBy   :: !Admin
  , mActive       :: !Bool
  , mCreatedAt    :: !UTCTime
  , mUpdatedAt    :: !UTCTime
  } deriving (Eq, Generic, Show)

-- | A "basic" user within the service.
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
-- Arbitrary data generation
--------------------------------------------------------------------------------
-- An `Arbitrary` instance for `User` that automatically derives the typeclass
-- methods required to generate random `User`s using the datatype's
-- automatically derived `Generic` instance.
instance Arbitrary User where
  arbitrary :: Gen User
  arbitrary = genericArbitraryU

-- A `ToADTArbitrary` instance for `User`, which helps create arbitrary values
-- every constructor in an algebraic data type.
--
-- This is especially useful for sum types, so that there's a guarantee that
-- all branches of the sum are properly tested.
instance ToADTArbitrary User

-- An `Arbitrary` instance for `Admin` that automatically derives the typeclass
-- methods required to generate random `Admin`s using the datatype's
-- automatically derived `Generic` instance.
--
-- Note that since `Admin` can potentially contain another `Admin` within
-- itself, a "base case" is provided that explicitly does _not_ contain an
-- `Admin`.
instance Arbitrary Admin where
  arbitrary :: Gen Admin
  arbitrary = genericArbitraryRec uniform `withBaseCase` baseAdminCase
    where
      baseAdminCase :: Gen Admin
      baseAdminCase = Admin
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> pure Nothing
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

-- An `Arbitrary` instance for `BasicUser` that automatically derives the
-- typeclass methods required to generate random `BasicUser`s using the
-- datatype's automatically derived `Generic` instance.
instance Arbitrary BasicUser where
  arbitrary :: Gen BasicUser
  arbitrary = genericArbitraryU

-- An `Arbitrary` instance for `Moderator` that automatically derives the
-- typeclass methods required to generate random `Moderator`s using the
-- datatype's automatically derived `Generic` instance.
instance Arbitrary Moderator where
  arbitrary :: Gen Moderator
  arbitrary = genericArbitraryU

--------------------------------------------------------------------------------
-- JSON serialization/deserialization
--------------------------------------------------------------------------------
-- Aeson typeclass instances that automatically derive JSON de/serialization
-- methods for `User` using the datatype's automatically derived `Generic`
-- instance
instance FromJSON User
instance ToJSON User

-- Aeson typeclass instances that automatically derive JSON de/serialization
-- methods for `BasicUser` using the datatype's automatically derived `Generic`
-- instance
instance FromJSON BasicUser
instance ToJSON BasicUser

-- Aeson typeclass instances that automatically derive JSON de/serialization
-- methods for `Moderator` using the datatype's automatically derived `Generic`
-- instance
instance FromJSON Moderator
instance ToJSON Moderator

-- Aeson typeclass instances that automatically derive JSON de/serialization
-- methods for `Admin` using the datatype's automatically derived `Generic`
-- instance
instance FromJSON Admin
instance ToJSON Admin

--------------------------------------------------------------------------------
-- Modified Generic JSON serialiation and deserialization
--------------------------------------------------------------------------------
-- -- | Modifications to Aeson's generic JSON encoders/decoders to make the
-- -- | resulting output JSON a little more idiomatic.
-- -- |
-- -- | Takes an input function that modifies the record field labels before
-- -- | converting them from "CamelCase" to "snake_case".
-- -- |
-- -- | Additionally it, removes the `'` character from any constructor names,
-- -- | converts all constructor names from "CamelCase" to "snake_case", and
-- -- | wraps the generated JSON in an object whose key is the constructor name.
-- idiomaticJsonOptions :: (String -> String) -> Options
-- idiomaticJsonOptions f = defaultOptions
--   { constructorTagModifier = camelTo2 '_' . filter (not . (== '\''))
--   , fieldLabelModifier = camelTo2 '_' . f
--   , sumEncoding = ObjectWithSingleField
--   , tagSingleConstructors = True
--   }

-- -- JSON serialization and deserialization typeclass instances for `User`s,
-- -- slightly modified to produce more idiomatic JSON output
-- instance FromJSON User where
--   parseJSON = genericParseJSON (idiomaticJsonOptions id)
--     { sumEncoding = UntaggedValue
--     }

-- instance ToJSON User where
--   toJSON = genericToJSON (idiomaticJsonOptions id)
--     { sumEncoding = UntaggedValue
--     }

-- -- JSON serialization and deserialization typeclass instances for `User`s,
-- -- slightly modified to produce more idiomatic JSON output
-- instance FromJSON BasicUser where
--   parseJSON = genericParseJSON (idiomaticJsonOptions (drop 2))

-- instance ToJSON BasicUser where
--   toJSON = genericToJSON (idiomaticJsonOptions (drop 2))

-- -- JSON serialization and deserialization typeclass instances for
-- -- `Moderator`s, slightly modified to produce more idiomatic JSON output
-- instance FromJSON Moderator where
--   parseJSON = genericParseJSON (idiomaticJsonOptions (drop 1))

-- instance ToJSON Moderator where
--   toJSON = genericToJSON (idiomaticJsonOptions (drop 1))

-- -- JSON serialization and deserialization typeclass instances for `Admin`s,
-- -- slightly modified to produce more idiomatic JSON output
-- instance FromJSON Admin where
--   parseJSON = genericParseJSON (idiomaticJsonOptions (drop 1))

-- instance ToJSON Admin where
--   toJSON = genericToJSON (idiomaticJsonOptions (drop 1))

--------------------------------------------------------------------------------
-- Servant routing and web server
--------------------------------------------------------------------------------
-- | The API specification for all routes associated with a `User`.
-- |
-- | An API's route specification is defined as a record of type-level route
-- | definitions.
-- |
-- | A brief summary of the symbols involved:
-- |
-- | (:-) - Boilerplate that helps Servant resolve the routing type.
-- |
-- | (:>) - A generic resource separator, this symbol is used to separate logical
-- | components of the API type, such as path segments, capture groups, query
-- | parameters, request body, etc.
-- |
-- | ('[JSON, HTML]) - A list of MIME types that the route can handle, either as
-- | a request or a response.
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
      :- Summary "Get a list of all 'basic' users."
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


-- | A record of handlers corresponding to each of API routes specified in
-- | `UserRoutes`.
userRouteHandlers :: UserRoutes AsServer
userRouteHandlers = UserRoutes
  { getUsers      = liftIO $ generate arbitrary
  , getBasicUsers = liftIO $ generate arbitrary
  , getModerators = liftIO $ generate arbitrary
  , getAdmins     = liftIO $ generate arbitrary
  }

-- | The API specification for all routes in the application.
-- |
-- | Note that the `Routes` record can, itself, contain route specifications.
-- | This allows users to factor out components of the API type into different
-- | modules as necessary (e.g. to separate concerns, version sub-routes, etc.)
data Routes route
  = Routes
  { v1Routes :: route :- "v1" :> (ToServantApi UserRoutes)
  } deriving Generic

-- | A record of handlers corresponding to each of API routes specified in
-- | `Routes`.
routeHandlers :: Routes AsServer
routeHandlers = Routes
  { v1Routes = genericServer userRouteHandlers
  }

--------------------------------------------------------------------------------
-- | The entry point for the application.
runServer :: IO ()
runServer = run 8080 . logStdoutDev $ genericServe routeHandlers

--------------------------------------------------------------------------------
-- Helper functions to demosntrat arbitrary generation of datatypes _and_ their
-- generically derived JSON encoders/decoders

arbitraryUserJson :: IO LBS.ByteString
arbitraryUserJson = do
  arbitraryUser :: User <- generate arbitrary
  pure (encodePretty arbitraryUser)

printArbitraryEncodedUser :: IO ()
printArbitraryEncodedUser = do
  encodedUser <- arbitraryUserJson
  LBS.putStrLn encodedUser

printArbitraryDecodedUser :: IO ()
printArbitraryDecodedUser = do
  encodedUser <- arbitraryUserJson
  let (maybeUser :: Maybe User) = decode encodedUser
  case maybeUser of
    Nothing   -> print ("Failed to decode a User!" :: Text)
    Just user -> pPrint user
