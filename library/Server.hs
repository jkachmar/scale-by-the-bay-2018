module Server where

import           Data.Aeson.Encode.Pretty

-- Miscellaneous external modules
import           Data.Aeson
import qualified Data.ByteString.Lazy          as LBS
import           Data.Proxy
import           Data.Text                     (Text)
import           Data.Time
import           GHC.Generics

-- Random data generation modules
import           Generic.Random                (genericArbitraryRec,
                                                genericArbitraryU, uniform,
                                                withBaseCase)
import           Test.QuickCheck               (Arbitrary (..), Gen, generate)
import           Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)
import           Test.QuickCheck.Instances     ()

-- Servant modules
import           Servant.API
import           Servant.API.Generic

--------------------------------------------------------------------------------
-- Application data model, JSON serialization, and arbitrary data generation
--------------------------------------------------------------------------------
-- | A user within the service, who must be one of the `Admin`, `Moderator` or
-- | `BasicUser` types.
data User
  = Admin'     !Admin
  | Moderator' !Moderator
  | BasicUser' !BasicUser
  deriving (Eq, Generic, Show)

-- Aeson typeclass instances that automatically derive JSON de/serialization
-- methods for `User` using the datatype's automatically derived `Generic`
-- instance
instance FromJSON User
instance ToJSON User

-- An `Arbitrary` instance for `User` that automatically derives the typeclass
-- methods required to generate random `User`s using the datatype's
-- automatically derived `Generic` instance.
instance Arbitrary User where
  arbitrary :: Gen User
  arbitrary = genericArbitraryU

instance ToADTArbitrary User

--------------------------------------------------------------------------------
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

-- Aeson typeclass instances that automatically derive JSON de/serialization
-- methods for `BasicUser` using the datatype's automatically derived `Generic`
-- instance
instance FromJSON BasicUser
instance ToJSON BasicUser

-- An `Arbitrary` instance for `BasicUser` that automatically derives the
-- typeclass methods required to generate random `BasicUser`s using the
-- datatype's automatically derived `Generic` instance.
instance Arbitrary BasicUser where
  arbitrary :: Gen BasicUser
  arbitrary = genericArbitraryU

-- A typeclass instance for `BasicUser` that helps us create `Arbitrary`
-- instances for more complex algebraic data types.
instance ToADTArbitrary BasicUser

--------------------------------------------------------------------------------
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

-- Aeson typeclass instances that automatically derive JSON de/serialization
-- methods for `Moderator` using the datatype's automatically derived `Generic`
-- instance
instance FromJSON Moderator
instance ToJSON Moderator

-- An `Arbitrary` instance for `Moderator` that automatically derives the
-- typeclass methods required to generate random `Moderator`s using the
-- datatype's automatically derived `Generic` instance.
instance Arbitrary Moderator where
  arbitrary :: Gen Moderator
  arbitrary = genericArbitraryU

-- A typeclass instance for `Moderator` that helps us create `Arbitrary`
-- instances for more complex algebraic data types.
instance ToADTArbitrary Moderator

--------------------------------------------------------------------------------
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

-- Aeson typeclass instances that automatically derive JSON de/serialization
-- methods for `Admin` using the datatype's automatically derived `Generic`
-- instance
instance FromJSON Admin
instance ToJSON Admin

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

-- A typeclass instance for `Admin` that helps us create `Arbitrary` instances
-- for more complex algebraic data types.
instance ToADTArbitrary Admin

--------------------------------------------------------------------------------
-- Helper functions to demosntrate arbitrary generation of datatypes _and_ their
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
    Just user -> print user

{-

--------------------------------------------------------------------------------
-- Modified Generic JSON serialiation and deserialization
--------------------------------------------------------------------------------

-- JSON serialization and deserialization typeclass instances for `User`s,
-- slightly modified to produce more idiomatic JSON output
instance FromJSON User where
  parseJSON = genericParseJSON userJsonOptions

instance ToJSON User where
  toJSON = genericToJSON userJsonOptions

-- | Modifications to Aeson's generic JSON encoding/decoding features for
-- | `User`s.
userJsonOptions :: Options
userJsonOptions
  = defaultOptions
  { -- TODO(jkachmar): Explain what this option does
    sumEncoding = ObjectWithSingleField
    -- TODO(jkachmar): Explain what this option modifies
  , constructorTagModifier =
      -- Filters out any ' characters, which are used in `User`, but not useful
      -- to leak into our JSON codecs
      let dropTicks = filter $ not . \char -> (char == '\'')
      in camelTo2 '_' . dropTicks
  }

--------------------------------------------------------------------------------
-- JSON serialization and deserialization typeclass instances for `User`s,
-- slightly modified to produce more idiomatic JSON output
instance FromJSON BasicUser where
  parseJSON = genericParseJSON basicUserJsonOptions

instance ToJSON BasicUser where
  toJSON = genericToJSON basicUserJsonOptions

-- | Modifications to Aeson's generic JSON encoding/decoding features for
-- | `BasicUser`s.
basicUserJsonOptions :: Options
basicUserJsonOptions
  = defaultOptions
  { -- Drop the leading 2 chars ("bu" prefix) and convert to snake_case
    fieldLabelModifier = camelTo2 '_' . drop 2
  }

--------------------------------------------------------------------------------
-- JSON serialization and deserialization typeclass instances for
-- `Moderator`s, slightly modified to produce more idiomatic JSON output
instance FromJSON Moderator where
  parseJSON = genericParseJSON moderatorJsonOptions

instance ToJSON Moderator where
  toJSON = genericToJSON moderatorJsonOptions

-- | Modifications to Aeson's generic JSON encoding/decoding features for
-- | `Moderator`s.
moderatorJsonOptions :: Options
moderatorJsonOptions
  = defaultOptions
  { -- Drop the leading 2 chars ("m" prefix) and convert to snake_case
    fieldLabelModifier = camelTo2 '_' . drop 1
  }

--------------------------------------------------------------------------------
-- JSON serialization and deserialization typeclass instances for `Admin`s,
-- slightly modified to produce more idiomatic JSON output
instance FromJSON Admin where
  parseJSON = genericParseJSON adminJsonOptions

instance ToJSON Admin where
  toJSON = genericToJSON adminJsonOptions

-- | Modifications to Aeson's generic JSON encoding/decoding features for
-- | `Admin`s.
adminJsonOptions :: Options
adminJsonOptions
  = defaultOptions
  { -- Drop the leading 2 chars ("a" prefix) and convert to snake_case
    fieldLabelModifier = camelTo2 '_' . drop 1
  }

-}

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
-- | (:-) - Boilerplate that helps the framework resolve the type following it
-- | as either a standalone Route or an internal Link to another resource.
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
  { _getUsers :: route
      :- Summary "Get a list of all users."
      :> Description "Get a list of all users currently registered with this \
                     \service."
      :> "users" :> Get '[JSON] [User]

  -- Equivalent to a "/users/basic" route that accepts GET requests and returns
  -- JSON
  , _getBasicUsers :: route
      :- Summary "Get a list of all 'basic' users."
      :> Description "Get a list of all users currently registered with this \
                     \service who are not moderators or administrators."
      :> "users" :> "basic" :> Get '[JSON] [BasicUser]

  -- Equivalent to a "/users/moderators" route that accepts GET requests and
  -- returns JSON
  , _getModerators :: route
      :- Summary "Get a list of all moderators."
      :> Description "Get a list of all users classified as moderators \
                     \currently registered with this service."
      :> "users" :> "moderators" :> Get '[JSON] [Moderator]

  -- Equivalent to a "/users/admins" route that accepts GET requests and
  -- returns JSON
  , _getAdmins :: route
      :- Summary "Get a list of all admins."
      :> Description "Get a list of all users classified as administrators \
                     \currently registered with this service."
      :> "users" :> "admins" :> Get '[JSON] [Admin]
  } deriving Generic

-- | The API contract for all routes associated with this specification.
-- |
-- | Note that the `Routes` record can, itself, contain route specifications.
-- | This allows users to factor out components of the API type into different
-- | modules as necessary (e.g. to separate concerns, version sub-routes, etc.)
data Routes route
  = Routes
  { _v1Routes :: route :- "v1" :> (ToServantApi UserRoutes)
  } deriving Generic

-- | A value "containing" the API contract as defined by the `Routes` type.
routes :: Proxy (ToServantApi Routes)
routes = genericApi (Proxy @Routes)

--------------------------------------------------------------------------------
-- | The entry point for the application.
runServer :: IO ()
runServer = putStrLn "Please finish writing me!"
