module Facebook.Sdk
  ( defaultConfig
  , Config (..)
  , Status (..)
  , Field (..)
  , StatusInfo (..)
  , AuthResponse (..)
  , UserId (..)
  , UserName (..)
  , UserEmail (..)
  , UserInfo (..)
  , AccessToken (..)
  , Sdk
  , AppId
  , ApiPath
  , ApiMethod (..)
  , LoginOptions (..)
  , Scope (..)
  , init
  , loginStatus
  , login
  , logout
  , api
  , me
  , userInfo
  ) where

import Control.Monad (bind, (<#>), (<$>), (<*>), (=<<))
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Array (zip)
import Data.Either (Either(..), either, note)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Foreign (F, Foreign, MultipleErrors, readInt, readNullOrUndefined, readString)
import Data.Foreign.Class (class Encode, encode)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.Function.Uncurried (Fn5, runFn5)
import Data.Functor (map)
import Data.Generic (class Generic, gShow)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap as SM
import Data.String (joinWith, toLower)
import Data.Traversable (intercalate, traverse)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Ord, class Show, Unit, const, pure, show, ($), (<<<), (<>), (>>=), (>>>))

type AppId = String

newtype Config = Config
  { appId                :: AppId
  , version              :: String
  , status               :: Boolean
  , cookie               :: Boolean
  , frictionlessRequests :: Boolean
  , hideFlashCallback    :: Boolean
  , autoLogAppEvents     :: Boolean
  , xmlfb                :: Boolean
  , debug                :: Boolean
  , locale               :: String
  }

defaultConfig :: AppId -> Config
defaultConfig appId = Config
  { appId                : appId
  , version              : "v2.10"
  , status               : false
  , cookie               : false
  , frictionlessRequests : false
  , hideFlashCallback    : false
  , autoLogAppEvents     : false
  , xmlfb                : false
  , debug                : false
  , locale               : "en_US"
  }

derive instance eqConfig :: Eq Config
derive instance genericConfig :: Generic Config
instance showConfig :: Show Config where show = gShow

data Status = Connected | NotAuthorized | Unknown

derive instance eqStatus :: Eq Status
derive instance genericStatus :: Generic Status
instance showStatus :: Show Status where show = gShow

newtype StatusInfo = StatusInfo
  { status       :: Status
  , authResponse :: Maybe AuthResponse
  }

derive instance eqStatusInfo :: Eq StatusInfo
derive instance genericStatusInfo :: Generic StatusInfo
instance showStatusInfo :: Show StatusInfo where show = gShow

newtype AccessToken = AccessToken String
derive newtype instance eqAccessToken :: Eq AccessToken
derive newtype instance showAccessToken :: Show AccessToken
derive newtype instance encodeAccessToken :: Encode AccessToken
derive instance genericAccessToken :: Generic AccessToken

newtype UserId = UserId String
derive newtype instance eqUserId :: Eq UserId
derive newtype instance showUserId :: Show UserId
derive newtype instance encodeUserId :: Encode UserId
derive instance genericUserId :: Generic UserId

newtype UserName = UserName String
derive newtype instance eqUserName :: Eq UserName
derive newtype instance showUserName :: Show UserName
derive instance genericUserName :: Generic UserName

newtype UserEmail = UserEmail String
derive newtype instance eqUserEmail :: Eq UserEmail
derive newtype instance showUserEmail :: Show UserEmail
derive instance genericUserEmail :: Generic UserEmail

newtype AuthResponse = AuthResponse
  { accessToken   :: AccessToken
  , expiresIn     :: Int
  , signedRequest :: String
  , userId        :: UserId
  }

derive instance eqAuthResponse :: Eq AuthResponse
derive instance genericAuthResponse :: Generic AuthResponse
instance showAuthResponse :: Show AuthResponse where show = gShow

data Field = Id | Name | Email | Gender | Birthday
derive instance eqField :: Eq Field
derive instance genericField :: Generic Field
derive instance ordField :: Ord Field
instance showField :: Show Field where
  show Id = "Id"
  show Name = "Name"
  show Email = "Email"
  show Gender = "Gender"
  show Birthday = "Birthday"

readField :: String -> Either String Field
readField "id" = Right Id
readField "name" = Right Name
readField "email" = Right Email
readField "gender" = Right Gender
readField "birthday" = Right Birthday
readField field = Left $ "Unknown field in the Facebook Graph API response (" <> field <> ")"

newtype UserInfo = UserInfo
  { id    :: UserId
  , name  :: UserName
  , email :: UserEmail
  }

derive instance eqUserInfo :: Eq UserInfo

data Sdk

newtype Scope = Scope String
derive newtype instance eqScope :: Eq Scope
derive instance newtypeScope :: Newtype Scope _

newtype LoginOptions = LoginOptions
  { scopes :: Array Scope
  }
instance encodeLoginOptions :: Encode LoginOptions where
  encode (LoginOptions { scopes: sx }) =
    encode $ SM.fromFoldable [Tuple "scope" (encode $ joinWith "," (map unwrap sx))]

instance showSdk :: Show Sdk where
  show = const "[Facebook SDK]"


readStatusInfo :: Foreign -> F StatusInfo
readStatusInfo value = do
  st <- value ! "status" >>= readStatus
  ar <- value ! "authResponse" >>= readNullOrUndefined >>= traverse readAuthResponse
  pure $ StatusInfo { status: st, authResponse: ar}
  where
    readStatus :: Foreign -> F Status
    readStatus status = do
      str <- readString status
      case str of
        "connected"      -> pure Connected
        "not_authorized" -> pure NotAuthorized
        otherwise        -> pure Unknown
    readAuthResponse :: Foreign -> F AuthResponse
    readAuthResponse authResponse = do
      at <- authResponse ! "accessToken" >>= readString <#> AccessToken
      ei <- authResponse ! "expiresIn" >>= readInt
      sr <- authResponse ! "signedRequest" >>= readString
      id <- authResponse ! "userID" >>= readString <#> UserId
      pure $ AuthResponse { accessToken: at
                          , expiresIn: ei
                          , signedRequest: sr
                          , userId: id
                          }

type SMap = SM.StrMap String

readSMap :: Foreign -> F SMap
readSMap value = do
  ks <- keys value
  vs <- traverse (\key -> value ! key >>= readString) ks
  pure $ SM.fromFoldable (zip ks vs)

-- | Initialize Facebook SDK
init :: ∀ e. Config -> Aff e Sdk
init config = makeAff (\error success -> _init success config)

-- | Retrieve a Facebook Login status
loginStatus :: ∀ e. Sdk -> Aff e StatusInfo
loginStatus = returnStatusInfo _loginStatus

-- | Login user
login :: ∀ e. LoginOptions -> Sdk -> Aff e StatusInfo
login opts = returnStatusInfo $ _login (encode opts)

-- | Logout user
logout :: ∀ e. Sdk -> Aff e StatusInfo
logout = returnStatusInfo _logout

userInfo :: ∀ e. AccessToken -> Sdk -> Aff e UserInfo
userInfo token sdk = me [Id, Name, Email] token sdk >>= \m ->
  either missingField pure $ do
    id <- note "id" (UserId <$> M.lookup Id m)
    name <- note "name" (UserName <$> M.lookup Name m)
    email <- note "email" (UserEmail <$> M.lookup Email m)
    pure $ UserInfo { id, name, email }
  where missingField s = throwError $
    error ("Can't build Facebook user info because of missing user " <> s)

-- | Get information about logged user
-- | https://developers.facebook.com/docs/graph-api/overview#step3
me :: ∀ e. Array Field -> AccessToken -> Sdk -> Aff e (M.Map Field String)
me fields token sdk = transFields =<< api sdk token Get "/me" (requestFields fields)
  where transFields m = either (throwError <<< error) pure $ foldWithIndexM transField M.empty m
        transField key map val = (\k v -> M.insert k v map) <$> readField key <*> Right val
        requestFields fs = SM.insert "fields" (joinFields fs) SM.empty
        joinFields fs = joinWith "," (show >>> toLower <$> fs)

type ApiPath = String
type ApiParams = SMap
type ApiPathF = Foreign
type ApiParamsF = Foreign
type ApiMethodF = Foreign

data ApiMethod = Get | Post | Delete
derive instance eqApiMethod :: Eq ApiMethod
derive instance genericApiMethod :: Generic ApiMethod
instance showApiMethod :: Show ApiMethod where show = gShow

instance encodeApiMethod :: Encode ApiMethod where
  encode Get = encode "get"
  encode Post = encode "post"
  encode Delete = encode "delete"

api :: ∀ e. Sdk
    -> AccessToken
    -> ApiMethod
    -> ApiPath
    -> ApiParams
    -> Aff e SMap
api sdk (AccessToken token) method path params = do
  let path' = encode path
  let method' = encode method
  let params' = encode $ SM.insert "access_token" token params
  value <- makeAff (\_ cb -> runFn5 _api sdk path' method' params' cb)
  let prefix = "Facebook graph api call (" <> (show method) <> " "
                                           <> (show path)   <> ") error"
  either (handleForeignErrors prefix) pure $ runExcept (readSMap value)

returnStatusInfo :: ∀ e. (Sdk -> (Foreign -> Eff e Unit) -> Eff e Unit)
                 -> Sdk
                 -> Aff e StatusInfo
returnStatusInfo f sdk = do
  value <- makeAff (\_ success -> f sdk success)
  either (handleForeignErrors "Facebook status info") pure $
    runExcept (readStatusInfo value)

-- | String is an error message prefix
handleForeignErrors :: ∀ e a. String -> MultipleErrors -> Aff e a
handleForeignErrors prefix errors =
  let message = prefix <> ": " <> intercalate "; " (map show errors)
  in throwError $ error message

-- | https://developers.facebook.com/docs/javascript/reference/.init/v2.10
foreign import _init :: ∀ e. (Sdk -> Eff e Unit) -> Config -> Eff e Unit

-- | https://developers.facebook.com/docs/reference/javascript/FB.login
foreign import _login :: ∀ e. Foreign -> Sdk -> (Foreign -> Eff e Unit) -> Eff e Unit

-- | https://developers.facebook.com/docs/reference/javascript/FB.logout
foreign import _logout :: ∀ e. Sdk -> (Foreign -> Eff e Unit) -> Eff e Unit

-- | https://developers.facebook.com/docs/reference/javascript/FB.getLoginStatus
foreign import _loginStatus :: ∀ e. Sdk -> (Foreign -> Eff e Unit) -> Eff e Unit

-- | https://developers.facebook.com/docs/javascript/reference/FB.api/
foreign import _api :: ∀ e. Fn5 Sdk ApiPathF ApiMethodF ApiParamsF (Foreign -> Eff e Unit) (Eff e Unit)
