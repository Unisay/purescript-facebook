module Facebook.Sdk
  ( defaultConfig
  , Config (..)
  , Status (..)
  , StatusInfo (..)
  , AuthResponse (..)
  , Sdk
  , AppId
  , init
  , loginStatus
  , login
  , logout
  ) where

import Control.Monad (bind)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (F, Foreign, MultipleErrors, readInt, readNullOrUndefined, readString)
import Data.Foreign.Index ((!))
import Data.Functor (map)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Data.Traversable (intercalate, traverse)
import Prelude (class Show, Unit, const, pure, show, ($), (>>=))

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

derive instance genericConfig :: Generic Config
instance showConfig :: Show Config where show = gShow

data Status = Connected | NotAuthorized | Unknown

derive instance genericStatus :: Generic Status
instance showStatus :: Show Status where show = gShow

newtype StatusInfo = StatusInfo
  { status       :: Status
  , authResponse :: Maybe AuthResponse
  }

derive instance genericStatusInfo :: Generic StatusInfo
instance showStatusInfo :: Show StatusInfo where show = gShow

newtype AuthResponse = AuthResponse
  { accessToken   :: String
  , expiresIn     :: Int
  , signedRequest :: String
  , userId        :: String
  }

derive instance genericAuthResponse :: Generic AuthResponse
instance showAuthResponse :: Show AuthResponse where show = gShow

data Sdk

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
      at <- authResponse ! "accessToken" >>= readString
      ei <- authResponse ! "expiresIn" >>= readInt
      sr <- authResponse ! "signedRequest" >>= readString
      id <- authResponse ! "userID" >>= readString
      pure $ AuthResponse { accessToken: at
                          , expiresIn: ei
                          , signedRequest: sr
                          , userId: id
                          }

-- | Initialize Facebook SDK
-- | https://developers.facebook.com/docs/javascript/reference/.init/v2.10
init :: ∀ e. Config -> Aff e Sdk
init config = makeAff (\error success -> _init success config)

-- | Retrieve a Facebook Login status
-- | https://developers.facebook.com/docs/reference/javascript/FB.getLoginStatus
loginStatus :: ∀ e. Sdk -> Aff e StatusInfo
loginStatus = returnStatusInfo _loginStatus

-- | Login user
-- | https://developers.facebook.com/docs/reference/javascript/FB.login
login :: ∀ e. Sdk -> Aff e StatusInfo
login = returnStatusInfo _login

-- | Logout user
-- | https://developers.facebook.com/docs/reference/javascript/FB.logout
logout :: ∀ e. Sdk -> Aff e StatusInfo
logout = returnStatusInfo _logout

returnStatusInfo :: ∀ e. (Sdk -> (Foreign -> Eff e Unit) -> Eff e Unit) -> Sdk -> Aff e StatusInfo
returnStatusInfo f sdk = do
  value <- makeAff (\error success -> f sdk success)
  either handleForeignErrors pure $ runExcept (readStatusInfo value)

handleForeignErrors :: ∀ e a. MultipleErrors -> Aff e a
handleForeignErrors errors = throwError (error $ intercalate "; " (map show errors))

foreign import _init :: ∀ e. (Sdk -> Eff e Unit) -> Config -> Eff e Unit
foreign import _login :: ∀ e. Sdk -> (Foreign -> Eff e Unit) -> Eff e Unit
foreign import _logout :: ∀ e. Sdk -> (Foreign -> Eff e Unit) -> Eff e Unit
foreign import _loginStatus :: ∀ e. Sdk -> (Foreign -> Eff e Unit) -> Eff e Unit
