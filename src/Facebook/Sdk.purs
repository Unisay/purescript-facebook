module Facebook.Sdk
  ( Sdk
  , AppId
  , Config (..)
  , StatusInfo (..)
  , Status (..)
  , AuthResponse (..)
  , defaultConfig
  , init
  , loginStatus
  ) where

import Control.Monad (bind)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (F, Foreign, readNullOrUndefined, readString)
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
  , xml                :: Boolean
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
  , xml                : false
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
  , expiresIn     :: String
  , signedRequest :: String
  , userId        :: String
  }

derive instance genericAuthResponse :: Generic AuthResponse

instance showAuthResponse :: Show AuthResponse where show = gShow

readStatusInfo :: Foreign -> F StatusInfo
readStatusInfo value = do
  st <- value ! "status" >>= readStatus
  ar <- value ! "authResponse" >>= readNullOrUndefined >>= traverse readAuthResponse
  pure $ StatusInfo { status: st, authResponse: ar}

readStatus :: Foreign -> F Status
readStatus value = do
  str <- readString value
  case str of
    "connected"      -> pure Connected
    "not_authorized" -> pure NotAuthorized
    otherwise        -> pure Unknown

readAuthResponse :: Foreign -> F AuthResponse
readAuthResponse value = do
  at <- value ! "accessToken" >>= readString
  ei <- value ! "expiresIn" >>= readString -- TODO: proper type
  sr <- value ! "signedRequest" >>= readString
  id <- value ! "userID" >>= readString
  pure $ AuthResponse { accessToken: at
                        , expiresIn: ei
                        , signedRequest: sr
                        , userId: id
                        }

data Sdk

instance showSdk :: Show Sdk where
  show = const "[Facebook SDK]"

-- | Initialize Facebook SDK
-- | https://developers.facebook.com/docs/javascript/reference/.init/v2.10
init :: ∀ e. Config -> Aff e Sdk
init config = makeAff (\error success -> _init success config)

foreign import _init :: ∀ e. (Sdk -> Eff e Unit) -> Config -> Eff e Unit

-- | Retrieve a Facebook Login status
-- | https://developers.facebook.com/docs/facebook-login/web#checklogin
loginStatus :: ∀ e. Sdk -> Aff e StatusInfo
loginStatus sdk = do
  value <- makeAff (\error success -> _loginStatus sdk success)
  either handleErrors pure $ runExcept (readStatusInfo value)
    where handleErrors errors = let message = intercalate "; " (map show errors)
                                in throwError (error message)

foreign import _loginStatus :: ∀ e. Sdk -> (Foreign -> Eff e Unit) -> Eff e Unit
