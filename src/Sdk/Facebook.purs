module Facebook.Sdk
  ( FbConfig (..)
  , FbStatusInfo (..)
  , FbStatus (..)
  , FbAuthResponse (..)
  , facebookInit
  , facebookLoginStatus
  ) where

import Control.Monad (bind)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Functor (map)
import Data.Foreign (F, Foreign, readNullOrUndefined, readString)
import Data.Foreign.Index ((!))
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Data.Traversable (intercalate, traverse)
import Prelude (class Show, Unit, pure, show, ($), (>>=))

newtype FbConfig = FbConfig
  { appId            :: String
  , version          :: String
  , status           :: Boolean
  , autoLogAppEvents :: Boolean
  , xfbml            :: Boolean
  , locale           :: String -- en_US
  }

derive instance genericFbConfig :: Generic FbConfig
instance showFbConfig :: Show FbConfig where show = gShow

data FbStatus = Connected | NotAuthorized | Unknown

derive instance genericFbStatus :: Generic FbStatus
instance showFbStatus :: Show FbStatus where show = gShow

newtype FbStatusInfo = FbStatusInfo
  { status       :: FbStatus
  , authResponse :: Maybe FbAuthResponse
  }

derive instance genericFbStatusInfo :: Generic FbStatusInfo
instance showFbStatusInfo :: Show FbStatusInfo where show = gShow

newtype FbAuthResponse = FbAuthResponse
  { accessToken   :: String
  , expiresIn     :: String
  , signedRequest :: String
  , userId        :: String
  }

derive instance genericFbAuthResponse :: Generic FbAuthResponse

instance showFbAuthResponse :: Show FbAuthResponse where show = gShow

readFbStatusInfo :: Foreign -> F FbStatusInfo
readFbStatusInfo value = do
  st <- value ! "status" >>= readFbStatus
  ar <- value ! "authResponse" >>= readNullOrUndefined >>= traverse readAuthResponse
  pure $ FbStatusInfo { status: st, authResponse: ar}

readFbStatus :: Foreign -> F FbStatus
readFbStatus value = do
  str <- readString value
  case str of
    "connected"      -> pure Connected
    "not_authorized" -> pure NotAuthorized
    otherwise        -> pure Unknown

readAuthResponse :: Foreign -> F FbAuthResponse
readAuthResponse value = do
  at <- value ! "accessToken" >>= readString
  ei <- value ! "expiresIn" >>= readString -- TODO: proper type
  sr <- value ! "signedRequest" >>= readString
  id <- value ! "userID" >>= readString
  pure $ FbAuthResponse { accessToken: at
                        , expiresIn: ei
                        , signedRequest: sr
                        , userId: id
                        }

-- | Initialize Facebook SDK
-- | https://developers.facebook.com/docs/javascript/quickstart#loading
facebookInit :: ∀ e. FbConfig -> Aff e Unit
facebookInit config = makeAff (\error success -> _fbInit success config)

foreign import _fbInit :: ∀ e. (Unit -> Eff e Unit) -> FbConfig -> Eff e Unit

-- | Retrieve a Facebook Login status
-- | https://developers.facebook.com/docs/facebook-login/web#checklogin
facebookLoginStatus :: ∀ e. Aff e FbStatusInfo
facebookLoginStatus = do
  value <- makeAff (\error success -> _fbGetLoginStatus success)
  either handleErrors pure $ runExcept (readFbStatusInfo value)
    where handleErrors errors = let message = intercalate "; " (map show errors)
                                in throwError (error message)

foreign import _fbGetLoginStatus :: ∀ e. (Foreign -> Eff e Unit) -> Eff e Unit
