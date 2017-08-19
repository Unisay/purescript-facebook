# purescript-facebook
Idiomatic Purescript bindings for the Facebook SDK

### Disclaimer: this project is a work in progress

Init Facebook SDK & retrieve a current login status:

``` purescript
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Console (CONSOLE)
import Facebook.Sdk (FbConfig(FbConfig), facebookInit, facebookLoginStatus)

initFacebook :: ∀ e. Aff (console :: CONSOLE | e) Unit
initFacebook = do
  facebookInit $ FbConfig { appId: "1234567890" -- your app id
                          , status: true
                          , autoLogAppEvents: true
                          , xfbml: false
                          , version: "v2.10"
                          , locale: "en_US"
                          }
  info <- facebookLoginStatus
  logShow info
```

Outputs to the console:
```
Facebook.Sdk.FbStatusInfo {authResponse: Data.Maybe.Nothing, status: Facebook.Sdk.NotAuthorized}
```
