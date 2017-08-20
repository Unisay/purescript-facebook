# purescript-facebook
Idiomatic Purescript bindings for the Facebook SDK

### Disclaimer: this project is a work in progress

Currently supported methods:
1. [FB.init](https://developers.facebook.com/docs/javascript/reference/FB.init/v2.10)
1. [FB.loginStatus](https://developers.facebook.com/docs/reference/javascript/FB.getLoginStatus/v2.10)
1. [FB.login](https://developers.facebook.com/docs/reference/javascript/FB.login/v2.10)

Init Facebook SDK & retrieve a current login status:

``` purescript
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Console (CONSOLE)
import Facebook.Sdk (init, loginStatus, defaultConfig) as FB

initFacebook :: ∀ e. Aff (console :: CONSOLE | e) Unit
initFacebook = do
  sdk <- FB.init $ defaultConfig "1234567890" -- your app id
  info <- FB.loginStatus sdk
  logShow info
```

Outputs to the console:
```
Facebook.Sdk.FbStatusInfo {authResponse: Data.Maybe.Nothing, status: Facebook.Sdk.NotAuthorized}
```

### Installation
```
bower install purescript-facebook --save
```
