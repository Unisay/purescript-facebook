# purescript-facebook
Idiomatic Purescript bindings for the Facebook SDK

[![Build Status](https://travis-ci.org/Unisay/purescript-facebook.svg?branch=master)](https://travis-ci.org/Unisay/purescript-facebook)

### Disclaimer: this project is a work in progress

Currently supported methods:
1. [FB.init](https://developers.facebook.com/docs/javascript/reference/FB.init/v3.1)
1. [FB.loginStatus](https://developers.facebook.com/docs/reference/javascript/FB.getLoginStatus/v3.1)
1. [FB.login](https://developers.facebook.com/docs/reference/javascript/FB.login/v3.1)
1. [FB.logout](https://developers.facebook.com/docs/reference/javascript/FB.logout/v3.1)

Init Facebook SDK & retrieve a current login status:

``` purescript
import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Facebook.Sdk (init, loginStatus, defaultConfig) as FB

initFacebook :: Aff Unit
initFacebook = do
  sdk <- FB.init $ FB.defaultConfig "1234567890" -- your app id
  info <- FB.loginStatus sdk
  liftEffect $ logShow info
```

Outputs to the console:
```
Facebook.Sdk.FbStatusInfo {authResponse: Data.Maybe.Nothing, status: Facebook.Sdk.NotAuthorized}
```

### Installation
```
bower install purescript-facebook --save
```
