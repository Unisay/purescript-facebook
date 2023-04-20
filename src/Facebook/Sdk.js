"use strict";

export function _loginStatus(fb) {
  return function (callback) {
    return function () {
      // returns an effect
      fb.getLoginStatus(function (response) {
        callback(response)(); // callback itself returns an effect
      });
    };
  };
}

export function _login(opts) {
  return function (fb) {
    return function (callback) {
      return function () {
        // returns an effect
        fb.login(function (response) {
          callback(response)(); // callback itself returns an effect
        }, opts);
      };
    };
  };
}

export function _logout(fb) {
  return function (callback) {
    return function () {
      // returns an effect
      fb.logout(function (response) {
        callback(response)(); // callback itself returns an effect
      });
    };
  };
}

export function _api(fb, path, method, params, callback) {
  return function () {
    // returns an effect
    fb.api(path, method, params, function (response) {
      callback(response)(); // callback itself returns an effect
    });
  };
}

export function _init(callback) {
  return function (fbConfig) {
    return function () {
      // returns an effect
      // Check if FB is already initialized
      if (window.isFBLoaded || (window.FB && typeof window.FB === "object")) {
        callback(window.FB)();
      } else {
        window.fbAsyncInit = function () {
          window.FB.init(fbConfig);
          window.FB.AppEvents.logPageView();
          window.isFBLoaded = true;
          callback(window.FB)(); // callback itself returns an effect
        };
        (function (d, s, id) {
          var js,
            fjs = d.getElementsByTagName(s)[0];
          if (d.getElementById(id)) {
            return;
          }
          js = d.createElement(s);
          js.id = id;
          var debug = fbConfig["debug"] ? "/debug" : "";
          js.src =
            "//connect.facebook.net/" +
            fbConfig["locale"] +
            "/sdk" +
            debug +
            ".js";
          fjs.parentNode.insertBefore(js, fjs);
        })(window.document, "script", "facebook-jssdk");
      }
    };
  };
}
