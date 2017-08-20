"use strict";

exports._loginStatus = function(fb) {
  return function(callback) {
    return function() { // returns an effect
      fb.getLoginStatus(function(response) {
        callback(response)(); // callback itself returns an effect
      });
    }
  }
}

exports._login = function(fb) {
  return function(callback) {
    return function() { // returns an effect
      fb.login(function (response) {
        callback(response)();  // callback itself returns an effect
      });
    }
  }
}

exports._logout = function(fb) {
  return function(callback) {
    return function() { // returns an effect
      fb.logout(function (response) {
        callback(response)();  // callback itself returns an effect
      });
    }
  }
}

exports._init = function(callback) {
  return function(fbConfig) {
    return function() { // returns an effect
      window.fbAsyncInit = function() {
        FB.init(fbConfig);
        FB.AppEvents.logPageView();
        callback(FB)(); // callback itself returns an effect
      };
      (function(d, s, id){
        var js, fjs = d.getElementsByTagName(s)[0];
        if (d.getElementById(id)) {return;}
        js = d.createElement(s);
        js.id = id;
        var debug = fbConfig['debug'] ? "/debug" : "";
        js.src = "//connect.facebook.net/" + fbConfig['locale'] + "/sdk" + debug + ".js";
        fjs.parentNode.insertBefore(js, fjs);
      }(window.document, 'script', 'facebook-jssdk'));
    }
  }
}
