"use strict"

module.exports = config => {
  config.set({
    autoWatch: true,
    browsers: ["Chrome"],
    files: [
      "karma/index.js",
      "https://connect.facebook.net/en_US/sdk.js"
    ],
    reporters: ["spec"],
    singleRun: false,
    logLevel: "config.LOG_DEBUG"
  })
}
