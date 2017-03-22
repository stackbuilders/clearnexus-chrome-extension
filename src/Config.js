"use strict"

// Pattern for Aff Monad
exports.loadEnvironment = function (success, error) {
  chrome.storage.sync.get('environment', function(items) {
    if (items) {
      success(items);
    } else {
      error();
    }
  });
};

exports.uncurriedSaveEnv = function(chrome_, env) {
  return function() {
    //Dinamically assign Chrome if this is called in Browser
    var chrome =  chrome_.value0 || window.chrome;

      chrome.storage.sync.set({
	environment: env
      }, function() {
	console.log('Environment: ', env);
      });
  };
};
