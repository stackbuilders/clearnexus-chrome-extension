"use strict"

// This is a special pattern to use EffFn1 type
exports.getStoredToken = function(cb) {
  chrome.storage.sync.get('authtoken', cb);
};


exports.uncurriedAddEventListener = function(selector, event, listener, useCapture) {
  return function() {
    var div = document.querySelector(selector);

    if(div){
      return div.addEventListener(event, listener, useCapture);
    }

    return null;
  };

};