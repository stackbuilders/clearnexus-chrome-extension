"use strict"

// This is a special pattern to use EffFn1 type
exports.getStoredToken = function(cb) {
  chrome.storage.sync.get('authtoken', cb);
};
