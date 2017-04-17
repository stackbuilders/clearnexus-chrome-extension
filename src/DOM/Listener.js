"use strict"

// This is a special pattern to use EffFn1 type
exports.getStoredToken = function(cb) {
  chrome.storage.sync.get('authtoken', cb);
};


/* Custom setTimeout to avoid accumulative background callbacks */

var timeOutId;

exports.safeSetTimeout = function(cb) {
  clearTimeout(timeOutId);
  timeOutId = setTimeout(cb, 500);
};
