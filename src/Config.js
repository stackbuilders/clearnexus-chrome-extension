exports.loadEnvironment = function (callback) {
  chrome.storage.sync.get('environment', callback)
}
