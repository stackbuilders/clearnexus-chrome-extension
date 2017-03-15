"use strict"

exports.uncurriedSaveToken = function(document_, chrome_) {
  return function() {
    //Dinamically assign the DOM and Chrome if this is called in Browser
    var document = document_.value0 || window.document;
    var chrome =  chrome_.value0 || window.chrome;

    var input = document.getElementById('authtoken_cn');
    var token = input ? input.value : null;

    if (token) {
      chrome.storage.sync.set({
	authtoken: token
      }, function() {
	alert('Your token has been saved');
	document.getElementById('save_tkn_cn').disabled = true;
	input.disabled = true;
      });
    }

    return token;
  };
};
