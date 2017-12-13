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
	var options = { type: "basic",
			iconUrl: "logo.png",
			title: "Clear Nexus",
			message: "Your token has been set" };

	chrome.notifications.create("token-notif-id", options, function() {
	  document.getElementById('save_tkn_cn').disabled = true;
	  input.disabled = true;

	  // Delay a bit the closing of the pop up for better user experience.
	  setTimeout(function() {
	    chrome.notifications.clear("token-notif-id");
	    window.close();
	  }, 1700);
	});
      });
    }
    return token;
  };
};
