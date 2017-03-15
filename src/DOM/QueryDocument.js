"use strict"

exports.queryEmails = function(document_) {
  return function() {
    //Dinamically assign the DOM if this is called in Browser
    var document = document_.value0 || window.document;

    var divs = document.getElementsByClassName('vR');
    var arr = Array.prototype.slice.call(divs);
    var emails = arr.map(function(div) {
      return div.firstChild.getAttribute('email');
    });

    return emails.filter(function(email) { return email != null; });
  };
};
