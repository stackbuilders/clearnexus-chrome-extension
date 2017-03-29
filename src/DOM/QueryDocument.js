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

exports.pasteLink = function(document_,  link) {
  return function() {
    //Dinamically assign the DOM if this is called in Browser
    var document = document_.value0 || window.document;
    var div = document.querySelector('div.Am.Al.editable.LW-avf');
    var text = '\n ' + link;

    if (div) {
      text = div.innerText + text;
      div.innerText =  text;
      return text;
    }

    return null;
  };
};
