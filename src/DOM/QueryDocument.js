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

exports.uncurriedPasteLink = function(document_,  link) {
  return function() {
    //Dinamically assign the DOM if this is called in Browser
    var document = document_.value0 || window.document;
    var div = document.querySelector('div.Am.Al.editable.LW-avf');
    var signature = document.querySelector('div[data-smartmail=gmail_signature]');
    var a = document.createElement('a');

    a.innerText = 'Click here to unsubscribe';
    a.href = link;

    if (signature) {
      var ref = signature.firstChild;

      signature.insertBefore(a, ref);
      return link;
    }

    if (div) {
      div.appendChild(a);
      return link;
    }

    return null;
  };
};
