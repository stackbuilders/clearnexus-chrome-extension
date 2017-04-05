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

    //Dinamically  assign objects for testing purposes
    signature = document_.value0 ? signature.value0 : signature;
    div = document_.value0 ? div.value0 : div;

    a.innerText = '\nClick here to unsubscribe\n';
    a.href = link;

    if (signature && div) {
      var ref = div.childNodes[2];
      div.insertBefore(a, ref);

      return a.innerText;
    }

    if (div) {
      div.appendChild(a);
      return a.innerText;
    }

    return null;
  };
};
