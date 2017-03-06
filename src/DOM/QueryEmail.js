"use strict"

function queryEmails_(isBrowser) {
  return function(document_) {
    return function() {
      //Dinamically assign the DOM if this is called in Browser
      if(isBrowser) { document_ = document; }
      var inputs = document_.getElementsByTagName('input');
      var arr = Array.prototype.slice.call(inputs);
      var targets = arr.filter(function(input) {
        return input.getAttribute('name') === 'to';
      });

      return targets.map(function(input) { return input.value; });
    };
  };
};


module.exports = {
  // Suitable for Testing, it receives a mock DOM object
  queryEmails_: queryEmails_(false),

  // Suitable for Browser, it uses the browser-dom
  queryEmails: queryEmails_(true)(null)
};
