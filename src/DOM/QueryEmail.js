exports.queryEmail = function() {
  var inputs = document.getElementsByTagName('input');
  var arr = Array.prototype.slice.call(inputs);

  var target = arr.filter(function(input) {
    return input.getAttribute('name') === 'to';
  })[0];

  if (!target) { return undefined; }

  return target.defaultValue;
};
