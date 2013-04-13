$(document).ready(function () {
  start($("#thing"));
});

function switchto (element, fn) {
  element.empty();
  fn(element);
}

function field (element, fn) {
  var $form = $("<form/>");
  var $text = $("<input/>", {type : "text"});
  $form.append($text);
  $form.submit(function () {
    fn(element, $text.val());
    return false;
  });
  element.append($form);
}

function link (_, f) {
  return function (element, g) {
    var $a = $('<a>');
    $a.click(function () {
      g(element);
    });
    f($a);
    element.append($a);
  }
}

function exec (element, f) {
  f(element);
}

function if_ (_, c) {
  return function (_, f) {
    return function (element, g) {
      if (c) { f(element); } else { g(element); };
    }
  }
}
