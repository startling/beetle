$(document).ready(function () {
  start($("#content"));
  document.title = metadata.title;
});

var beetle = {
  switch___to: function (element, fn) {
    element.empty();
    fn(element);
  },

  paragraph: function (element, string) {
    var $par = $("<p/>");
    $par.append(document.createTextNode(string));
    element.append($par);
  },

  field: function (element, fn) {
    var $form = $("<form/>");
    var $text = $("<input/>", {type : "text"});
    $form.append($text);
    $form.submit(function () {
      fn(element, $text.val())(element);
      return false;
    });
    element.append($form);
  },

  link: function (_, f) {
    return function (element, g) {
      var $a = $('<a/>');
      $a.addClass("link");
      $a.click(function () {
        g(element);
      });
      f($a);
      element.append($a);
    };
  },

  exec: function (element, f) {
    f(element);
  },

  debug___print: function (_, x) {
    console.log(x);
  },

  $if: function (_, c) {
    return function (_, f) {
      return function (element, g) {
        if (c) { f(element); } else { g(element); };
      }
    }
  },
  _______t: true,
  _______f: false
 

};
