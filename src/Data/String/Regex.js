/* global exports */
"use strict";

// module Data.String.Regex

exports.showRegex$prime = function(r) {
  return '' + r;
};

exports.regex$prime = function(s1) {
  return function(s2) {
    return new RegExp(s1, s2);
  };
};

exports.source = function(r) {
  return r.source;
};

exports.flags = function(r) {
  return {
    multiline: r.multiline,
    ignoreCase: r.ignoreCase,
    global: r.global,
    sticky: !!r.sticky,
    unicode: !!r.unicode
  };
};

exports.test = function(r) {
  return function(s) {
    return r.test(s);
  };
};

exports._match = function(r, s, Just, Nothing) {
  var m = s.match(r);
  if (m == null) {
    return Nothing;
  } else {
    var list = [];
    for (var i = 0; i < m.length; i++) {
      list.push(m[i] == null ? Nothing : Just(m[i]));
    }
    return Just(list);
  }
};

exports.replace = function(r) {
  return function(s1) {
    return function(s2) {
      return s2.replace(r, s1);
    };
  };
};

exports.replace$prime = function(r) {
  return function(f) {
    return function(s2) {
      return s2.replace(r, function(match) {
        return f(match)(Array.prototype.splice.call(arguments, 1, arguments.length - 3));
      });
    };
  };
};

exports.search = function(r) {
  return function(s) {
    return s.search(r);
  };
};

exports.split = function(r) {
  return function(s) {
    return s.split(r);
  };
};