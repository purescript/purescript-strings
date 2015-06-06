/* global exports */
"use strict";

// module Data.String

exports._charAt = function(i, s, Just, Nothing) {
  return i >= 0 && i < s.length ? Just(s.charAt(i)) : Nothing;
};

exports._charCodeAt = function(i, s, Just, Nothing) {
  return i >= 0 && i < s.length ? Just(s.charCodeAt(i)) : Nothing;
};

exports.fromCharArray = function(a) {
  return a.join('');
};

exports._indexOf = function(just, nothing, x, s) {
  var i = s.indexOf(x);
  return i == -1 ? nothing : just(i);
};

exports._indexOf$prime = function(just, nothing, x, startAt, s) {
  var i = s.indexOf(x, startAt);
  return i == -1 ? nothing : just(i);
};

exports._lastIndexOf = function(just, nothing, x, s) {
  var i = s.lastIndexOf(x);
  return i == -1 ? nothing : just(i);
};

exports._lastIndexOf$prime = function(just, nothing, x, startAt, s) {
  var i = s.lastIndexOf(x, startAt);
  return i == -1 ? nothing : just(i);
};

exports.length = function(s) {
  return s.length;
};

exports.localeCompare = function(lt, eq, gt, s1, s2) {
  var result = s1.localeCompare(s2);
  return result < 0 ? lt : result > 1 ? gt : eq;
};

exports.replace = function(s1) {
  return function(s2) {
    return function(s3) {
      return s3.replace(s1, s2);
    };
  };
};

exports.take = function(n) {
  return function(s) {
    return s.substr(0, n);
  };
};

exports.drop = function(n) {
  return function(s) {
    return s.substr(n);
  };
};

exports.split = function(sep) {
  return function(s) {
    return s.split(sep);
  };
};

exports.toCharArray = function(s) {
  return s.split('');
};

exports.toLower = function(s) {
  return s.toLowerCase();
};

exports.toUpper = function(s) {
  return s.toUpperCase();
};

exports.trim = function(s) {
  return s.trim();
};

exports.joinWith = function(s) {
  return function(xs) {
    return xs.join(s);
  };
};