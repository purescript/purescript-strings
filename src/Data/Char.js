/* global exports */
"use strict";

// module Data.Char

exports.toString = function (c) {
  return c;
};

exports.toCharCode = function (c) {
  return c.charCodeAt(0);
};

exports.fromCharCode = function (c) {
  return String.fromCharCode(c);
};

exports.toLowerChar = function (c) {
  return c.toLowerCase();
};

exports.toUpperChar = function (c) {
  return c.toUpperCase();
};
