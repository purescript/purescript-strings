"use strict";
/* global Symbol */

var hasArrayFrom = typeof Array.from === "function";
var hasStringIterator =
  typeof Symbol !== "undefined" &&
  Symbol != null &&
  typeof Symbol.iterator !== "undefined" &&
  typeof String.prototype[Symbol.iterator] === "function";
var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
var hasCodePointAt = typeof String.prototype.codePointAt === "function";

var codePointAt0 = hasCodePointAt
  ? function (str) { return str.codePointAt(0); }
  : function (str) {
    if (str.length === 1) {
      return str.charCodeAt(0);
    }
    return ((str.charCodeAt(0) - 0xD800) * 0x400 + (str.charCodeAt(1) - 0xDC00) + 0x10000);
  };

exports._codePointAt = function (fallback) {
  return function (Just) {
    return function (Nothing) {
      return function (index) {
        return function (str) {
          var length = str.length;
          if (index < 0 || index >= length) return Nothing;
          if (hasArrayFrom) {
            var cps = Array.from(str);
            if (index >= cps.length) return Nothing;
            return Just(codePointAt0(cps[index]));
          } else if (hasStringIterator) {
            var iter = str[Symbol.iterator]();
            for (var i = index;; --i) {
              var o = iter.next();
              if (o.done) return Nothing;
              if (i === 0) return Just(codePointAt0(o.value));
            }
          }
          return fallback(index)(str);
        };
      };
    };
  };
};

exports._count = function (isLead) {
  return function (isTrail) {
    return function (unsurrogate) {
      return function (pred) {
        return function (str) {
          var cpCount = 0;
          for (var cuCount = 0; cuCount < str.length; ++cuCount) {
            var lead = str.charCodeAt(cuCount);
            var cp = lead;
            if (isLead(lead) && cuCount + 1 < str.length) {
              var trail = str.charCodeAt(cuCount + 1);
              if (isTrail(trail)) {
                cp = unsurrogate(lead)(trail);
                ++cuCount;
              }
            }
            if (!pred(cp)) return cpCount;
            ++cpCount;
          }
          return cpCount;
        };
      };
    };
  };
};

exports._fromCodePointArray = function (singleton) {
  return hasFromCodePoint
    // TODO: using F.p.apply here will fail for very large strings; use alternative implementation for very large strings
    ? function (cps) { return String.fromCodePoint.apply(String, cps); }
    : function (cps) { return cps.map(singleton).join(""); };
};

exports._singleton = function (fallback) {
  return hasFromCodePoint ? String.fromCodePoint : fallback;
};

exports._take = function (fallback) {
  return function (n) {
    if (hasArrayFrom) {
      return function (str) {
        return Array.from(str).slice(0, Math.max(0, n)).join("");
      };
    } else if (hasStringIterator) {
      return function (str) {
        var accum = "";
        var iter = str[Symbol.iterator]();
        for (var i = 0; i < n; ++i) {
          var o = iter.next();
          if (o.done) return accum;
          accum += o.value;
        }
        return accum;
      };
    }
    return fallback(n);
  };
};

exports._toCodePointArray = function (fallback) {
  if (hasArrayFrom) {
    return function (str) {
      return Array.from(str, codePointAt0);
    };
  } else if (hasStringIterator) {
    return function (str) {
      var accum = [];
      var iter = str[Symbol.iterator]();
      for (;;) {
        var o = iter.next();
        if (o.done) return accum;
        accum.push(codePointAt0(o.value));
      }
    };
  }
  return fallback;
};
