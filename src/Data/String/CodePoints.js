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

exports._unsafeCodePointAt0 = function (fallback) {
  return hasCodePointAt
    ? function (str) { return str.codePointAt(0); }
    : fallback;
};

exports._codePointAt = function (fallback) {
  return function (Just) {
    return function (Nothing) {
      return function (unsafeCodePointAt0) {
        return function (index) {
          return function (str) {
            var length = str.length;
            if (index < 0 || index >= length) return Nothing;
            if (hasArrayFrom) {
              var cps = Array.from(str);
              if (index >= cps.length) return Nothing;
              return Just(unsafeCodePointAt0(cps[index]));
            } else if (hasStringIterator) {
              var iter = str[Symbol.iterator]();
              for (var i = index;; --i) {
                var o = iter.next();
                if (o.done) return Nothing;
                if (i === 0) return Just(unsafeCodePointAt0(o.value));
              }
            }
            return fallback(index)(str);
          };
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
    ? function (cps) {
      // Function.prototype.apply will fail for very large second parameters,
      // so we don't use it for arrays with 10KB or more entries.
      if (cps.length < 10240) {
        return String.fromCodePoint.apply(String, cps);
      }
      return cps.map(singleton).join("");
    }
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
  return function (unsafeCodePointAt0) {
    if (hasArrayFrom) {
      return function (str) {
        return Array.from(str, unsafeCodePointAt0);
      };
    } else if (hasStringIterator) {
      return function (str) {
        var accum = [];
        var iter = str[Symbol.iterator]();
        for (;;) {
          var o = iter.next();
          if (o.done) return accum;
          accum.push(unsafeCodePointAt0(o.value));
        }
      };
    }
    return fallback;
  };
};
