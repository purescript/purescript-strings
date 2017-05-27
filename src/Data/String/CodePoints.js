var hasArrayFrom = typeof Array.from === 'function';
var hasStringIterator =
  typeof Symbol !== 'undefined' &&
  Symbol != null &&
  typeof Symbol.iterator !== 'undefined' &&
  typeof String.prototype[Symbol.iterator] === 'function';
var hasFromCodePoint = typeof String.prototype.fromCodePoint === 'function';
var hasCodePointAt = typeof String.prototype.codePointAt === 'function';

exports._codePointAt = function (fallback) {
  return function (Just) {
    return function (Nothing) {
      return function (index) {
        return function (str) {
          var length = str.length;
          if (index < 0 || index >= length) return Nothing;
          if (hasArrayFrom && hasCodePointAt) {
            var cps = Array.from(str);
            if (index >= cps.length) return Nothing;
            return Just(cps[index].codePointAt(0));
          } else if (hasStringIterator) {
            var iter = str[Symbol.iterator]();
            for (var i = index;; --i) {
              var o = iter.next();
              if (o.done) return Nothing;
              if (i == 0) return Just(o.value);
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

exports.fromCodePointArray = hasFromCodePoint
  ? function (cps) { return String.fromCodePoint.apply(String, cps); }
  : function (cps) { return cps.map(fromCodePoint).join(''); };

exports.singleton = hasFromCodePoint ? String.fromCodePoint : fromCodePoint;

exports._take = function (fallback) {
  return function (n) {
    if (hasArrayFrom) {
      return function (str) {
        return Array.from(str).slice(0, Math.max(0, n)).join('');
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
    return fallback;
  };
};

exports._toCodePointArray = function (fallback) {
  if (hasArrayFrom && hasCodePointAt) {
    return function (str) {
      return Array.from(str, function (x) { return x.codePointAt(0); });
    };
  } else if (hasStringIterator && hasCodePointAt) {
    return function (str) {
      var accum = [];
      var iter = str[Symbol.iterator]();
      for (;;) {
        var o = iter.next();
        if (o.done) return accum;
        accum.push(o.value.codePointAt(0));
      }
    };
  }
  return fallback;
};

function fromCodePoint(cp) {
  if (cp <= 0xFFFF) return String.fromCharCode(cp);
  var cu1 = String.fromCharCode(Math.floor((cp - 0x10000) / 0x400) + 0xD800);
  var cu2 = String.fromCharCode((cp - 0x10000) % 0x400 + 0xDC00);
  return cu1 + cu2;
}
