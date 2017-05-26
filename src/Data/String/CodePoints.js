var hasArrayFrom = typeof Array.from === 'function';
var hasStringIterator = 
  typeof Symbol !== 'undefined' &&
  Symbol != null &&
  typeof Symbol.iterator !== 'undefined' &&
  typeof String.prototype[Symbol.iterator] === 'function';
var hasFromCodePoint = typeof String.prototype.fromCodePoint === 'function';

exports._codePointAt = function (fallback) {
  return function (Just) {
    return function (Nothing) {
      return function (relIndex) {
        return function (str) {
          var length = str.length;
          if (length <= relIndex) return Nothing;
          var index = relIndex < 0 ? ((relIndex % length) + length) % length : relIndex;
          if (typeof String.prototype.codePointAt === 'function') {
            var cp = str.codePointAt(index);
            return cp == null ? Nothing : Just(cp);
          } else if (hasArrayFrom) {
            var cps = Array.from(str);
            if (cps.length <= index) return Nothing;
            return Just(cps[index]);
          } else if (hasStringIterator) {
            var i = index;
            var iter = str[Symbol.iterator]();
            for (;;) {
              var o = iter.next();
              if (o.done) return Nothing;
              if (i == 0) return Just(o.value);
              --i;
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
          for (var cuCount = 0, cpCount = 0; cuCount < str.length; ++cuCount, ++cpCount) {
            var lead = str.charCodeAt(cuCount);
            var cp = lead;
            if (isLead(lead) && cuCount + 1 < str.length) {
              var trail = str.charCodeAt(cuCount + 1);
              if (isTrail(trail)) {
                cp = unsurrogate(lead, trail);
                ++cuCount;
              }
            }
            if (!pred(cp)) return cpCount;
          }
          return str.length;
        };
      };
    };
  };
};

exports.fromCodePointArray = function (cps) {
  if (hasFromCodePoint) {
    return String.fromCodePoint.apply(String, cps);
  }
  return cps.map(fromCodePoint).join('');
};

exports.singleton = hasFromCodePoint ? String.fromCodePoint : fromCodePoint;

exports._take = function (fallback) {
  return function (n) {
    return function (str) {
      if (hasArrayFrom) {
        return Array.from(str);
      } else if (hasStringIterator) {
        var accum = "";
        var iter = str[Symbol.iterator]();
        for (var i = 0; i < n; ++i) {
          var o = iter.next();
          if (o.done) return accum;
          accum += o.value;
        }
        return accum;
      }
      return fallback(str);
    };
  };
};

exports._toCodePointArray = function (fallback) {
  return function (str) {
    if (hasArrayFrom) {
      return Array.from(str);
    } else if (hasStringIterator) {
      var accum = [];
      var iter = str[Symbol.iterator]();
      for (;;) {
        var o = iter.next();
        if (o.done) return accum;
        accum.push(o.value);
      }
    }
    return fallback(str);
  };
};

function fromCodePoint(cp) {
  if (cp <= 0xFFFF) return String.fromCharCode(cp);
  var cu1 = String.fromCharCode(Math.floor((cp - 0x10000) / 0x400) + 0xD800);
  var cu2 = String.fromCharCode((cp - 0x10000) % 0x400 + 0xDC00);
  return cu1 + cu2;
}
