var hasArrayFrom = typeof Array.from === 'function';
var hasStringIterator = 
  typeof Symbol !== 'undefined' &&
  Symbol != null &&
  typeof Symbol.iterator !== 'undefined' &&
  typeof String.prototype[Symbol.iterator] === 'function';

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
