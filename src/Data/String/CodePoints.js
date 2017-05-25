const hasArrayFrom = typeof Array.from === 'function';
const hasStringIterator = 
  typeof Symbol !== 'undefined' &&
  Symbol != null &&
  typeof Symbol.iterator !== 'undefined' &&
  typeof String.prototype[Symbol.iterator] === 'function';

exports._codePointAt = function (fallback) {
  return function (Just) {
    return function (Nothing) {
      return function (relIndex) {
        return function (str) {
          let length = str.length;
          if (length <= relIndex) return Nothing;
          let index = relIndex < 0 ? ((relIndex % length) + length) % length : relIndex;
          if (typeof String.prototype.codePointAt === 'function') {
            let cp = str.codePointAt(index);
            return cp == null ? Nothing : Just(cp);
          } else if (hasArrayFrom) {
            let cps = Array.from(str);
            if (cps.length <= index) return Nothing;
            return Just(cps[index]);
          } else if (hasStringIterator) {
            let iter = str[Symbol.iterator]();
            for (;;) {
              let { value, done } = iter.next();
              if (done) return Nothing;
              if (i == 0) return Just(value);
              --i;
            }
          }
          return fallback(index)(str);
        };
      };
    };
  };
};

exports._toCodePointArray = function (str) {
  if (hasArrayFrom) {
    return Array.from(str);
  } else if (hasStringIterator) {
    let accum = [];
    let iter = str[Symbol.iterator]();
    for (;;) {
      let { value, done } = iter.next();
      if (done) return accum;
      accum.push(value);
    }
  }
  let accum = [];
  for (let cuCount = 0; cuCount < str.length; ++cuCount) {
    let cu = str[cuCount];
    let cp = cu;
    if (isLead(cu) && cuCount + 1 < str.length) {
      let lead = cu;
      let trail = str[cuCount + 1];
      if (isTrail(trail)) {
        cp = unsurrogate(lead, trail);
      }
    }
    accum.push(cp);
  }
  return accum;
};

function isLead(cu) { return 0xD800 <= cu && cu <= 0xDBFF; }
function isTrail(cu) { return 0xDC00 <= cu && cu <= 0xDFFF; }
function unsurrogate(h, l) {
  return (h - 0xD800) * 0x400 + (l - 0xDC00) + 0x10000;
}
