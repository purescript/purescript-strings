"use strict";

export var fromCharArray = function (a) {
  return a.join("");
};

export var toCharArray = function (s) {
  return s.split("");
};

export var singleton = function (c) {
  return c;
};

export var _charAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.charAt(i)) : nothing;
      };
    };
  };
};

export var _toChar = function (just) {
  return function (nothing) {
    return function (s) {
      return s.length === 1 ? just(s) : nothing;
    };
  };
};

export var length = function (s) {
  return s.length;
};

export var countPrefix = function (p) {
  return function (s) {
    var i = 0;
    while (i < s.length && p(s.charAt(i))) i++;
    return i;
  };
};

export var _indexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.indexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

export var _indexOfStartingAt = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          if (startAt < 0 || startAt > s.length) return nothing;
          var i = s.indexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

export var _lastIndexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.lastIndexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

export var _lastIndexOfStartingAt = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          var i = s.lastIndexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

export var take = function (n) {
  return function (s) {
    return s.substr(0, n);
  };
};

export var drop = function (n) {
  return function (s) {
    return s.substring(n);
  };
};

export var _slice = function (b) {
  return function (e) {
    return function (s) {
      return s.slice(b,e);
    };
  };
};

export var splitAt = function (i) {
  return function (s) {
    return { before: s.substring(0, i), after: s.substring(i) };
  };
};
