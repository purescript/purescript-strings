"use strict";

exports.showRegexImpl = function (r) {
  return "" + r;
};

exports.regexImpl = function (left) {
  return function (right) {
    return function (s1) {
      return function (s2) {
        try {
          return right(new RegExp(s1, s2));
        } catch (e) {
          return left(e.message);
        }
      };
    };
  };
};

exports.source = function (r) {
  return r.source;
};

exports.flagsImpl = function (r) {
  return {
    multiline: r.multiline,
    ignoreCase: r.ignoreCase,
    global: r.global,
    dotAll: r.dotAll,
    sticky: !!r.sticky,
    unicode: !!r.unicode
  };
};

exports.test = function (r) {
  return function (s) {
    var lastIndex = r.lastIndex;
    var result = r.test(s);
    r.lastIndex = lastIndex;
    return result;
  };
};

exports._match = function (just) {
  return function (nothing) {
    return function (r) {
      return function (s) {
        var m = s.match(r);
        if (m == null || m.length === 0) {
          return nothing;
        } else {
          for (var i = 0; i < m.length; i++) {
            m[i] = m[i] == null ? nothing : just(m[i]);
          }
          return just(m);
        }
      };
    };
  };
};

exports.replace = function (r) {
  return function (s1) {
    return function (s2) {
      return s2.replace(r, s1);
    };
  };
};

exports._replaceBy = function (just) {
  return function (nothing) {
    return function (r) {
      return function (f) {
        return function (s) {
          return s.replace(r, function (match) {
            var groups = [];
            var group, i = 1;
            while (typeof (group = arguments[i++]) !== "number") {
              groups.push(group == null ? nothing : just(group));
            }
            return f(match)(groups);
          });
        };
      };
    };
  };
};

exports._search = function (just) {
  return function (nothing) {
    return function (r) {
      return function (s) {
        var result = s.search(r);
        return result === -1 ? nothing : just(result);
      };
    };
  };
};

exports.split = function (r) {
  return function (s) {
    return s.split(r);
  };
};
