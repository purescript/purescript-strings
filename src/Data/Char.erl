-module(data_char@foreign).
-export([toCharCode/1, fromCharCode/1, toLower/1, toUpper/1]).

toCharCode(C) -> C.
fromCharCode(N) -> N.
toLower(C) -> string:to_lower(C).
toUpper(C) -> string:to_upper(C).
