-module(data_string_unsafe@foreign).
-export([charAt/2, char/1]).

charAt(I,S) -> binary:at(S, I).
char(<<C>>) -> C.
