-module(data_string_unsafe@foreign).
-export([charCodeAt/2, charAt/2, char/1]).

charCodeAt(I,S) -> binary:at(S, I).
charAt(I,S) -> binary:at(S, I).
char(<<C>>) -> C.
