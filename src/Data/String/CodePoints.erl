-module(data_string_codePoints@foreign).
-export(['_unsafeCodePointAt0'/1
       , '_codePointAt'/6
       , '_fromCodePointArray'/2
       , '_singleton'/1
       , '_take'/1
       , '_toCodePointArray'/1]).

'_unsafeCodePointAt0'(_Fallback) -> fun (Str) ->
    case unicode:characters_to_list(Str) of
        [CP|_] -> CP;
        _ -> error("told you I was unsafe")
    end
end.

'_codePointAt'(_Fallback, Just, Nothing, _unsafeCodePointAt0, Index, Str) ->
    Cps = unicode:characters_to_list(Str, utf8),
    case length(Cps) of
        Length when Index < 0, Index >= Length -> Nothing;
        _ -> Just(lists:nth(Index+1, Cps))
    end
.

'_fromCodePointArray'(_Fallback, Array) ->
  List = array:to_list(Array),
  unicode:characters_to_binary(List, utf8).

'_singleton'(_Fallback) -> fun (CP) ->
    unicode:characters_to_binary([CP], utf8)
end.
'_take'(_Fallback) ->
    fun (N) ->
        fun (S) ->
            unicode:characters_to_binary(lists:sublist(unicode:characters_to_list(S, utf8), N), utf8)
        end
    end.
'_toCodePointArray'(_Fallback) -> fun (_UnsafeCodePointAt0) ->
    fun (Str) ->
        array:from_list(unicode:characters_to_list(Str, utf8))
    end
end.
