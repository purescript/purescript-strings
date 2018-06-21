-module(data_string_codeUnits@foreign).
-export([fromCharArray/1, toCharArray/1, singleton/1, '_charAt'/4,  '_toChar'/3,length/1, '_indexOf'/4, '_indexOf\''/5,'_lastIndexOf'/4,'_lastIndexOf\''/5,take/2, drop/2,'_splitAt'/4, countPrefix/2, '_slice'/3, splitAt/2]).

fromCharArray(A) -> case unicode:characters_to_binary(array:to_list(A)) of
  {error, S, _} -> S; % In case of hitting bad character, return initial portion!
  S -> S
end.

toCharArray(S) -> array:from_list(unicode:characters_to_list(S)).

singleton(C) -> <<C>>.

'_charAt'(Just,Nothing,I,S) ->
  case byte_size(S) of
    N when I >= 0, I < N -> Just(binary:at(S, I));
    _ -> Nothing
  end;
'_charAt'(_,Nothing,_,_) -> Nothing.

'_toChar'(Just,_,<<C>>) -> Just(C);
'_toChar'(_,Nothing,_) -> Nothing.

length(S) -> byte_size(S).

'_indexOf'(Just,_,<<>>,_) -> Just(0);
'_indexOf'(Just,Nothing,X,S) ->
  case string:str(unicode:characters_to_list(S), unicode:characters_to_list(X)) of
    0 -> Nothing;
    N -> Just(N-1)
  end.
'_indexOf\''(_Just,Nothing,_X,StartAt,S) when StartAt < 0; StartAt > byte_size(S) -> Nothing;
'_indexOf\''(Just,Nothing,X,StartAt,S) ->
  case '_indexOf'(Just,Nothing,X,drop(StartAt,S)) of
    {just,N} -> Just(N+StartAt);
    _ -> Nothing
  end.

'_lastIndexOf'(Just,_,<<>>,S) -> Just(byte_size(S));
'_lastIndexOf'(Just,Nothing,X,S) ->
    case string:rstr(unicode:characters_to_list(S), unicode:characters_to_list(X)) of
      0 -> Nothing;
      N -> Just(N-1)
    end.
'_lastIndexOf\''(_Just,Nothing,_X,StartAt,S) when StartAt < 0; StartAt > byte_size(S) -> Nothing;
'_lastIndexOf\''(Just,Nothing,X,StartAt,S) ->
  case '_lastIndexOf'(Just,Nothing,X,take(min(byte_size(S), StartAt+byte_size(X)),S)) of
    {just,N} -> Just(N);
    _ -> Nothing
  end.

take(N,_) when N < 0 -> <<>>;
take(N,S) when N > byte_size(S) -> S;
take(N,S) -> binary:part(S, {0, N}).

drop(N,S) ->
  M = max(0, min(N, byte_size(S))),
  binary:part(S, {M, byte_size(S)-M}).

'_splitAt'(Just,_,I,S) when I >= 0, I =< byte_size(S) ->
  Just(array:from_list([binary:part(S, {0, I}), binary:part(S, {I, byte_size(S)-I})]));
'_splitAt'(_,Nothing,_,_) -> Nothing.


countPrefixImpl(_, S, I) when I > byte_size(S) -> I;
countPrefixImpl(P, S, I) -> 
    case P(binary:at(S, I)) of
        true -> countPrefixImpl(P, S, I+1);
        false -> I
    end.

countPrefix(P, S) -> countPrefixImpl(P, S, 0).

'_slice'(B, E, S) -> binary:part(S, B, E).

splitAt(I, S) ->
    Start = binary:part(S, 0, I),
    End = binary:part(S, I, (binary:size(S)-1)),
    #{before => Start, 'after' => End}.
