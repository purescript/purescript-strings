-module(data_string_codeUnits@foreign).
-export([fromCharArray/1, toCharArray/1, singleton/1, '_charAt'/4,  '_toChar'/3,length/1, '_indexOf'/4, '_indexOfStartingAt'/5,'_lastIndexOf'/4,'_lastIndexOfStartingAt'/5,take/2, drop/2, countPrefix/2, '_slice'/3, splitAt/2]).

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
  end.

'_toChar'(Just,_,<<C>>) -> Just(C);
'_toChar'(_,Nothing,_) -> Nothing.

length(S) -> byte_size(S).

'_indexOf'(Just,_,<<>>,_) -> Just(0);
'_indexOf'(Just,Nothing,X,S) ->
  case binary:match(S, X) of
    nomatch -> Nothing;
    { Start, _Length } -> Just(Start)
  end.
'_indexOfStartingAt'(_Just,Nothing,_X,StartAt,S) when StartAt < 0; StartAt > byte_size(S) -> Nothing;
'_indexOfStartingAt'(Just,_Nothing,<<>>,StartAt,_S) -> Just(StartAt);
'_indexOfStartingAt'(Just,Nothing,X,StartAt,S) ->
  case binary:match(S, X, [{scope, {StartAt, byte_size(S)-StartAt}}]) of
    nomatch -> Nothing;
    { Start, _Length } -> Just(Start)
  end.

'_lastIndexOf'(Just,_,<<>>,S) -> Just(byte_size(S));
'_lastIndexOf'(Just,Nothing,X,S) ->
  % there is no binary reverse match, and matches returns only nonoverlapping so would not always include the last match
  case string:find(S, X, trailing) of
    nomatch -> Nothing;
    Substr -> Just(byte_size(S) - byte_size(Substr))
  end.
'_lastIndexOfStartingAt'(Just,Nothing,X,StartAt,S) ->
  L = byte_size(S),
  S0 = min(L, max(0, StartAt)),
  Part = binary:part(S, {0, S0+byte_size(X)}),
  case '_lastIndexOf'(Just,Nothing,X,Part) of
    {just,N} -> Just(N);
    _ -> Nothing
  end.

take(N,_) when N < 0 -> <<>>;
take(N,S) when N > byte_size(S) -> S;
take(N,S) -> binary:part(S, {0, N}).

drop(N,S) ->
  M = max(0, min(N, byte_size(S))),
  binary:part(S, {M, byte_size(S)-M}).

countPrefixImpl(_, S, I) when I >= byte_size(S) -> I;
countPrefixImpl(P, S, I) -> 
    case P(binary:at(S, I)) of
        true -> countPrefixImpl(P, S, I+1);
        false -> I
    end.

countPrefix(P, S) -> countPrefixImpl(P, S, 0).

'_slice'(B, E, S) -> 
  L = byte_size(S),
  BB = case B < 0 of
        true -> L + B;
        false -> B
       end,
  EE = case E < 0 of
        true -> L + E;
        false -> E
       end,
  binary:part(S, BB, (EE-BB)).

splitAt(I0, S) ->
    I = min(max(0, I0), erlang:byte_size(S)),
    Start = binary:part(S, 0, I),
    End = binary:part(S, I, erlang:byte_size(S)-I),
    #{before => Start, 'after' => End}.
