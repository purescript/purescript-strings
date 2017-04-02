-module(data_string@foreign).
-export(['_charAt'/4, singleton/1, '_charCodeAt'/4, '_toChar'/3, fromCharArray/1, '_indexOf'/4, '_indexOf\''/5, '_lastIndexOf'/4,'_lastIndexOf\''/5, length/1, take/2, drop/2, count/2, split/2, '_splitAt'/4, toCharArray/1, toUpper/1, toLower/1, trim/1, joinWith/2]).

'_charAt'(Just,Nothing,I,S) ->
  case byte_size(S) of
    N when I >= 0, I < N -> Just(binary:at(S, I));
    _ -> Nothing
  end;
'_charAt'(Just,Nothing,I,S) -> Nothing.

singleton(C) -> <<C>>.

'_charCodeAt'(Just,Nothing,I,S) -> '_charAt'(Just,Nothing,I,S).

'_toChar'(Just,Nothing,<<C>>) -> Just(C);
'_toChar'(Just,Nothing,_) -> Nothing.

fromCharArray(A) -> case unicode:characters_to_binary(array:to_list(A)) of
  {error, S, _} -> S; % In case of hitting bad character, return initial portion!
  S -> S
end.

'_indexOf'(Just,Nothing,<<>>,_) -> Just(0);
'_indexOf'(Just,Nothing,X,S) ->
  case string:str(unicode:characters_to_list(S), unicode:characters_to_list(X)) of
    0 -> Nothing;
    N -> Just(N-1)
  end.
'_indexOf\''(Just,Nothing,X,StartAt,S) when StartAt < 0; StartAt > byte_size(S) -> Nothing;
'_indexOf\''(Just,Nothing,X,StartAt,S) ->
  case '_indexOf'(Just,Nothing,X,drop(StartAt,S)) of
    {just,N} -> Just(N+StartAt);
    _ -> Nothing
  end.

'_lastIndexOf'(Just,Nothing,<<>>,S) -> Just(byte_size(S));
'_lastIndexOf'(Just,Nothing,X,S) ->
    case string:rstr(unicode:characters_to_list(S), unicode:characters_to_list(X)) of
      0 -> Nothing;
      N -> Just(N-1)
    end.
'_lastIndexOf\''(Just,Nothing,X,StartAt,S) when StartAt < 0; StartAt > byte_size(S) -> Nothing;
'_lastIndexOf\''(Just,Nothing,X,StartAt,S) ->
  case '_lastIndexOf'(Just,Nothing,X,take(min(byte_size(S), StartAt+byte_size(X)),S)) of
    {just,N} -> Just(N);
    _ -> Nothing
  end.

length(S) -> byte_size(S).

% '_localeCompare'(Lt,Eq,Gt,S1,S2) -> error("not implemented").
% replace(S1,S2,S3) -> error("not implemented").
% replaceAll(S1,S2,S3)-> error("not implemented").

take(N,S) when N < 0 -> <<>>;
take(N,S) when N > byte_size(S) -> S;
take(N,S) -> binary:part(S, {0, N}).

drop(N,S) ->
  M = max(0, min(N, byte_size(S))),
  binary:part(S, {M, byte_size(S)-M}).

count(P,<<C,S/binary>>) ->
  case P(C) of
    true -> 1+count(P,S);
    false -> 0
  end;
count(_,_) -> 0.

split(Sep,S) ->
  Split = re:split(S, Sep),
  Res = case {Sep,S} of
    {_,<<>>} -> lists:droplast(Split);
    {<<>>,_} -> lists:droplast(Split);
    _ -> Split
  end,
  array:from_list(Res).

'_splitAt'(Just,Nothing,I,S) when I >= 0, I =< byte_size(S) ->
  Just(array:from_list([binary:part(S, {0, I}), binary:part(S, {I, byte_size(S)-I})]));
'_splitAt'(Just,Nothing,I,S) -> Nothing.

toCharArray(S) -> array:from_list(unicode:characters_to_list(S)).

% TODO ugh
toLower(S) -> unicode:characters_to_binary(string:to_lower(unicode:characters_to_list(S))).

toUpper(S) -> unicode:characters_to_binary(string:to_upper(unicode:characters_to_list(S))).

trim(S) -> re:replace(S, "^\\s*(.*?)\\s*$","\\1", [{return, binary}]).

joinWith(S, XS) ->
  XS1 = lists:map(fun unicode:characters_to_list/1, array:to_list(XS)),
  Res = string:join(XS1, unicode:characters_to_list(S)),
  unicode:characters_to_binary(Res).
