-module(data_string_regex@foreign).
-export([showRegexImpl/1, regexImpl/4, source/1, flagsImpl/1, test/2, '_match'/4, replace/3, '_replaceBy'/5, '_search'/4, split/2]).

showRegexImpl({_,{S,_}}) -> S.

flags_to_options([], Acc) -> {opts, Acc};
flags_to_options([$g|Flags], Acc) -> flags_to_options(Flags, Acc);
flags_to_options([$i|Flags], Acc) -> flags_to_options(Flags, [caseless|Acc]);
flags_to_options([$m|Flags], Acc) -> flags_to_options(Flags, [multiline|Acc]);
flags_to_options([$y|Flags], Acc) -> flags_to_options(Flags, Acc);
flags_to_options([$u|Flags], Acc) -> flags_to_options(Flags, [ucp|[unicode|Acc]]);
flags_to_options(_,_) -> err.

is_global(S) -> case string:chr(unicode:characters_to_list(S), $g) of
  0 -> false;
  _ -> true
end.

regexImpl(Left,Right,S1,S2) ->
  FlagsStr = unicode:characters_to_list(S2),
  case flags_to_options(FlagsStr, []) of
    {opts, Opts} ->
      case re:compile(S1,Opts) of
        {ok, R} -> Right({R,{S1,S2}});
        {error, {Err, _}} -> Left(Err)
      end;
    _ -> Left("Bad flags")
end.

source({_,{S,_}}) -> S.

flagsImpl(R) -> error("no flags").

% exports["flags'"] = function (r) {
%   return {
%     multiline: r.multiline,
%     ignoreCase: r.ignoreCase,
%     global: r.global,
%     sticky: !!r.sticky,
%     unicode: !!r.unicode
%   };
% };

test({R,_},S) -> case re:run(S,R) of
  {match, _} -> true;
  _ -> false
end.

'_match'(Just,Nothing,{R,_},S) -> case re:run(S,R,[{capture,all,binary}]) of
  {match,Res} -> Just(array:from_list(lists:map(Just, Res)));
  _ -> Nothing
end.

replace({R,{_,F}},S1,S2) ->
  G = case is_global(F) of true -> [global]; false -> [] end,
  re:replace(S2,R,S1,G++[{return,binary}]).
% TODO
'_replaceBy'(_Just,_Nothing,{_R,_},_F,_S2) -> error("_replaceBy not supported").

'_search'(Just,Nothing,{R,_},S) -> case re:run(S,R) of
  {match, [{I,_}]} -> Just(I);
  _ -> Nothing
end.

split({R,RS},S) ->
  Res = re:split(S,R),
  Res1 = case RS of
    "" -> lists:droplast(Res);
    _ -> Res end,
  array:from_list(Res1).
