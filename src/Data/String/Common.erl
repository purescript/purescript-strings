-module(data_string_common@foreign).
-export(['_localeCompare'/5, replace/3, replaceAll/3, split/2, toLower/1, toUpper/1, trim/1, joinWith/2]).

'_localeCompare'(_Lt,_Eq,_Gt,_S1,_S2) -> error("not implemented").
replace(S1,S2,S3) -> string:replace(S3, S1, S2).
replaceAll(S1,S2,S3) -> string:replace(S3, S1, S2, all).

split(Sep,S) ->
  Split = string:split(S, Sep, all),
  Res = case {Sep,S} of
    {_,<<>>} -> lists:droplast(Split);
    _ -> Split
  end,
  array:from_list(Res).

% TODO ugh
toLower(S) -> unicode:characters_to_binary(string:to_lower(unicode:characters_to_list(S))).

toUpper(S) -> unicode:characters_to_binary(string:to_upper(unicode:characters_to_list(S))).

trim(S) -> re:replace(S, "^\\s*(.*?)\\s*$","\\1", [{return, binary}]).

joinWith(S, XS) ->
  XS1 = lists:map(fun unicode:characters_to_list/1, array:to_list(XS)),
  Res = string:join(XS1, unicode:characters_to_list(S)),
  unicode:characters_to_binary(Res).
