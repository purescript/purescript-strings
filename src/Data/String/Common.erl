-module(data_string_common@foreign).
-export(['_localeCompare'/5, replace/3, replaceAll/3, split/2, toLower/1, toUpper/1, trim/1, joinWith/2]).

'_localeCompare'(_Lt,_Eq,_Gt,_S1,_S2) -> error("not implemented").

replace(S1,S2,S3) -> unicode:characters_to_binary(string:replace(S3, S1, S2)).

replaceAll(S1,S2,S3) -> unicode:characters_to_binary(string:replace(S3, S1, S2, all)).

split(Sep,S) ->
  Res = case {Sep,S} of
    {<<>>,<<>>} -> []; %% string:split says [<<>>] but JS says []
    {<<>>,_} -> 
      %% string:split does not work on empty split pattern
      lists:map(fun (C) -> unicode:characters_to_binary([C], utf8) end, unicode:characters_to_list(S, utf8));
     
    %% {_,<<>>} behaves the same in JS vs string:split
    _ -> string:split(S, Sep, all)
  end,
  array:from_list(Res).

toLower(S) -> string:lowercase(S).

toUpper(S) -> string:uppercase(S).

trim(S) -> string:trim(S).

joinWith(S, XS) ->
  unicode:characters_to_binary(lists:join(S, array:to_list(XS))).
