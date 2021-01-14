-module(util).
-export([parse/1]).

%%====================================================================
%% parse arguments
%%====================================================================

parse(Args) -> parse(Args, [], "").


parse("", List, Elem) ->
    List ++ [Elem];

parse([$\n|Args], List, Elem) ->
    parse(Args, List, Elem);

parse([$\ |Args], List, Elem) ->
    parse(Args, List ++ [Elem], "");

parse([Char|Args], List, Elem) ->
    parse(Args, List, Elem ++ [Char]).
