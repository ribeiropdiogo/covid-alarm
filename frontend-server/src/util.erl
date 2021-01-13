-module(util).
-export([connect_districts/0, parse/1]).

%%====================================================================
%% connect to the districts
%%====================================================================

connect_districts() -> connect_districts(1, #{}).


connect_districts(DistrictNum, Map) ->
    if
        DistrictNum > 18 ->
            {ok, Map};
        true ->
            {ok, Socket} = chumak:socket(req),
            Port = 7000 + DistrictNum*10 + 1,
            case chumak:connect(Socket, tcp, "localhost", Port) of
                {ok, _} ->
                    NewMap = maps:put(DistrictNum, Socket, Map),
                    connect_districts(DistrictNum+1, NewMap);
                {error, Reason} ->
                    {error, unicode:characters_to_list(["Connection failed: ", Reason], utf8)};
                Reply ->
                    {error, unicode:characters_to_list(["Unhandled reply: ", Reply], utf8)}
            end
    end.

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
