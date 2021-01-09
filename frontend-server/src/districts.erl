-module(districts).
-export([connect/0]).


connect() -> connect(1, #{}).

connect(DistrictNum, Map) ->
    if
        DistrictNum > 8 ->
            {ok, Map};
        true ->
            {ok, Socket} = chumak:socket(req),
            Port = 7000 + DistrictNum*10 + 1,
            case chumak:connect(Socket, tcp, "localhost", Port) of
                {ok, _} ->
                    NewMap = maps:put(DistrictNum, Socket, Map),
                    connect(DistrictNum+1, NewMap);
                {error, Reason} ->
                    {error, unicode:characters_to_list(["Connection failed: ", Reason], utf8)};
                Reply ->
                    {error, unicode:characters_to_list(["Unhandled reply: ", Reply], utf8)}
            end
    end.
