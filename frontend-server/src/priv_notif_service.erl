-module(priv_notif_service).
-export([start/1]).

%%====================================================================
%% Start
%%====================================================================

start(Port) -> spawn(fun() -> init(Port) end).


init(Port) ->
    % connect to district servers
    {ok, SubSocket} = connect_districts(),
    io:format("[PUB/SUB] Connected to the district servers...~n"),
    % open port
    {ok, PubSocket} = chumak:socket(pub),
    {ok, _} = chumak:bind(PubSocket, tcp, "localhost", Port),
    io:format("[PUB/SUB] Publishing on port ~b...~n", [Port]),
    loop(SubSocket, PubSocket).

%%====================================================================
%% Connect to the districts
%%====================================================================

connect_districts() ->
    {ok, Socket} = chumak:socket(sub),
    connect_districts(1, Socket).


connect_districts(DistrictNum, Socket) ->
    if
        DistrictNum > 18 ->
            {ok, Socket};
        true ->
            Port = 7000 + DistrictNum*10 + 3,
            Topic = string:pad(integer_to_list(DistrictNum), 2, leading, $0) ++ " ",
            chumak:subscribe(Socket, Topic),
            {ok, _} = chumak:connect(Socket, tcp, "localhost", Port),
            connect_districts(DistrictNum+1, Socket)
    end.

%%====================================================================
%% State
%%====================================================================

loop(SubSocket, PubSocket) ->
    {ok, Data} = chumak:recv(SubSocket),
    ok = chumak:send(PubSocket, Data),
    loop(SubSocket, PubSocket).
