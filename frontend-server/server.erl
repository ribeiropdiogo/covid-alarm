-module(server).
-export([start_server/1]).


start_server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, once}, {packet, line}, {reuseaddr, true}]),
    % connect to district servers here
    spawn(fun() -> acceptor(LSock) end),
    ok.

acceptor(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock) end),
    % authenticate here
    user(Sock).

user(Sock) ->
    receive
        % from district server:
        % ...
        % from user:
        {tcp, _, Data} ->
            io:format("received ~p~n", [Data]),
            inet:setopts(Sock, [{active, once}]),
            gen_tcp:send(Sock, Data),
            user(Sock);
        {tcp_closed, _} ->
            io:format("user disconnected~n");
        {tcp_error, _, _} ->
            io:format("tcp error~n")
    end.
