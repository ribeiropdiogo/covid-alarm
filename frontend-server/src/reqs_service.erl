-module(reqs_service).
-export([start/1]).

%%
%% Districts: #{ DistrictNumber: integer => Socket: socket req  }
%%

%%====================================================================
%% Start
%%====================================================================

start(Port) -> spawn(fun() -> init(Port) end).


init(Port) ->
    % connect to district servers
    {ok, Districts} = connect_districts(),
    io:format("[REQ/REP] Connected to the district servers...~n"),
    % open port
    {ok, ServSock} = gen_tcp:listen(Port, [list, {active, once}, {packet, line}, {reuseaddr, true}]),
    io:format("[REQ/REP] Listening on port ~b...~n", [Port]),
    % start accepting connections (on the main process)
    acceptor(ServSock, Districts).

%%====================================================================
%% Connect to the districts
%%====================================================================

connect_districts() -> connect_districts(1, #{}).


connect_districts(DistrictNum, Map) ->
    if
        DistrictNum > 18 ->
            {ok, Map};
        true ->
            {ok, Socket} = chumak:socket(req),
            Port = 7000 + DistrictNum*10 + 1,
            {ok, _} = chumak:connect(Socket, tcp, "localhost", Port),
            NewMap = maps:put(DistrictNum, Socket, Map),
            connect_districts(DistrictNum+1, NewMap)
    end.

%%====================================================================
%% State
%%====================================================================

acceptor(ServSock, Districts) ->
    {ok, CliSock} = gen_tcp:accept(ServSock),
    io:format("[~p] Connection accepted~n", [CliSock]),
    Pid = spawn(fun() -> authenticator(CliSock, Districts) end),
    gen_tcp:controlling_process(CliSock, Pid),
    acceptor(ServSock, Districts).


authenticator(CliSock, Districts) ->
    receive
        % create account
        {tcp, _, "ca " ++ Args} ->
            % parse the arguments
            [Username, Password, DistNumStr, LocX, LocY] = util:parse(Args),
            % get district number
            DistNum = list_to_integer(DistNumStr),
            % get district's socket
            DistSock = maps:get(DistNum, Districts),
            % create new user at the district server
            chumak:send(DistSock, "nu " ++ LocX ++ " " ++ LocY),
            % receive the user's id
            {ok, IDBin} = chumak:recv(DistSock),
            ID = binary_to_list(IDBin),
            inet:setopts(CliSock, [{active, once}]),
            % create local account
            case login_manager:create_account(Username, ID, Password, DistNum) of
                ok ->
                    io:format("[~s :: ~s] Created an account~n", [Username, ID]),
                    gen_tcp:send(CliSock, "ok " ++ DistNumStr ++ " " ++ ID ++ "\n"),
                    user({Username, ID}, CliSock, DistSock, Districts);
                user_exists ->
                    gen_tcp:send(CliSock, "error user_exists\n"),
                    authenticator(CliSock, Districts)
            end;
        % login
        {tcp, _, "li " ++ Args} ->
            % parse the arguments
            [Username, Password] = util:parse(Args),
            inet:setopts(CliSock, [{active, once}]),
            case login_manager:login(Username, Password) of
                {ok, DistNum, ID} ->
                    io:format("[~s :: ~s] Logged in~n", [Username, ID]),
                    % get district's socket
                    DistSock = maps:get(DistNum, Districts),
                    gen_tcp:send(CliSock, "ok " ++ integer_to_list(DistNum) ++ " " ++ ID ++ "\n"),
                    user({Username, ID}, CliSock, DistSock, Districts);
                already_logged_in ->
                    gen_tcp:send(CliSock, "error already_logged_in\n"),
                    authenticator(CliSock, Districts);
                invalid ->
                    gen_tcp:send(CliSock, "error invalid\n"),
                    authenticator(CliSock, Districts)
            end;
        % other
        {tcp, _, Req} ->
            io:format("[~p] Error: invalid request (~p)~n", [CliSock, Req]),
            inet:setopts(CliSock, [{active, once}]),
            gen_tcp:send(CliSock, "error invalid_request\n"),
            authenticator(CliSock, Districts);
        {tcp_closed, _} ->
            io:format("[~p] Disconnected~n", [CliSock]);
        {tcp_error, _, Reason} ->
            io:format("[~p] TCP error: ~p~n", [CliSock, Reason])
    end.


user({Name, ID} = User, CliSock, DistSock, Districts) ->
    receive
        {tcp, _, "lo\n"} ->
            login_manager:logout(Name),
            inet:setopts(CliSock, [{active, once}]),
            gen_tcp:send(CliSock, "ok\n"),
            io:format("[~s :: ~s] Logged out~n", [Name, ID]),
            authenticator(CliSock, Districts);
        {tcp, _, "ul " ++ Args} ->
            chumak:send(DistSock, "ul " ++ ID ++ " " ++ string:trim(Args)),
            {ok, Response} = chumak:recv(DistSock),
            inet:setopts(CliSock, [{active, once}]),
            gen_tcp:send(CliSock, binary_to_list(Response) ++ "\n"),
            user(User, CliSock, DistSock, Districts);
        {tcp, _, ("us " ++ _) = Data} ->
            chumak:send(DistSock, string:trim(Data)),
            {ok, Response} = chumak:recv(DistSock),
            inet:setopts(CliSock, [{active, once}]),
            gen_tcp:send(CliSock, binary_to_list(Response) ++ "\n"),
            user(User, CliSock, DistSock, Districts);
        {tcp, _, "ai\n"} ->
            chumak:send(DistSock, "ai " ++ ID),
            {ok, Response} = chumak:recv(DistSock),
            login_manager:infected(Name),
            inet:setopts(CliSock, [{active, once}]),
            gen_tcp:send(CliSock, binary_to_list(Response) ++ "\n"),
            io:format("[~s :: ~s] Infected, Logged out~n", [Name, ID]),
            authenticator(CliSock, Districts);
        {tcp, _, _} ->
            inet:setopts(CliSock, [{active, once}]),
            gen_tcp:send(CliSock, "error invalid_request\n"),
            user(User, CliSock, DistSock, Districts);
        {tcp_closed, _} ->
            login_manager:logout(Name),
            io:format("[~s :: ~s] Disconnected~n", [Name, ID]);
        {tcp_error, _, Reason} ->
            login_manager:logout(Name),
            io:format("[~s :: ~s] TCP error: ~p~n", [Name, ID, Reason])
    end.
