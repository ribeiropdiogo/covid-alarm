-module(frontend_server).
-export([main/1]).

%%
%% Districts: #{ DistrictNumber: integer => Socket: socket req  }
%%

%%====================================================================
%% Start
%%====================================================================

main(_) ->
    Port = 8000,
    % start login manager
    login_manager:start(),
    % connect to district servers
    case districts:connect() of
        {ok, Districts} ->
            % open port
            {ok, ServSock} = gen_tcp:listen(Port, [list, {active, once}, {packet, line}, {reuseaddr, true}]),
            io:format("Listening on port ~b...~n", [Port]),
            % start accepting connections (on the main process)
            acceptor(ServSock, Districts);
        {error, Msg} ->
            io:format("Error connecting to the district servers~n"),
            io:format("  ~s~n", [Msg])
    end.

%%====================================================================
%% Internal functions
%%====================================================================

acceptor(ServSock, Districts) ->
    {ok, CliSock} = gen_tcp:accept(ServSock),
    Pid = spawn(fun() -> authenticate(CliSock, Districts) end),
    gen_tcp:controlling_process(CliSock, Pid),
    % keep accepting connections on the main process
    acceptor(ServSock, Districts).


authenticate(CliSock, Districts) ->
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
                    gen_tcp:send(CliSock, "ok\n"),
                    io:format("user {~s, ~s} :: created an account~n", [Username, ID]),
                    user({Username, ID}, CliSock, DistSock);
                user_exists ->
                    gen_tcp:send(CliSock, "error user_exists\n"),
                    authenticate(CliSock, Districts)
            end;
        % login
        {tcp, _, "li " ++ Args} ->
            % parse the arguments
            [Username, Password] = util:parse(Args),
            inet:setopts(CliSock, [{active, once}]),
            case login_manager:login(Username, Password) of
                {ok, ID, DistNum} ->
                    % get district's socket
                    DistSock = maps:get(DistNum, Districts),
                    gen_tcp:send(CliSock, "ok\n"),
                    io:format("user {~s, ~s} :: logged in~n", [Username, ID]),
                    user({Username, ID}, CliSock, DistSock);
                invalid ->
                    gen_tcp:send(CliSock, "error invalid\n"),
                    authenticate(CliSock, Districts)
            end;
        % other
        {tcp, _, _} ->
            inet:setopts(CliSock, [{active, once}]),
            gen_tcp:send(CliSock, "error invalid_request\n"),
            authenticate(CliSock, Districts);
        {tcp_closed, _} ->
            io:format("user disconnected~n");
        {tcp_error, _, _} ->
            io:format("tcp error~n")
    end.


user({Name, ID} = User, CliSock, DistSock) ->
    receive
        {tcp, _, "lo\n"} ->
            login_manager:logout(Name),
            gen_tcp:send(CliSock, "ok\n"),
            io:format("user {~s, ~s} :: logged out~n", [Name, ID]);
        {tcp, _, "ul " ++ Args} ->
            chumak:send(DistSock, "ul " ++ ID ++ " " ++ string:trim(Args)),
            {ok, Response} = chumak:recv(DistSock),
            inet:setopts(CliSock, [{active, once}]),
            gen_tcp:send(CliSock, binary_to_list(Response) ++ "\n"),
            user(User, CliSock, DistSock);
        {tcp, _, ("us " ++ _) = Data} ->
            chumak:send(DistSock, string:trim(Data)),
            {ok, Response} = chumak:recv(DistSock),
            inet:setopts(CliSock, [{active, once}]),
            gen_tcp:send(CliSock, binary_to_list(Response) ++ "\n"),
            user(User, CliSock, DistSock);
        {tcp, _, "ai\n"} ->
            chumak:send(DistSock, "ai " ++ ID),
            {ok, Response} = chumak:recv(DistSock),
            inet:setopts(CliSock, [{active, once}]),
            gen_tcp:send(CliSock, binary_to_list(Response) ++ "\n"),
            user(User, CliSock, DistSock);
        {tcp, _, _} ->
            inet:setopts(CliSock, [{active, once}]),
            gen_tcp:send(CliSock, "error invalid_request\n"),
            user(User, CliSock, DistSock);
        {tcp_closed, _} ->
            login_manager:logout(Name),
            io:format("user {~s, ~s} :: disconnected~n", [Name, ID]);
        {tcp_error, _, _} ->
            login_manager:logout(Name),
            io:format("user {~s, ~s} :: tcp error~n", [Name, ID])
    end.
