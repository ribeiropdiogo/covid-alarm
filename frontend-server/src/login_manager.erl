-module(login_manager).
-export([start/0, create_account/4, login/2, infected/1, logout/1]).

%
% State: #{ Username: string => {ID: string, Password: string, Distrito: integer, Online: boolean, Infected: boolean} }
%

start() -> register(?MODULE, spawn(fun() -> loop(#{}) end)).


loop(Accounts) ->
    receive
        {{create_account, Username, ID, Password, DistNum}, From} ->
            case maps:find(Username, Accounts) of
                error ->
                    From ! {ok, ?MODULE},
                    loop(maps:put(Username, {ID, Password, DistNum, false, false}, Accounts));
                _ ->
                    From ! {user_exists, ?MODULE},
                    loop(Accounts)
            end;
        {{login, Username, Password}, From} ->
            case maps:find(Username, Accounts) of
                {ok, {ID, Password, DistNum, false, false}} ->
                    From ! {{ok, DistNum, ID}, ?MODULE},
                    loop(maps:update(Username, {ID, Password, DistNum, true, false}, Accounts));
                {ok, {_, Password, _, true, _}} ->
                    From ! {already_logged_in, ?MODULE},
                    loop(Accounts);
                _ ->
                    From ! {invalid, ?MODULE},
                    loop(Accounts)
            end;
        {{infected, Username}, From} ->
            case maps:find(Username, Accounts) of
                {ok, {ID, Password, DistNum, _, _}} ->
                    From ! {ok, ?MODULE},
                    loop(maps:update(Username, {ID, Password, DistNum, false, true}, Accounts));
                _ ->
                    From ! {ok, ?MODULE},
                    loop(Accounts)
            end;
        {{logout, Username}, From} ->
            case maps:find(Username, Accounts) of
                {ok, {ID, Password, DistNum, true, Infected}} ->
                    From ! {ok, ?MODULE},
                    loop(maps:update(Username, {ID, Password, DistNum, false, Infected}, Accounts));
                _ ->
                    From ! {ok, ?MODULE},
                    loop(Accounts)
            end
    end.


% create_account(Username, Password) -> ok | user_exists
create_account(Username, ID, Password, DistNum) -> rpc({create_account, Username, ID, Password, DistNum}).

% login(Username, Password) -> ok | already_logged_in | invalid
login(Username, Password) -> rpc({login, Username, Password}).

% infected(Username) -> ok
infected(Username) -> rpc({infected, Username}).

% logout(Username) -> ok
logout(Username) -> rpc({logout, Username}).


rpc(Request) ->
    ?MODULE ! {Request, self()},
    receive
        {Result, ?MODULE} -> Result
    end.
