-module(frontend_server).
-export([main/1]).

%%====================================================================
%% Start
%%====================================================================

main(_) ->
    % start login manager
    login_manager:start(),
    % start requests handler
    reqs_service:start(8001),
    % start private notifications handler
    priv_notif_service:start(8002),
    % sleep forever
    receive
        after infinity ->
            ok
    end.
