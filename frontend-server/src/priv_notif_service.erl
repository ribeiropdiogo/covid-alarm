-module(priv_notif_service).
-export([start/1]).

%%====================================================================
%% Start
%%====================================================================

start(Port) -> spawn(fun() -> init(Port) end).


init(Port) ->
    ok.
