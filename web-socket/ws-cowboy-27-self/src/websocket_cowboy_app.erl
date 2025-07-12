-module(websocket_cowboy_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    websocket_cowboy_sup:start_link().

stop(_State) ->
    ok. 