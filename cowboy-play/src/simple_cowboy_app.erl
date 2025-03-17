-module(simple_cowboy_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", hello_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener,
        % [{port, 8080}],
        [{port, application:get_env(simple_cowboy_app, port)}],
        #{env => #{dispatch => Dispatch}}
    ),
    simple_cowboy_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http_listener),
    ok.