-module(cowboy_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {inline, "text/html", "<h1>Hello, Cowboy!</h1>"}},
            {"/[...]", cowboy_static, {priv_dir, ?MODULE, "."}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("Cowboy server running on port 8080~n"),
    {ok, self()}.

stop(_State) ->
    ok.