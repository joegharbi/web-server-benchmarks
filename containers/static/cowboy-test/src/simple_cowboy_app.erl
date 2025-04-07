-module(simple_cowboy_app).
-behaviour(application).

-export([start/2, stop/1, handle/2]).

start(_StartType, _StartArgs) ->
    io:format("Starting Cowboy application~n"),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {file, "/app/index.html"}}
        ]}
    ]),
    io:format("Attempting to start Cowboy listener~n"),
    {ok, Pid} = cowboy:start_clear(http_listener,
        #{port => 80,
          ip => {0,0,0,0},
          num_acceptors => 8,
          max_connections => infinity},
        #{env => #{dispatch => Dispatch},
          middlewares => [cowboy_router, ?MODULE, cowboy_handler]}
    ),
    io:format("Cowboy listener started successfully with pid ~p~n", [Pid]),
    simple_cowboy_sup:start_link().

stop(_State) ->
    io:format("Stopping Cowboy application~n"),
    cowboy:stop_listener(http_listener),
    ok.

handle(Req, State) ->
    io:format("Handling request for ~p~n", [cowboy_req:path(Req)]),
    {ok, Req, State}.