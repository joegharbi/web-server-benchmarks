-module(simple_cowboy_app).
-behaviour(application).

-export([start/2, stop/1, init/2]).

start(_StartType, _StartArgs) ->
    case cowboy:start_clear(http_listener, [{port, 80}, {ip, {0, 0, 0, 0}}], #{
        env => #{dispatch => cowboy_router:compile([
            {"_", [
                {"/", cowboy_static, #{
                    content => fun(Req) ->
                        cowboy_req:reply(200, #{"content-type" => "text/plain"}, "Hello, World!", Req)
                    end
                }}
            ]}
        ])}
    }) of
        {ok, _} ->
            io:format("Cowboy listener started on port 80~n"),
            {ok, self()};
        {error, {already_started, _}} ->
            io:format("Cowboy listener already started on port 80~n"),
            {ok, self()};
        {error, Reason} ->
            io:format("Failed to start Cowboy listener: ~p~n", [Reason]),
            {stop, Reason}
    end.

stop(_State) ->
    ok.

init(Req, State) ->
    %% Serve the index.html file using Cowboy static handler
    {ok, Req, State} = cowboy_static:serve(#{
        directory => {priv_dir, simple_cowboy_app},
        file => "index.html"
    }, Req),
    {ok, Req, State}.