-module(websocket_cowboy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10}, [
        {cowboy, {cowboy, start_clear, [http, [{port, 8080}], #{env => #{dispatch => dispatch()}}]}}
    ]}}.

dispatch() ->
    cowboy_router:compile([
        {'_', [
            {"/", static_handler, []},
            {"/ws", websocket_handler, []}
        ]}
    ]). 