-module(simple_cowboy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(PORT, 80).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, #{priv_dir => simple_cowboy_app,
                                      file => "index.html"}}
        ]}
    ]),
    ListenerSpec = #{id => http_listener,
                    start => {cowboy, start_clear, [http_listener, [{port, ?PORT}], #{env => #{dispatch => Dispatch}}]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker},
    ChildSpecs = [ListenerSpec],
    {ok, {SupFlags, ChildSpecs}}.