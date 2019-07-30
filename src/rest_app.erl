-module(rest_app).
-behaviour(application).
-behaviour(supervisor).
-export([init/1,start/2, stop/1]).

init([]) -> {ok, {{one_for_one, 5, 10}, []}}.
stop(_State) -> ok.
start(_StartType, _StartArgs) ->
   cowboy:start_tls(http,n2o_cowboy:env(rest),
                 #{env=>#{dispatch=> points() }}),
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

points() -> cowboy_router:compile([{'_', [
             {"/rest/:resource",     rest_cowboy, []},
             {"/rest/:resource/:id", rest_cowboy, []}
            ]}]).