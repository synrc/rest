-module(rest_app).
-behaviour(application).
-behaviour(supervisor).
-export([init/1,start/2, stop/1]).

init([]) ->
   users:init(),
   cowboy:start_clear(http, [{port,8005}], #{env=>#{dispatch=> points() }}),
   {ok, {{one_for_one, 5, 10}, []}}.
stop(_State) -> ok.
start(_StartType, _StartArgs) ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

points() -> cowboy_router:compile([{'_', [
             {"/rest/:resource",     rest_cowboy, []},
             {"/rest/:resource/:id", rest_cowboy, []}
            ]}]).