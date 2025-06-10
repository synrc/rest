%% @doc REST Framework Application
%% Main application module for the REST framework
-module(rest_app).

-author('Erwin Flaming').
-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_PORT, 8005).

%% @doc Application start callback
-spec start(StartType :: application:start_type(), StartArgs :: term()) ->
    {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    rest_sup:start_link().

%% @doc Application stop callback
-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.
