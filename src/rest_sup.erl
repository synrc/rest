%% @doc REST Framework Supervisor
%% Top-level supervisor for the REST framework
-module(rest_sup).

-author('Dmitry Bushmelev').
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8005).

%% @doc Start the supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Supervisor init callback
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Initialize KVS if available
    try_kvs_join(),

    %% Start HTTP server if enabled
    maybe_start_http_server(),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    %% No child processes for now - HTTP server is started directly
    %% In a production system, you might want to supervise the HTTP server
    ChildSpecs = [],

    {ok, {SupFlags, ChildSpecs}}.

%% @private
%% Try to join KVS if available
try_kvs_join() ->
    try
        kvs:join()
    catch
        error:undef ->
            %% KVS not available, continue without it
            ok;
        Error:Reason ->
            error_logger:warning_msg("Failed to join KVS: ~p:~p~n", [Error, Reason])
    end.

%% @private
%% Start HTTP server if configured
maybe_start_http_server() ->
    case application:get_env(rest, start_http, true) of
        true ->
            Port = application:get_env(rest, port, ?DEFAULT_PORT),
            start_http_server(Port);
        false ->
            ok
    end.

%% @private
%% Start the HTTP server
start_http_server(Port) ->
    try
        Dispatch = rest_router:get_routes(),
        cowboy:start_clear(
            rest_http_listener,
            [{port, Port}],
            #{env => #{dispatch => Dispatch}}
        ),
        error_logger:info_msg("REST HTTP server started on port ~p~n", [Port])
    catch
        Error:Reason ->
            error_logger:error_msg("Failed to start HTTP server: ~p:~p~n", [Error, Reason])
    end.
