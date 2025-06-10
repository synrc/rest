%% @doc REST KVS Handler
%% HTTP handler for KVS operations
-module(rest_kvs_handler).

-export([init/2]).

%% @doc Cowboy HTTP handler init
init(Req0, State) ->
    Method = cowboy_req:method(Req0),

    try
        Req1 = handle_kvs_request(Method, Req0),
        {ok, Req1, State}
    catch
        error:undef ->
            Req2 = cowboy_req:reply(503, #{}, <<"KVS not available">>, Req0),
            {ok, Req2, State};
        error:Reason ->
            error_logger:error_msg("KVS handler error: ~p~n", [Reason]),
            Req3 = cowboy_req:reply(500, #{}, <<"Internal server error">>, Req0),
            {ok, Req3, State}
    end.

%% @private
%% Handle KVS requests
handle_kvs_request(<<"GET">>, Req) ->
    %% This is a placeholder for KVS operations
    %% In a real implementation, you would extract the path info
    %% and perform the appropriate KVS operations
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                   <<"{\"status\": \"KVS endpoint\"}">>, Req);

handle_kvs_request(Method, Req) ->
    cowboy_req:reply(405, #{<<"allow">> => <<"GET">>},
                   <<"Method ", Method/binary, " not allowed">>, Req).
