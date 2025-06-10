%% @doc REST Resource Handler
%% HTTP handler for REST resources
-module(rest_resource_handler).

-export([init/2]).

%% @doc Cowboy HTTP handler init
init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Resource = cowboy_req:binding(resource, Req0),
    Version = cowboy_req:binding(version, Req0),
    Id = cowboy_req:binding(id, Req0),

    %% Handle default version from options
    ActualVersion = case Version of
        undefined ->
            proplists:get_value(default_version, Opts, v1);
        V -> V
    end,

    try
        case rest_router:resolve_module(ActualVersion, Resource) of
            {ok, Module} ->
                Req1 = handle_request(Method, Module, Id, Req0),
                {ok, Req1, Opts};
            {error, not_found} ->
                ErrorMsg = <<"Resource not found: ", Resource/binary,
                           " (version: ", (rest_json:to_binary(ActualVersion))/binary, ")">>,
                Req1 = cowboy_req:reply(404, #{}, ErrorMsg, Req0),
                {ok, Req1, Opts};
            {error, version_not_found} ->
                ErrorMsg = <<"API version not supported: ", (rest_json:to_binary(ActualVersion))/binary>>,
                Req1 = cowboy_req:reply(404, #{}, ErrorMsg, Req0),
                {ok, Req1, Opts};
            {error, module_not_loaded} ->
                ErrorMsg = <<"Resource module not loaded: ", Resource/binary>>,
                Req1 = cowboy_req:reply(503, #{}, ErrorMsg, Req0),
                {ok, Req1, Opts}
        end
    catch
        error:Reason:Stacktrace ->
            error_logger:error_msg("REST handler error: ~p, ~p~n", [Reason, Stacktrace]),
            Req2 = cowboy_req:reply(500, #{}, <<"Internal server error">>, Req0),
            {ok, Req2, Opts}
    end.

%% @private
%% Handle different HTTP methods
handle_request(<<"GET">>, Module, undefined, Req) ->
    case Module:get() of
        Data when is_list(Data) ->
            Json = rest_json:to_json(Data),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                           jsone:encode(Json), Req);
        {error, Reason} ->
            cowboy_req:reply(404, #{},
                           <<"Error: ", (rest_json:to_binary(Reason))/binary>>, Req)
    end;

handle_request(<<"GET">>, Module, Id, Req) ->
    case Module:get(Id) of
        {ok, Data} ->
            Json = rest_json:to_json(Data),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                           jsone:encode(Json), Req);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"Resource not found">>, Req);
        {error, Reason} ->
            cowboy_req:reply(400, #{},
                           <<"Error: ", (rest_json:to_binary(Reason))/binary>>, Req)
    end;

handle_request(<<"POST">>, Module, undefined, Req0) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),

    try
        JsonData = jsone:decode(Body, [return_maps]),
        Props = maps:to_list(JsonData),
        Record = Module:from_json(Props, Module:new()),

        case Module:post(Record) of
            {ok, CreatedRecord} ->
                Json = rest_json:to_json(CreatedRecord),
                cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>},
                               jsone:encode(Json), Req1);
            {error, Reason} ->
                cowboy_req:reply(400, #{},
                               <<"Error: ", (rest_json:to_binary(Reason))/binary>>, Req1)
        end
    catch
        error:badarg ->
            cowboy_req:reply(400, #{}, <<"Invalid JSON">>, Req1)
    end;

handle_request(<<"DELETE">>, Module, Id, Req) ->
    case Module:delete(Id) of
        ok ->
            cowboy_req:reply(204, #{}, <<>>, Req);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"Resource not found">>, Req);
        {error, Reason} ->
            cowboy_req:reply(400, #{},
                           <<"Error: ", (rest_json:to_binary(Reason))/binary>>, Req)
    end;

handle_request(Method, _Module, _Id, Req) ->
    cowboy_req:reply(405, #{<<"allow">> => <<"GET, POST, DELETE">>},
                   <<"Method ", Method/binary, " not allowed">>, Req).
