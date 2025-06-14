%% @doc REST Resource Handler (Updated to use generated protobuf functions)
%% HTTP handler for REST resources with automatic protobuf support
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

%% @private Helper to use module's protobuf functions if available, fallback to inline
safe_parse_body(Module, Req) ->
    case erlang:function_exported(Module, parse_body, 1) of
        true -> Module:parse_body(Req);
        false -> fallback_parse_body(Req)  %% Use original implementation
    end.

%% @private Helper to use module's reply function if available, fallback to inline
safe_reply_data(Module, Code, Data, Req) ->
    case erlang:function_exported(Module, reply_data, 3) of
        true -> Module:reply_data(Code, Data, Req);
        false -> fallback_reply_data(Code, Data, Req, Module)  %% Use original implementation
    end.

%% @private Original parse_body fallback
fallback_parse_body(Req) ->
    CT = cowboy_req:header(<<"content-type">>, Req, <<"application/json">>),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case CT of <<"application/x-protobuf">> -> {pb, Body, Req1}; _ -> {json, jsone:decode(Body, [return_maps]), Req1} end.

%% @private Original reply_data fallback
fallback_reply_data(Code, Data, Req, Module) ->
    Accept = cowboy_req:header(<<"accept">>, Req, <<"application/json">>),
    case Accept of
        <<"application/x-protobuf">> ->
            try
                {ok, PbMod} = get_protobuf_module(Module),
                PbData = case is_list(Data) of
                             true-> [PbMod:encode_msg(Rec, element(1, Rec)) || Rec <- Data];
                             _-> PbMod:encode_msg(Data, element(1, Data)) end,
                cowboy_req:reply(Code, #{<<"content-type">> => <<"application/x-protobuf">>}, PbData, Req)
            catch _:R:S ->
                    Json = rest_json:to_json(Data), cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, jsone:encode(Json), Req) end;
        _ ->
            Json = rest_json:to_json(Data),
            cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, jsone:encode(Json), Req)
    end.

%% @private
%% Handle different HTTP methods
handle_request(<<"GET">>, Module, undefined, Req) ->
    case Module:get() of
        Data when is_list(Data) ->
            safe_reply_data(Module, 200, Data, Req);
        {error, Reason} ->
            cowboy_req:reply(404, #{},
                           <<"Error: ", (rest_json:to_binary(Reason))/binary>>, Req)
    end;

handle_request(<<"GET">>, Module, Id, Req) ->
    case Module:get(Id) of
        {ok, Data} ->
            safe_reply_data(Module, 200, Data, Req);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"Resource not found">>, Req);
        {error, Reason} ->
            cowboy_req:reply(400, #{},
                           <<"Error: ", (rest_json:to_binary(Reason))/binary>>, Req)
    end;

handle_request(<<"POST">>, Module, undefined, Req0) ->
    try
        {Format, JsonData, Req1} = safe_parse_body(Module, Req0),
        Record = case Format of
                     pb ->
                         case erlang:function_exported(Module, decode_protobuf, 1) of
                             true -> maps:to_list(Module:decode_protobuf(JsonData));
                             false ->
                                 {ok, PbMod} = get_protobuf_module(Module),
                                 {ok, PbRec} = get_protobuf_record(Module),
                                 PbMod:decode_msg(JsonData, PbRec)
                         end;
                     json ->
                         Props = maps:to_list(JsonData),
                         Module:from_json(Props, Module:new())
                 end,
        case Module:post(Record) of
            ok ->
                io:format("log ~p ~p: ~p ~n", [?MODULE, ?LINE, Record]),
                safe_reply_data(Module, 201, Record, Req1);
            {error, Reason} ->
                cowboy_req:reply(400, #{}, <<"Error: ", (rest_json:to_binary(Reason))/binary>>, Req1)
        end
    catch
        error:badarg ->
            cowboy_req:reply(400, #{}, <<"Invalid data format">>, Req0)
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

%% protobuf functions
get_protobuf_module(RestModule) ->
    try
        Attrs = RestModule:module_info(attributes),
        [Module] = proplists:get_value(rest_protobuf_module, Attrs),
        {ok, Module}
    catch _:_:_->
            {error, pb_module_not_found}
    end.

get_protobuf_record(RestModule) ->
    try
        Attrs = RestModule:module_info(attributes),
        [RecordName] = proplists:get_value(rest_protobuf_record, Attrs),
        {ok, RecordName}
    catch _:_:_->
            {error, rest_protobuf_record_not_found}
    end.
