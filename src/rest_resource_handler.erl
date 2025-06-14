%% @doc REST Resource Handler with Simple Working Debugging
%% HTTP handler for REST resources with automatic protobuf support and debugging
-module(rest_resource_handler).

-export([init/2]).

%% SIMPLE WORKING MACROS - These will always work
-define(LOG_DEBUG(Msg), error_logger:info_msg("[DEBUG] " ++ Msg ++ "~n")).
-define(LOG_DEBUG(Msg, Args), error_logger:info_msg("[DEBUG] " ++ Msg ++ "~n", Args)).
-define(LOG_INFO(Msg, Args), error_logger:info_msg("[INFO] " ++ Msg ++ "~n", Args)).
-define(LOG_WARNING(Msg, Args), error_logger:warning_msg("[WARNING] " ++ Msg ++ "~n", Args)).
-define(LOG_ERROR(Msg, Args), error_logger:error_msg("[ERROR] " ++ Msg ++ "~n", Args)).

%% Enhanced debug macros with context
-define(DEBUG_REQUEST(Method, Resource, Version, Id),
    error_logger:info_msg("[DEBUG] Processing ~s request: resource=~p, version=~p, id=~p~n", [Method, Resource, Version, Id])).

-define(DEBUG_MODULE_RESOLUTION(Version, Resource, Result),
    error_logger:info_msg("[DEBUG] Module resolution: ~p/~p -> ~p~n", [Version, Resource, Result])).

-define(DEBUG_PROTOBUF(Action, Module, Result),
    error_logger:info_msg("[DEBUG] Protobuf ~s: module=~p, result=~p~n", [Action, Module, Result])).

-define(DEBUG_CONTENT_TYPE(Type, Accept),
    error_logger:info_msg("[DEBUG] Content negotiation: content-type=~p, accept=~p~n", [Type, Accept])).

-define(DEBUG_DATA_FLOW(Stage, Data),
    error_logger:info_msg("[DEBUG] Data flow [~s]: ~p~n", [Stage, debug_truncate(Data)])).

%% @doc Cowboy HTTP handler init with debugging
init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Resource = cowboy_req:binding(resource, Req0),
    Version = cowboy_req:binding(version, Req0),
    Id = cowboy_req:binding(id, Req0),

    ?DEBUG_REQUEST(Method, Resource, Version, Id),

    %% Handle default version from options
    ActualVersion = case Version of
        undefined ->
            DefaultVersion = proplists:get_value(default_version, Opts, v1),
            ?LOG_DEBUG("Using default version: ~p", [DefaultVersion]),
            DefaultVersion;
        V ->
            ?LOG_DEBUG("Using specified version: ~p", [V]),
            V
    end,

    try
        case rest_router:resolve_module(ActualVersion, Resource) of
            {ok, Module} ->
                ?DEBUG_MODULE_RESOLUTION(ActualVersion, Resource, {ok, Module}),
                Req1 = handle_request(Method, Module, Id, Req0),
                {ok, Req1, Opts};
            {error, not_found} = Error ->
                ?DEBUG_MODULE_RESOLUTION(ActualVersion, Resource, Error),
                ErrorMsg = <<"Resource not found: ", Resource/binary,
                           " (version: ", (rest_json:to_binary(ActualVersion))/binary, ")">>,
                ?LOG_WARNING("Resource not found: ~p/~p", [ActualVersion, Resource]),
                Req1 = cowboy_req:reply(404, #{}, ErrorMsg, Req0),
                {ok, Req1, Opts};
            {error, version_not_found} = Error ->
                ?DEBUG_MODULE_RESOLUTION(ActualVersion, Resource, Error),
                ErrorMsg = <<"API version not supported: ", (rest_json:to_binary(ActualVersion))/binary>>,
                ?LOG_WARNING("API version not supported: ~p", [ActualVersion]),
                Req1 = cowboy_req:reply(404, #{}, ErrorMsg, Req0),
                {ok, Req1, Opts};
            {error, module_not_loaded} = Error ->
                ?DEBUG_MODULE_RESOLUTION(ActualVersion, Resource, Error),
                ErrorMsg = <<"Resource module not loaded: ", Resource/binary>>,
                ?LOG_ERROR("Resource module not loaded: ~p", [Resource]),
                Req1 = cowboy_req:reply(503, #{}, ErrorMsg, Req0),
                {ok, Req1, Opts}
        end
    catch
        error:Reason:Stacktrace ->
            ?LOG_ERROR("REST handler error: ~p~nStacktrace: ~p", [Reason, Stacktrace]),
            Req2 = cowboy_req:reply(500, #{}, <<"Internal server error">>, Req0),
            {ok, Req2, Opts}
    end.

%% @private Helper to use module's protobuf functions if available, fallback to inline
safe_parse_body(Module, Req) ->
    ?LOG_DEBUG("Parsing body for module: ~p", [Module]),
    Result = case erlang:function_exported(Module, parse_body, 1) of
        true ->
            ?LOG_DEBUG("Using module-specific parse_body/1", []),
            Module:parse_body(Req);
        false ->
            ?LOG_DEBUG("Using fallback parse_body", []),
            fallback_parse_body(Req)
    end,
    ?DEBUG_DATA_FLOW("parse_body_result", Result),
    Result.

%% @private Helper to use module's reply function if available, fallback to inline
safe_reply_data(Module, Code, Data, Req) ->
    ?LOG_DEBUG("Replying data for module: ~p, code: ~p", [Module, Code]),
    ?DEBUG_DATA_FLOW("reply_data_input", Data),
    case erlang:function_exported(Module, reply_data, 3) of
        true ->
            ?LOG_DEBUG("Using module-specific reply_data/3", []),
            Module:reply_data(Code, Data, Req);
        false ->
            ?LOG_DEBUG("Using fallback reply_data", []),
            fallback_reply_data(Code, Data, Req, Module)
    end.

%% @private Original parse_body fallback with debugging
fallback_parse_body(Req) ->
    CT = cowboy_req:header(<<"content-type">>, Req, <<"application/json">>),
    ?DEBUG_CONTENT_TYPE(CT, undefined),

    {ok, Body, Req1} = cowboy_req:read_body(Req),
    ?LOG_DEBUG("Read body: ~p bytes", [byte_size(Body)]),

    case CT of
        <<"application/x-protobuf">> ->
            ?LOG_DEBUG("Parsing protobuf body", []),
            {pb, Body, Req1};
        _ ->
            ?LOG_DEBUG("Parsing JSON body", []),
            try
                JsonData = jsone:decode(Body, [return_maps]),
                ?DEBUG_DATA_FLOW("json_parsed", JsonData),
                {json, JsonData, Req1}
            catch
                Error:Reason ->
                    ?LOG_ERROR("JSON parsing failed: ~p:~p", [Error, Reason]),
                    error(badarg)
            end
    end.

%% @private Original reply_data fallback with debugging
fallback_reply_data(Code, Data, Req, Module) ->
    Accept = cowboy_req:header(<<"accept">>, Req, <<"application/json">>),
    ?DEBUG_CONTENT_TYPE(undefined, Accept),

    case Accept of
        <<"application/x-protobuf">> ->
            ?LOG_DEBUG("Client requested protobuf response", []),
            try
                {ok, PbMod} = get_protobuf_module(Module),
                ?DEBUG_PROTOBUF("encode_start", PbMod, {data_type, type_of(Data)}),

                PbData = case is_list(Data) of
                    true ->
                        ?LOG_DEBUG("Encoding list of ~p items", [length(Data)]),
                        [PbMod:encode_msg(Rec, element(1, Rec)) || Rec <- Data];
                    _ ->
                        ?LOG_DEBUG("Encoding single record", []),
                        PbMod:encode_msg(Data, element(1, Data))
                end,

                ?DEBUG_PROTOBUF("encode_success", PbMod, {size, byte_size(iolist_to_binary(PbData))}),
                cowboy_req:reply(Code, #{<<"content-type">> => <<"application/x-protobuf">>}, PbData, Req)
            catch
                ErrorType:Reason:Stacktrace ->
                    ?LOG_ERROR("Protobuf encoding failed: ~p:~p", [ErrorType, Reason]),
                    ?LOG_DEBUG("Protobuf error stacktrace: ~p", [Stacktrace]),
                    ?LOG_INFO("Falling back to JSON response", []),
                    Json = rest_json:to_json(Data),
                    cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, jsone:encode(Json), Req)
            end;
        _ ->
            ?LOG_DEBUG("Client requested JSON response (or default)", []),
            Json = rest_json:to_json(Data),
            ?DEBUG_DATA_FLOW("json_output", Json),
            cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, jsone:encode(Json), Req)
    end.

%% @private Handle different HTTP methods with debugging
handle_request(<<"GET">>, Module, undefined, Req) ->
    ?LOG_DEBUG("Handling GET request for all resources", []),
    case Module:get() of
        Data when is_list(Data) ->
            ?LOG_INFO("GET all: returning ~p items", [length(Data)]),
            ?DEBUG_DATA_FLOW("get_all_result", Data),
            safe_reply_data(Module, 200, Data, Req);
        {error, Reason} ->
            ?LOG_WARNING("GET all failed: ~p", [Reason]),
            cowboy_req:reply(404, #{},
                           <<"Error: ", (rest_json:to_binary(Reason))/binary>>, Req)
    end;

handle_request(<<"GET">>, Module, Id, Req) ->
    ?LOG_DEBUG("Handling GET request for resource ID: ~p", [Id]),
    case Module:get(Id) of
        {ok, Data} ->
            ?LOG_INFO("GET ~p: success", [Id]),
            ?DEBUG_DATA_FLOW("get_single_result", Data),
            safe_reply_data(Module, 200, Data, Req);
        {error, not_found} ->
            ?LOG_INFO("GET ~p: not found", [Id]),
            cowboy_req:reply(404, #{}, <<"Resource not found">>, Req);
        {error, Reason} ->
            ?LOG_WARNING("GET ~p failed: ~p", [Id, Reason]),
            cowboy_req:reply(400, #{},
                           <<"Error: ", (rest_json:to_binary(Reason))/binary>>, Req)
    end;

handle_request(<<"POST">>, Module, undefined, Req0) ->
    ?LOG_DEBUG("Handling POST request", []),
    try
        {Format, JsonData, Req1} = safe_parse_body(Module, Req0),
        ?LOG_DEBUG("POST body format: ~p", [Format]),

        Record = case Format of
            pb ->
                ?LOG_DEBUG("Processing protobuf POST data", []),
                case erlang:function_exported(Module, decode_protobuf, 1) of
                    true ->
                        ?LOG_DEBUG("Using module-specific decode_protobuf/1", []),
                        maps:to_list(Module:decode_protobuf(JsonData));
                    false ->
                        ?LOG_DEBUG("Using generic protobuf decoding", []),
                        {ok, PbMod} = get_protobuf_module(Module),
                        {ok, PbRec} = get_protobuf_record(Module),
                        ?DEBUG_PROTOBUF("decode", PbMod, {record_type, PbRec}),
                        PbMod:decode_msg(JsonData, PbRec)
                end;
            json ->
                ?LOG_DEBUG("Processing JSON POST data", []),
                Props = maps:to_list(JsonData),
                ?DEBUG_DATA_FLOW("json_props", Props),
                Module:from_json(Props, Module:new())
        end,

        ?DEBUG_DATA_FLOW("post_record", Record),

        case Module:post(Record) of
            ok ->
                ?LOG_INFO("POST: created successfully", []),
                ?DEBUG_DATA_FLOW("post_success", Record),
                safe_reply_data(Module, 201, Record, Req1);
            {ok, CreatedRecord} ->
                ?LOG_INFO("POST: created successfully with returned data", []),
                ?DEBUG_DATA_FLOW("post_success_with_data", CreatedRecord),
                safe_reply_data(Module, 201, CreatedRecord, Req1);
            {error, Reason} ->
                ?LOG_WARNING("POST failed: ~p", [Reason]),
                cowboy_req:reply(400, #{}, <<"Error: ", (rest_json:to_binary(Reason))/binary>>, Req1)
        end
    catch
        error:badarg:Stacktrace ->
            ?LOG_ERROR("POST: Invalid data format", []),
            ?LOG_DEBUG("POST badarg stacktrace: ~p", [Stacktrace]),
            cowboy_req:reply(400, #{}, <<"Invalid data format">>, Req0);
        ErrorType:Reason1:Stacktrace1 ->
            ?LOG_ERROR("POST: Unexpected error ~p:~p", [ErrorType, Reason1]),
            ?LOG_DEBUG("POST error stacktrace: ~p", [Stacktrace1]),
            cowboy_req:reply(500, #{}, <<"Internal server error">>, Req0)
    end;

handle_request(<<"DELETE">>, Module, Id, Req) ->
    ?LOG_DEBUG("Handling DELETE request for ID: ~p", [Id]),
    case Module:delete(Id) of
        ok ->
            ?LOG_INFO("DELETE ~p: success", [Id]),
            cowboy_req:reply(204, #{}, <<>>, Req);
        {error, not_found} ->
            ?LOG_INFO("DELETE ~p: not found", [Id]),
            cowboy_req:reply(404, #{}, <<"Resource not found">>, Req);
        {error, Reason} ->
            ?LOG_WARNING("DELETE ~p failed: ~p", [Id, Reason]),
            cowboy_req:reply(400, #{},
                           <<"Error: ", (rest_json:to_binary(Reason))/binary>>, Req)
    end;

handle_request(Method, _Module, _Id, Req) ->
    ?LOG_WARNING("Unsupported HTTP method: ~p", [Method]),
    cowboy_req:reply(405, #{<<"allow">> => <<"GET, POST, DELETE">>},
                   <<"Method ", Method/binary, " not allowed">>, Req).

%% protobuf functions with debugging
get_protobuf_module(RestModule) ->
    ?LOG_DEBUG("Getting protobuf module for: ~p", [RestModule]),
    try
        Attrs = RestModule:module_info(attributes),
        [Module] = proplists:get_value(rest_protobuf_module, Attrs),
        ?DEBUG_PROTOBUF("module_found", Module, {rest_module, RestModule}),
        {ok, Module}
    catch
        ErrorType:Reason:Stacktrace ->
            ?LOG_ERROR("Failed to get protobuf module for ~p: ~p:~p", [RestModule, ErrorType, Reason]),
            ?LOG_DEBUG("Protobuf module error stacktrace: ~p", [Stacktrace]),
            {error, pb_module_not_found}
    end.

get_protobuf_record(RestModule) ->
    ?LOG_DEBUG("Getting protobuf record for: ~p", [RestModule]),
    try
        Attrs = RestModule:module_info(attributes),
        [RecordName] = proplists:get_value(rest_protobuf_record, Attrs),
        ?DEBUG_PROTOBUF("record_found", RecordName, {rest_module, RestModule}),
        {ok, RecordName}
    catch
        ErrorType:Reason:Stacktrace ->
            ?LOG_ERROR("Failed to get protobuf record for ~p: ~p:~p", [RestModule, ErrorType, Reason]),
            ?LOG_DEBUG("Protobuf record error stacktrace: ~p", [Stacktrace]),
            {error, rest_protobuf_record_not_found}
    end.

%% Utility functions for debugging
debug_truncate(Data) when is_binary(Data), byte_size(Data) > 100 ->
    <<Prefix:100/binary, _/binary>> = Data,
    <<Prefix/binary, "...">>;
debug_truncate(Data) when is_list(Data), length(Data) > 10 ->
    {Prefix, _} = lists:split(10, Data),
    Prefix ++ ['...'];
debug_truncate(Data) ->
    Data.

type_of(X) when is_list(X) -> list;
type_of(X) when is_map(X) -> map;
type_of(X) when is_tuple(X) -> tuple;
type_of(X) when is_binary(X) -> binary;
type_of(X) when is_atom(X) -> atom;
type_of(X) when is_integer(X) -> integer;
type_of(_) -> unknown.
