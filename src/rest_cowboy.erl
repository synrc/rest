-module(rest_cowboy).
-author('Erwin Flaming').

%% Version-aware REST handler for Cowboy
%% Configure: {rest, [{modules, [{v1, [{users, user_rest_v1}]}]}]}
%% Routes: {"/rest/:version/:resource", rest_cowboy, []}, {"/rest/:version/:resource/:id", rest_cowboy, []}
%% URL: /rest/{version}/{resource}/[{id}]

-record(state, {module, id, version}).

-export([init/2, rest_init/2, resource_exists/2, allowed_methods/2,
         content_types_provided/2, to_json/2, content_types_accepted/2,
         delete_resource/2, handle_json_data/2, handle_urlencoded_data/2,
         clean_json/1]).

-define(JSON, application:get_env(rest, json, jsone)).
-define(CORS, <<"Access-Control-Allow-Origin">>).

%% Cowboy callbacks
init(Req, Opts) -> {cowboy_rest, Req, Opts}.

rest_init(Req, _) ->
    {Version, Req1} = cowboy_req:binding(version, Req),
    {Resource, Req2} = cowboy_req:binding(resource, Req1),
    {Id, Req3} = cowboy_req:binding(id, Req2),
    Module = resolve_module(Version, Resource),
    Req4 = cowboy_req:set_resp_header(?CORS, <<"*">>, Req3),
    State = #state{module = Module, id = norm_id(Id), version = norm_version(Version)},
    {ok, Req4, State}.

resource_exists(#{bindings := #{version := V, resource := R, id := Id}} = Req, State) ->
    Module = resolve_module(V, R),
    Exists = case Module of
        undefined -> false;
        _ -> try Module:exists(Id) catch _:_ -> false end
    end,
    {Exists, Req, State};
resource_exists(#{bindings := #{version := V, resource := R}} = Req, State) ->
    Module = resolve_module(V, R),
    {Module =/= undefined, Req, State};
resource_exists(Req, State) -> {false, Req, State}.

allowed_methods(#{bindings := #{id := _}} = Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State};
allowed_methods(#{bindings := #{resource := _}} = Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State};
allowed_methods(Req, State) -> {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(#{bindings := #{version := V, resource := R, id := Id}} = Req, State) ->
    Module = resolve_module(V, R),
    try
        Struct = case Module of
            undefined -> #{error => <<"Module not found">>};
            _ -> case norm_id(Id) of
                undefined -> [clean_json(rest:to_map(Module, Res)) || Res <- Module:get()];
                NId -> clean_json(rest:to_map(Module, Module:get(NId)))
            end
        end,
        Json = iolist_to_binary((?JSON):encode(Struct)),
        {Json, Req, State}
    catch _:Error:Stacktrace ->
        ErrorJson = iolist_to_binary((?JSON):encode(#{error => Error})),
            io:format("log ~p ~p: ~p ~n", [?MODULE, ?LINE, {Error, Stacktrace}]),
        {ErrorJson, Req, State}
    end;
to_json(#{bindings := #{version := V, resource := R}} = Req, State) ->
    Module = resolve_module(V, R),
    try
        Struct = case Module of
            undefined -> #{error => <<"Module not found">>};
            _ -> [clean_json(rest:to_map(Module, Res)) || Res <- Module:get()]
        end,
        Json = iolist_to_binary((?JSON):encode(Struct)),
        {Json, Req, State}
    catch _:Error:Stacktrace ->
        ErrorJson = iolist_to_binary((?JSON):encode(#{error => Error})),
            io:format("log ~p ~p: ~p ~n", [?MODULE, ?LINE, {Error, Stacktrace}]),
        {ErrorJson, Req, State}
    end.

content_types_accepted(Req, State) ->
    {[{<<"application/x-www-form-urlencoded">>, handle_urlencoded_data},
      {<<"application/json">>, handle_json_data}], Req, State}.

handle_urlencoded_data(#{bindings := #{version := V, resource := R, id := Id}} = Req, State) ->
    Module = resolve_module(V, R),
    case {Module, cowboy_req:read_urlencoded_body(Req)} of
        {undefined, _} -> {false, Req, State};
        {_, {ok, Data, Req2}} -> {handle_data(Module, norm_id(Id), Data), Req2, State};
        _ -> {false, Req, State}
    end;
handle_urlencoded_data(#{bindings := #{version := V, resource := R}} = Req, State) ->
    Module = resolve_module(V, R),
    case {Module, cowboy_req:read_urlencoded_body(Req)} of
        {undefined, _} -> {false, Req, State};
        {_, {ok, Data, Req2}} -> {handle_data(Module, undefined, Data), Req2, State};
        _ -> {false, Req, State}
    end.

handle_json_data(#{bindings := #{version := V, resource := R, id := Id}} = Req, State) ->
    Module = resolve_module(V, R),
    case {Module, cowboy_req:read_body(Req)} of
        {undefined, _} -> {false, Req, State};
        {_, {ok, Binary, Req2}} ->
            try
                Data = (?JSON):decode(Binary),
                {handle_data(Module, norm_id(Id), Data), Req2, State}
            catch _:_ -> {false, Req2, State}
            end;
        _ -> {false, Req, State}
    end;
handle_json_data(#{bindings := #{version := V, resource := R}} = Req, State) ->
    Module = resolve_module(V, R),
    case {Module, cowboy_req:read_body(Req)} of
        {undefined, _} -> {false, Req, State};
        {_, {ok, Binary, Req2}} ->
            try
                Data = (?JSON):decode(Binary),
                {handle_data(Module, undefined, Data), Req2, State}
            catch _:_ -> {false, Req2, State}
            end;
        _ -> {false, Req, State}
    end.

delete_resource(#{bindings := #{version := V, resource := R, id := Id}} = Req, State) ->
    Module = resolve_module(V, R),
    case {Module, norm_id(Id)} of
        {undefined, _} -> {false, Req, State};
        {_, undefined} -> {false, Req, State};
        {_, NId} -> try {Module:delete(NId), Req, State} catch _:_ -> {false, Req, State} end
    end;
delete_resource(Req, State) -> {false, Req, State}.

%% Helper functions
resolve_module(undefined, _) -> undefined;
resolve_module(_, undefined) -> undefined;
resolve_module(VBin, RBin) when is_binary(VBin), is_binary(RBin) ->
    try
        V = binary_to_existing_atom(VBin, utf8),
        R = binary_to_existing_atom(RBin, utf8),
        case application:get_env(rest, modules) of
            {ok, Versions} ->
                case proplists:get_value(V, Versions) of
                    undefined -> undefined;
                    Resources -> proplists:get_value(R, Resources)
                end;
            _ -> undefined
        end
    catch _:_ -> undefined
    end.

norm_version(undefined) -> undefined;
norm_version(VBin) when is_binary(VBin) ->
    try binary_to_existing_atom(VBin, utf8) catch _:_ -> undefined end.

norm_id(undefined) -> undefined;
norm_id(<<"undefined">>) -> undefined;
norm_id(Id) when is_binary(Id) -> Id.

handle_data(undefined, _, _) -> false;
handle_data(Module, undefined, Data) -> try Module:post(Data) catch _:_ -> false end;
handle_data(Module, Id, Data) ->
    try
        case erlang:function_exported(Module, put, 2) of
            true -> Module:put(Id, Data);
            false ->
                Existing = Module:get(Id),
                New = Module:from_json(Data, Existing),
                Module:post(New)
        end
    catch _:_ -> false
    end.

clean_json(Data) when is_map(Data) ->
    maps:fold(fun(K, V, Acc) ->
        case V of
            undefined -> Acc; <<"undefined">> -> Acc; "undefined" -> Acc; [] -> Acc;
            _ -> maps:put(K, clean_json(V), Acc)
        end
    end, #{}, Data);
clean_json(Data) when is_list(Data) -> [clean_json(Item) || Item <- Data];
clean_json(Data) -> Data.
