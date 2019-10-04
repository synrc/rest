-module(rest_kvs).
-include_lib("kvs/include/kvs.hrl").
-export([exists/1, exists/2, new/1, get/1, delete/2, post/2, post/3]).
-export([init/2, resource_exists/2, allowed_methods/2, content_types_provided/2,
         to_html/2, to_json/2, content_types_accepted/2, delete_resource/2,
         handle_urlencoded_data/2, handle_json_data/2]).

-ifndef(REST_JSON).
-define(REST_JSON, (application:get_env(rest,json,jsone))).
-endif.

c(X) -> binary_to_list(X).

% kvs rest api

new(Type)          -> Type:new().
exists(Mod)        -> {X,_} = kvs:get(writer,c(Mod)), X == ok.
exists(Mod,Id)     -> {X,_} = kvs:get(c(Mod),c(Id)), X == ok.
get(Mod)           -> kvs:all(Mod).
delete(Mod,Id)     -> kvs:delete(Mod,Id).
post(Mod,Resource) when is_tuple(Resource) -> kvs:append(Mod,Resource).
post(Type,Mod,Data)     when is_list(Data) -> post(Mod,Type:from_json(new(Type))).

% cowboy rest api

update_req(Req) -> #{ bindings := Bindings, path_info := List } = Req,
   Req#{bindings => Bindings#{ resource => list_to_binary("/" ++
      string:join(lists:map(fun (X) -> binary_to_list(X) end, List),"/")) }}.

init(#{bindings := #{id := Id}} = Req, State) -> {cowboy_rest, update_req(Req), State};
init(Req, State) -> {cowboy_rest, update_req(Req), State}.

resource_exists(#{bindings := #{resource := Module, id := Id}} = Req, State) -> {rest_kvs:exists(Module,Id), Req, State};
resource_exists(#{bindings := #{resource := Module}} = Req, State) -> {rest_kvs:exists(Module), Req, State};
resource_exists(#{bindings := #{id := _}} = Req, State) -> {true, Req, State}.

allowed_methods(#{bindings := #{resource := _}} = Req, State) -> {[<<"GET">>, <<"POST">>], Req, State};
allowed_methods(#{bindings := #{resource := _, id := _}} = Req, State) -> {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

delete_resource(#{bindings := #{resource := Module, id := []}} = Req, State) -> {[], Req, State};
delete_resource(#{bindings := #{resource := Module, id := Id}} = Req, State) -> {rest_kvs:delete(Module,Id), Req, State}.

content_types_provided(#{bindings := #{resource := Module}} = Req, State) ->
    {case application:get_env(rest,html,false) of
         false -> [{<<"application/json">>, to_json}];
         true  -> [{<<"text/html">>, to_html},
                   {<<"application/json">>, to_json}] end, Req, State}.

to_html(#{bindings := #{resource := Module, id := Id}} = Req, State) ->
    Body = case Id of
               Id when Id==[];Id==undefined -> [ rest_kvs:to_html(Module, Resource) || Resource <- rest_kvs:get(Module,Id) ];
               _ -> rest_kvs:to_html(rest_kvs:get(Module,Id)) end,
    Html = case application:get_env(rest,html_layout,false) of
               true  -> rest_kvs:html_layout(Module, Req, Body);
               false -> default_html_layout(Body) end,
    {Html, Req, State}.

default_html_layout(Body) -> [<<"<html><body>">>, Body, <<"</body></html>">>].

to_json(#{bindings := #{resource := Module, id := Id}} = Req, State) ->
    {ok,Resource} = kvs:get(c(Module),c(Id)),
    Type = element(1,Resource),
    {iolist_to_binary(?REST_JSON:encode(Type:to_json(Resource))), Req, State};
to_json(#{bindings := #{resource := Module}} = Req, State) ->
    Fold = [ begin M = element(1,Resource), M:to_json(Resource) end || Resource <- kvs:all(c(Module))],
    io:format("DEBUG: ~p",[{Module,Fold}]),
    {iolist_to_binary(?REST_JSON:encode([{Module,Fold}])), Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/x-www-form-urlencoded">>, handle_urlencoded_data},
    {<<"application/json">>, handle_json_data}], Req, State}.

handle_urlencoded_data(#{bindings := #{resource := Module, id := Id}} = Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_urlencoded_body(Req),
    io:format("FORM: ~p~n",[Data]),
    {handle_data(Module, Id, Data, Req), Req2, State};

handle_urlencoded_data(#{bindings := #{resource := Module}} = Req0, State) ->
    {ok, Data1, Req} = cowboy_req:read_urlencoded_body(Req0),
    io:format("FORM: ~p, Data1: ~p~n",[Module,Data1]),
    {handle_data(Module, [], Data1, Req), Req, State}.

handle_json_data(#{bindings := #{resource := Module, id := Id}} = Req, State) ->
    {ok, Binary, Req2} = cowboy_req:read_body(Req),
    io:format("JSON: ~p~n",[Binary]),
    Data = case ?REST_JSON:decode(Binary) of {struct, Struct} -> Struct; S -> S end,
    {handle_data(Module, Id, Data, Req), Req2, State};

handle_json_data(#{bindings := #{resource := Module}} = Req, State) ->
    {ok, Binary, Req2} = cowboy_req:read_body(Req),
    io:format("JSON: ~p~n",[Binary]),
    Data = case ?REST_JSON:decode(Binary) of {struct, Struct} -> Struct; S -> S end,
    {handle_data(Module, [], Data, Req), Req2, State}.

handle_data(Mod, Id, Data, Req) ->
    Type = proplists:get_value(<<"rec">>, Data),
    Valid = case application:get_env(rest,validate,false) of
                true  -> rest_kvs:validate(Mod,Id, Data);
                false -> default_validate(Mod, Id, Data, Req) end,
    case {Valid, Id} of
        {false, _}         -> false;
        {true,  <<"undefined">>} -> rest_kvs:post(Type,Mod,Data);
        {true,  _}         -> case application:get_env(rest,custom_put,false) of
                                  true  -> Type:put(Mod, Id, Data);
                                  false -> default_put(Type, Mod, Id, Data, Req) end
    end.

default_put(Type, Mod, Id, Data, Req) when is_map(Data) -> default_put(Type, Mod, Id, maps:to_list(Data), Req);
default_put(Type, Mod, Id, Data, Req) ->
    NewRes = Type:from_json(Data, rest_kvs:get(Mod,Id)),
    NewId = proplists:get_value(id, Type:to_json(NewRes)),
    case Id =/= NewId of
        true when Id =:= [] -> skip;
        true -> rest_kvs:delete(Mod,Id);
        false -> true end,
    rest_kvs:post(Type,Mod,NewRes).

default_validate(Mod, Id, DataX, Req0) when is_map(DataX) -> default_validate(Mod, Id, maps:to_list(DataX), Req0);
default_validate(Mod, Id, Data, Req0) ->
    Allowed = case application:get_env(rest, keys_allowed, false) of
      true  -> rest_kvs:keys_allowed(c(Mod),proplists:get_keys(Data));
      false -> true end,
    validate_match(Mod, Id, Allowed, proplists:get_value(<<"id">>, Data)).

validate_match(_Mod,  [], true, [])      -> false;
validate_match( Mod,  [], true, NewId)   -> not rest_kvs:exists(Mod,NewId);
validate_match(_Mod, _Id, true, [])      -> true;
validate_match(_Mod,  Id, true, Id)      -> true;
validate_match( Mod, _Id, true, NewId)   -> not rest_kvs:exists(Mod,NewId);
validate_match(   _,         _,    _, _) -> false.

