-module(rest_cowboy).

-author('Dmitry Bushmelev').

-record(st,
        {resource_module = undefined :: atom(),
         resource_id = undefined :: binary()}).

-export([init/2,
         rest_init/2,
         resource_exists/2,
         allowed_methods/2,
         content_types_provided/2,
         to_html/2,
         to_json/2,
         content_types_accepted/2,
         delete_resource/2,
         handle_urlencoded_data/2,
         handle_json_data/2]).

init(Req, Opts) -> {cowboy_rest, Req, Opts}.

-ifndef(REST_JSON).

-define(REST_JSON,
        application:get_env(rest, json, jsone)).

-endif.

c(X) -> list_to_atom(binary_to_list(X)).

rest_init(Req, _Opts) ->
    {Resource, Req1} = cowboy_req:binding(resource, Req),
    Module = case rest_module(Resource) of
                 {ok, M} -> M;
                 _ -> undefined
             end,
    {Id, Req2} = cowboy_req:binding(id, Req1),
    {Origin, Req3} = cowboy_req:header(<<"origin">>,
                                       Req2,
                                       <<"*">>),
    Req4 =
        cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>,
                                   Origin,
                                   Req3),
    io:format("REST INIT~p"),
    {ok,
     Req4,
     #st{resource_module = Module, resource_id = Id}}.

resource_exists(#{bindings :=
                      #{resource := Module, id := Id}} =
                    Req,
                State) ->
    M = c(Module),
    io:format("EXISTS: ~p dymamic: ~p~n",
              [Id, M:exists(Id)]),
    {M:exists(Id), Req, State};
resource_exists(#{bindings := #{resource := Module}} =
                    Req,
                State) ->
    io:format("resource ~p: no-id~n", [Module]),
    {true, Req, State};
resource_exists(#{bindings := #{id := _}} = Req,
                State) ->
    io:format("EXISTS id: true~n"),
    {true, Req, State}.

allowed_methods(#{bindings := #{resource := _}} = Req,
                State) ->
    {[<<"GET">>, <<"POST">>], Req, State};
allowed_methods(#{bindings :=
                      #{resource := _, id := _}} =
                    Req,
                State) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(#{bindings :=
                             #{resource := Module}} =
                           Req,
                       State) ->
    {case erlang:function_exported(c(Module), to_html, 1) of
         false -> [{<<"application/json">>, to_json}];
         true ->
             [{<<"text/html">>, to_html},
              {<<"application/json">>, to_json}]
     end,
     Req,
     State}.

to_html(#{bindings := #{resource := Module, id := Id}} =
            Req,
        State) ->
    M = c(Module),
    Body = case Id of
               undefined ->
                   [M:to_html(Resource) || Resource <- M:get()];
               _ -> M:to_html(M:get(Id))
           end,
    Html = case erlang:function_exported(M, html_layout, 2)
               of
               true -> M:html_layout(Req, Body);
               false -> default_html_layout(Body)
           end,
    {Html, Req, State}.

default_html_layout(Body) ->
    [<<"<html><body>">>, Body, <<"</body></html>">>].

to_json(#{bindings := #{resource := Module, id := Id}} =
            Req,
        State) ->
    io:format("~p ~p ~p~n", [?FUNCTION_NAME, Module, Id]),
    M = c(Module),
    Struct = case Id of
                 undefined ->
                     [{M, [M:to_json(Resource) || Resource <- M:get()]}];
                 _ -> M:to_json(M:get(Id))
             end,
    {iolist_to_binary((?REST_JSON):encode(Struct)),
     Req,
     State};
to_json(#{bindings := #{resource := Module}} = Req,
        State) ->
    io:format("~p ~p~n", [?FUNCTION_NAME, Module]),
    M = c(Module),
    Struct = [{M,
               [M:to_json(Resource) || Resource <- M:get()]}],
    {iolist_to_binary((?REST_JSON):encode(Struct)),
     Req,
     State}.

content_types_accepted(Req, State) ->
    {[{<<"application/x-www-form-urlencoded">>,
       handle_urlencoded_data},
      {<<"application/json">>, handle_json_data}],
     Req,
     State}.

handle_urlencoded_data(#{bindings :=
                             #{resource := Module}} =
                           Req0,
                       State) ->
    {ok, Data1, Req} =
        cowboy_req:read_urlencoded_body(Req0),
    io:format("FORM: ~p, Data1: ~p~n", [Module, Data1]),
    {handle_data(c(Module), [], Data1, Req), Req, State};
handle_urlencoded_data(#{bindings :=
                             #{resource := Module, id := Id}} =
                           Req,
                       State) ->
    {ok, Data, Req2} = cowboy_req:read_urlencoded_body(Req),
    io:format("FORM: ~p~n", [Data]),
    {handle_data(c(Module), Id, Data, Req), Req2, State}.

handle_json_data(#{bindings := #{resource := Module}} =
                     Req,
                 State) ->
    {ok, Binary, Req2} = cowboy_req:read_body(Req),
    io:format("JSON: ~p~n", [Binary]),
    Data = case (?REST_JSON):decode(Binary) of
               {struct, Struct} -> Struct;
               S -> S
           end,
    {handle_data(c(Module), [], Data, Req), Req2, State};
handle_json_data(#{bindings :=
                       #{resource := Module, id := Id}} =
                     Req,
                 State) ->
    {ok, Binary, Req2} = cowboy_req:read_body(Req),
    io:format("JSON: ~p~n", [Binary]),
    Data = case (?REST_JSON):decode(Binary) of
               {struct, Struct} -> Struct;
               S -> S
           end,
    {handle_data(c(Module), Id, Data, Req), Req2, State}.

handle_data(Mod, Id, Data, Req) ->
    io:format("handle_data(~p)~n", [{Mod, Id, Data, Req}]),
    Valid = case erlang:function_exported(Mod, validate, 2)
                of
                true -> Mod:validate(Id, Data);
                false -> default_validate(Mod, Id, Data, Req)
            end,
    io:format("Valid ~p Id ~p~n", [Valid, Id]),
    case {Valid, Id} of
        {false, _} -> false;
        {true, []} -> Mod:post(Data);
        {true, <<"undefined">>} -> Mod:post(Data);
        {true, _} ->
            case erlang:function_exported(Mod, put, 2) of
                true -> Mod:put(Id, Data);
                false -> default_put(Mod, Id, Data, Req)
            end
    end.

default_put(Mod, Id, Data, Req) when is_map(Data) ->
    default_put(Mod, Id, maps:to_list(Data), Req);
default_put(Mod, Id, Data, Req) ->
    NewRes = Mod:from_json(Data, Mod:get(Id)),
    NewId = proplists:get_value(id, Mod:to_json(NewRes)),
    io:format("Id ~p NewId ~p~n", [Id, NewId]),
    case Id =/= NewId of
        true when Id =:= [] -> skip;
        true -> Mod:delete(Id);
        false -> true
    end,
    Mod:post(NewRes).

default_validate(Mod, Id, DataX, Req0)
    when is_map(DataX) ->
    default_validate(Mod, Id, maps:to_list(DataX), Req0);
default_validate(Mod, Id, Data, Req0) ->
    Allowed = case erlang:function_exported(Mod,
                                            keys_allowed,
                                            1)
                  of
                  true -> Mod:keys_allowed(proplists:get_keys(Data));
                  false -> true
              end,
    validate_match(Mod,
                   Id,
                   Allowed,
                   proplists:get_value(<<"id">>, Data)).

validate_match(_Mod, [], true, []) -> false;
validate_match(Mod, [], true, NewId) ->
    not Mod:exists(NewId);
validate_match(_Mod, _Id, true, []) -> true;
validate_match(_Mod, Id, true, Id) -> true;
validate_match(Mod, _Id, true, NewId) ->
    not Mod:exists(NewId);
validate_match(_, _, _, _) -> false.

delete_resource(#{bindings :=
                      #{resource := Module, id := []}} =
                    Req,
                State) ->
    {[], Req, State};
delete_resource(#{bindings :=
                      #{resource := Module, id := Id}} =
                    Req,
                State) ->
    M = c(Module),
    io:format("DELETE: ~p ~p ~p~n", [M, Id, M:delete(Id)]),
    {M:delete(Id), Req, State}.

rest_module(Module) when is_binary(Module) ->
    rest_module(binary_to_list(Module));
rest_module(Module) ->
    try M = list_to_existing_atom(Module),
        Info = proplists:get_value(attributes, M:module_info()),
        true = lists:member(rest,
                            proplists:get_value(behaviour, Info)),
        {ok, M}
    catch
        error:Error -> {error, Error}
    end.
