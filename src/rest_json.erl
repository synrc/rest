%% @doc REST JSON Utilities
%% Utilities for JSON serialization and deserialization
-module(rest_json).

-include_lib("kvs/include/metainfo.hrl").

-export([
    from_json/2,
    to_json/1,
    to_binary/1,
    binarize/1,
    atomize/1,
    parse/1
]).

-type json_term() ::
    null | boolean() | number() | binary() |
    [json_term()] | #{binary() => json_term()}.

%% @doc Convert JSON to Erlang record
-spec from_json(json_term() | binary(), module()) -> term().
from_json(<<Data/binary>>, _Module) ->
    binary_to_list(Data);
from_json({struct, Props}, Module) ->
    from_json(Props, Module);
from_json([{Key, _} | _] = Props, Module) when Key =/= struct ->
    try
        Module:from_json(binarize(Props), Module:new())
    catch
        error:undef ->
            error({module_not_found, Module});
        Error:Reason ->
            error({conversion_failed, Error, Reason})
    end;
from_json(Any, _Module) ->
    Any.

%% @doc Convert Erlang term to JSON, omitting undefined values
-spec to_json(term()) -> json_term().
to_json(undefined) ->
    undefined;  % undefined values will be omitted
to_json(X) when is_tuple(X) ->
    case element(1, X) of
        Module when is_atom(Module) ->
            % Only treat as record if first element is an atom (potential module name)
            to_map(Module, X);
        _ ->
            % Handle other tuples (like date/time tuples) as regular data
            json_match(X)
    end;
to_json(Data) ->
    case is_string(Data) of
        true -> to_binary(Data);
        false -> json_match(Data)
    end.

%% @doc Convert atom keys to binary keys in a property list, omitting undefined values
-spec binarize([{atom() | binary(), term()}]) -> [{binary(), term()}].
binarize([{Key, _} | _] = Props) when Key =/= struct ->
    ProcessedProps = lists:map(fun
        ({K, V}) when is_atom(K) ->
            {atom_to_binary(K, utf8), allowed_value(V)};
        ({K, V}) when is_binary(K) ->
            {K, allowed_value(V)}
    end, Props),
    %% Filter out undefined values
    [{K, V} || {K, V} <- ProcessedProps, V =/= undefined];
binarize(X) ->
    X.

%% @doc Convert binary keys to atom keys in a property list
-spec atomize([{binary() | atom(), term()}]) -> [{atom(), term()}].
atomize([{Key, _} | _] = Props) when Key =/= struct ->
    lists:map(fun
        ({K, V}) when is_atom(K) ->
            {K, V};
        ({K, V}) when is_binary(K) ->
            try
                {binary_to_existing_atom(K, utf8), V}
            catch
                error:badarg ->
                    error({unknown_atom, K})
            end
    end, Props);
atomize(X) ->
    X.

%% @doc Convert various types to binary
-spec to_binary(term()) -> binary().
to_binary(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) ->
    B;
to_binary(I) when is_integer(I) ->
    integer_to_binary(I);
to_binary(F) when is_float(F) ->
    float_to_binary(F, [{decimals, 9}, compact]);
to_binary(L) when is_list(L) ->
    iolist_to_binary(L);
to_binary(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).

%% @doc Parse Erlang term from string
-spec parse(string() | binary()) -> term().
parse(String) when is_binary(String) ->
    parse(binary_to_list(String));
parse(String) when is_list(String) ->
    try
        {ok, Tokens, _EndLine} = erl_scan:string(String ++ "."),
        {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
        {value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
        Value
    catch
        Error:Reason ->
            error({parse_error, Error, Reason})
    end.

%% @private
%% Convert module record to map
to_map(Module, Resource) ->
    case erlang:function_exported(Module, to_json, 1) of
        true ->
            try
                Module:to_json(Resource)
            catch
                Error:Reason ->
                    error({to_json_failed, Module, Error, Reason})
            end;
        false ->
            record_to_map(Resource)
    end.

%% @private
%% Convert record to map using metainfo if available, omitting undefined values
record_to_map(Record) when is_tuple(Record) ->
    Table = element(1, Record),
    case get_fields(Table) of
        {ok, Fields} ->
            BinFields = [atom_to_binary(F, utf8) || F <- Fields],
            Values = [element(I, Record) || I <- lists:seq(2, tuple_size(Record))],
            %% Apply to_json to each field value to handle string conversion
            ProcessedValues = [to_json(V) || V <- Values],
            Pairs = lists:zip(BinFields, ProcessedValues),
            %% Filter out undefined values
            DefinedPairs = [{K, V} || {K, V} <- Pairs, V =/= undefined],
            maps:from_list(DefinedPairs);
        {error, _} ->
            %% Fallback to generic field names, omitting undefined values
            Size = tuple_size(Record),
            Names = [<<"field_", (integer_to_binary(I))/binary>>
                    || I <- lists:seq(1, Size-1)],
            Values = [element(I, Record) || I <- lists:seq(2, Size)],
            %% Apply to_json to each field value to handle string conversion
            ProcessedValues = [to_json(V) || V <- Values],
            Pairs = lists:zip(Names, ProcessedValues),
            %% Filter out undefined values
            DefinedPairs = [{K, V} || {K, V} <- Pairs, V =/= undefined],
            maps:from_list(DefinedPairs)
    end;
record_to_map(Data) ->
    Data.

%% @private
%% Get record fields from metainfo
get_fields(Table) ->
    try
        case application:get_env(rest, metainfo_module) of
            {ok, MetaModule} ->
                Schema = MetaModule:metainfo(),
                Tables = Schema#schema.tables,
                case lists:keyfind(Table, #table.name, Tables) of
                    false -> {error, not_found};
                    TableDef -> {ok, TableDef#table.fields}
                end;
            undefined ->
                {error, no_metainfo_module}
        end
    catch
        _:_ ->
            {error, schema_not_found}
    end.

%% @private
%% Handle JSON matching for different data structures, omitting undefined values
json_match([{_, _} | _] = Props) ->
    ProcessedProps = [{to_binary(Key), to_json(Value)} || {Key, Value} <- Props],
    %% Filter out undefined values
    [{K, V} || {K, V} <- ProcessedProps, V =/= undefined];
json_match([_ | _] = NonEmptyList) ->
    ProcessedList = [to_json(X) || X <- NonEmptyList],
    %% Filter out undefined values from lists
    [X || X <- ProcessedList, X =/= undefined];
json_match(Tuple) when is_tuple(Tuple) ->
    %% Convert tuple to list for JSON serialization
    TupleList = tuple_to_list(Tuple),
    ProcessedList = [to_json(X) || X <- TupleList],
    %% Filter out undefined values from the converted list
    [X || X <- ProcessedList, X =/= undefined];
json_match(undefined) ->
    undefined;  % Will be filtered out by caller
json_match(Any) ->
    Any.

%% @private
%% Check if data is a string (list of characters) - improved version
is_string([]) ->
    true;  % Empty list is considered a string
is_string([H | T]) when is_integer(H), H >= 0, H =< 1114111 ->
    %% Check if it's a printable string and all elements are valid characters
    case io_lib:printable_unicode_list([H | T]) of
        true -> lists:all(fun is_char/1, T);
        false -> false
    end;
is_string(_) ->
    false.

%% @private
%% Check if value is a valid character
is_char(C) ->
    is_integer(C) andalso C >= 0 andalso C =< 1114111.  % Unicode range

%% @private
%% Filter out values that shouldn't be serialized
allowed_value(undefined) ->
    undefined;  % Keep undefined to be filtered out later
allowed_value(X) when is_reference(X) ->
    null;  % Convert references to null
allowed_value(X) when is_pid(X) ->
    null;  % Convert PIDs to null
allowed_value(X) when is_port(X) ->
    null;  % Convert ports to null
allowed_value(X) ->
    X.
