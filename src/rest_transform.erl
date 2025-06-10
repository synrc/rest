%% @doc REST Parse Transform
%% Parse transform for generating JSON serialization code
-module(rest_transform).

-export([parse_transform/2]).

-type ast_node() :: erl_parse:abstract_form().
-type transform_options() :: [term()].

%% @doc Parse transform entry point
-spec parse_transform([ast_node()], transform_options()) -> [ast_node()].
parse_transform(Forms, _Options) ->
    case find_rest_record(Forms) of
        undefined ->
            Forms;  % No rest_record attribute found
        RecordName ->
            RecordFields = find_record_fields(RecordName, Forms),
            Forms1 = maybe_generate_function({from_json, 2}, RecordName, RecordFields, Forms),
            Forms2 = maybe_generate_function({to_json, 1}, RecordName, RecordFields, Forms1),
            Forms2
    end.

%% @private
%% Find the rest_record attribute
find_rest_record([]) ->
    undefined;
find_rest_record([{attribute, _, rest_record, RecordName} | _]) ->
    RecordName;
find_rest_record([_ | Forms]) ->
    find_rest_record(Forms).

%% @private
%% Find record fields for the given record name
find_record_fields(RecordName, [{attribute, _, record, {RecordName, Fields}} | _]) ->
    [extract_field_name(Field) || Field <- Fields];
find_record_fields(RecordName, [_ | Forms]) ->
    find_record_fields(RecordName, Forms);
find_record_fields(_, []) ->
    [].

%% @private
%% Extract field name from record field definition
extract_field_name({record_field, _, {atom, _, Field}}) ->
    Field;
extract_field_name({record_field, _, {atom, _, Field}, _Default}) ->
    Field;
extract_field_name({typed_record_field, {record_field, _, {atom, _, Field}, _}, _Type}) ->
    Field.

%% @private
%% Generate function if not already exported
maybe_generate_function({FunName, Arity} = Fun, RecordName, Fields, Forms) ->
    case is_function_exported(Fun, Forms) of
        true ->
            Forms;
        false ->
            generate_function(FunName, Arity, RecordName, Fields, Forms)
    end.

%% @private
%% Check if function is already exported
is_function_exported(Fun, Forms) ->
    Exports = [E || {attribute, _, export, E} <- Forms],
    lists:member(Fun, lists:flatten(Exports)).

%% @private
%% Generate the actual function
generate_function(from_json, 2, RecordName, Fields, Forms) ->
    insert_function(generate_from_json_function(RecordName, Fields), Forms);
generate_function(to_json, 1, RecordName, Fields, Forms) ->
    insert_function(generate_to_json_function(RecordName, Fields), Forms).

%% @private
%% Insert function before eof
insert_function(FunctionDef, Forms) ->
    {BeforeEof, [{eof, Line}]} = lists:split(length(Forms) - 1, Forms),
    NewExport = add_export_if_needed(FunctionDef, BeforeEof),
    NewExport ++ [FunctionDef, {eof, Line}].

%% @private
%% Add export for the generated function
add_export_if_needed({function, Line, FunName, Arity, _}, Forms) ->
    Fun = {FunName, Arity},
    case is_function_exported(Fun, Forms) of
        true ->
            Forms;
        false ->
            add_export(Fun, Forms)
    end.

%% @private
%% Add export to the last export attribute or create new one
add_export(Fun, Forms) ->
    case find_last_export(Forms) of
        {found, Index, {attribute, Line, export, Exports}} ->
            NewExport = {attribute, Line, export, [Fun | Exports]},
            replace_at_index(Index, NewExport, Forms);
        not_found ->
            % Add new export after module declaration
            case find_module_declaration(Forms) of
                {found, Index} ->
                    NewExport = {attribute, 1, export, [Fun]},
                    insert_at_index(Index + 1, NewExport, Forms);
                not_found ->
                    Forms
            end
    end.

%% @private
%% Find the last export attribute
find_last_export(Forms) ->
    find_last_export(Forms, 1, not_found).

find_last_export([], _, Result) ->
    Result;
find_last_export([{attribute, _, export, _} = Export | Rest], Index, _) ->
    find_last_export(Rest, Index + 1, {found, Index, Export});
find_last_export([_ | Rest], Index, Result) ->
    find_last_export(Rest, Index + 1, Result).

%% @private
%% Find module declaration
find_module_declaration(Forms) ->
    find_module_declaration(Forms, 1).

find_module_declaration([], _) ->
    not_found;
find_module_declaration([{attribute, _, module, _} | _], Index) ->
    {found, Index};
find_module_declaration([_ | Rest], Index) ->
    find_module_declaration(Rest, Index + 1).

%% @private
%% Replace element at index
replace_at_index(Index, NewElement, List) ->
    {Before, [_ | After]} = lists:split(Index - 1, List),
    Before ++ [NewElement] ++ After.

%% @private
%% Insert element at index
insert_at_index(Index, NewElement, List) ->
    {Before, After} = lists:split(Index, List),
    Before ++ [NewElement] ++ After.

%% @private
%% Generate from_json function
generate_from_json_function(RecordName, Fields) ->
    Line = 1,
    {function, Line, from_json, 2, [
        % from_json([], Acc) -> Acc
        {clause, Line,
         [{nil, Line}, {var, Line, 'Acc'}],
         [],
         [{var, Line, 'Acc'}]},

        % Field clauses
        generate_from_json_clauses(Line, RecordName, Fields) ++

        % from_json([_|Json], Acc) -> from_json(rest_json:binarize(Json), Acc)
        [{clause, Line,
          [{cons, Line, {var, Line, '_'}, {var, Line, 'Json'}}, {var, Line, 'Acc'}],
          [],
          [{call, Line, {atom, Line, from_json},
            [{call, Line, {remote, Line, {atom, Line, rest_json}, {atom, Line, binarize}},
              [{var, Line, 'Json'}]},
             {var, Line, 'Acc'}]}]}]
    ]}.

%% @private
%% Generate field clauses for from_json
generate_from_json_clauses(_Line, _RecordName, []) ->
    [];
generate_from_json_clauses(Line, RecordName, [Field | Fields]) ->
    FieldVar = field_var(Field),
    FieldBin = atom_to_list(Field),
    [{clause, Line,
      [{cons, Line,
        {tuple, Line, [
          {bin, Line, [{bin_element, Line, {string, Line, FieldBin}, default, default}]},
          {var, Line, FieldVar}
        ]},
        {var, Line, 'Json'}},
       {var, Line, 'Acc'}],
      [],
      [{call, Line, {atom, Line, from_json},
        [{var, Line, 'Json'},
         {record, Line, {var, Line, 'Acc'}, RecordName,
          [{record_field, Line, {atom, Line, Field},
            {call, Line, {remote, Line, {atom, Line, rest_json}, {atom, Line, from_json}},
             [{var, Line, FieldVar}, {atom, Line, RecordName}]}}]}]}]}
    | generate_from_json_clauses(Line, RecordName, Fields)].

%% @private
%% Generate to_json function
generate_to_json_function(RecordName, Fields) ->
    Line = 1,
    FieldVars = [{record_field, Line, {atom, Line, F}, {var, Line, field_var(F)}}
                || F <- Fields],
    JsonList = generate_json_list(Line, Fields),

    {function, Line, to_json, 1, [
        {clause, Line,
         [{record, Line, RecordName, FieldVars}],
         [],
         [JsonList]}
    ]}.

%% @private
%% Generate JSON property list
generate_json_list(Line, Fields) ->
    generate_json_cons(Line, Fields).

generate_json_cons(Line, []) ->
    {nil, Line};
generate_json_cons(Line, [Field | Fields]) ->
    {cons, Line,
     {tuple, Line, [
       {atom, Line, Field},
       {call, Line, {remote, Line, {atom, Line, rest_json}, {atom, Line, to_json}},
        [{var, Line, field_var(Field)}]}
     ]},
     generate_json_cons(Line, Fields)}.

%% @private
%% Generate variable name for field
field_var(Field) ->
    list_to_atom("V_" ++ atom_to_list(Field)).
