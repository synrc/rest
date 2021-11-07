-module(rest).

-author('Dmitry Bushmelev').

-behaviour(application).

-behaviour(supervisor).

-export([init/1, start/2, stop/1]).

-export([behaviour_info/1,
         parse_transform/2,
         generate_to_json/3,
         binarize/1,
         generate_from_json/3,
         from_json/2,
         to_json/1,
         to_binary/1,
         parse/1,
         atomize/1]).

stop(_State) -> ok.

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    users:init(),
    kvs:join(),
    cowboy:start_clear(http,
                       [{port, application:get_env(n2o, port, 8005)}],
                       #{env => #{dispatch => points()}}),
    {ok, {{one_for_one, 5, 10}, []}}.

points() ->
    cowboy_router:compile([{'_',
                            [{"/rest/kvs/0/[...]", rest_kvs, []},
                             {"/rest/kvs/1/:id/[...]", rest_kvs, []},
                             {"/rest/:resource", rest_cowboy, []},
                             {"/rest/:resource/:id", rest_cowboy, []}]}]).

behaviour_info(callbacks) ->
    [{exists, 1},
     {get, 0},
     {get, 1},
     {post, 1},
     {delete, 1},
     {from_json, 2},
     {to_json, 1}];
behaviour_info(_) -> undefined.

parse_transform(Forms, _Options) ->
    RecordName = rest_record(Forms),
    RecordFields = record_fields(RecordName, Forms),
    Forms1 = generate({from_json, 2},
                      RecordName,
                      RecordFields,
                      Forms),
    Forms2 = generate({to_json, 1},
                      RecordName,
                      RecordFields,
                      Forms1),
    Forms2.

rest_record([]) -> [];
rest_record([{attribute, _, rest_record, RecordName}
             | _Forms]) ->
    RecordName;
rest_record([_ | Forms]) -> rest_record(Forms).

record_field({record_field, _, {atom, _, Field}},
             _cordName) ->
    %  io:format("Case 1: ~p~n",[Field]),
    Field;
record_field({record_field, _, {atom, _, Field}, Type},
             _cordName) ->
    %  io:format("Case 2: ~p~n",[Field]),
    Field;
record_field({typed_record_field,
              {record_field, _, {atom, _, Field}, _},
              Type},
             RecordName) ->
    Rec = allow(Type),
    put({RecordName, Field}, Rec),
    %  case Rec of
    %    undefined -> io:format("Case 3: ~p~n",[Field]);
    %            _ -> io:format("Case 3: ~p, Link: ~p~n",[Field,Rec]) end,
    Field.

allow({type, _, union, Components}) ->
    findType(Components);
allow(Type) -> findType([Type]).

findType([]) -> undefined;
findType([{type, _, record, [{atom, _, X}]} | T]) -> X;
findType([{remote_type,
           _,
           [{atom, _, _}, {atom, _, X}, _]}
          | T]) ->
    X;
findType([H | T]) ->
    %  io:format("Unknown Type: ~p~n",[H]),
    findType(T).

record_fields(RecordName,
              [{attribute, _, record, {RecordName, Fields}}
               | _Forms]) ->
    [record_field(Field, RecordName) || Field <- Fields];
record_fields(RecordName, [_ | Forms]) ->
    record_fields(RecordName, Forms);
record_fields(__cordName, []) -> [].

last_export_line(Exports) ->
    case lists:reverse(Exports) of
        [{_, Line, _, _} | _] -> Line;
        _ -> 0
    end.

generate({FunName, _Arity} = Fun, Record, Fields,
         Forms) ->
    Exports = lists:filter(fun ({attribute,
                                 _,
                                 export,
                                 _}) ->
                                   true;
                               (_) -> false
                           end,
                           Forms),
    case exported(Fun, Exports) of
        true -> Forms;
        false ->
            Line = last_export_line(Exports),
            Gen = list_to_atom("generate_" ++
                                   atom_to_list(FunName)),
            lists:flatten([(?MODULE):Gen(export(Form, Fun, Line),
                                         Record,
                                         Fields)
                           || Form <- Forms])
    end.

exported(Fun, Exports) ->
    lists:member(Fun,
                 lists:flatten([E
                                || {attribute, _, export, E} <- Exports])).

field_var(Field) ->
    list_to_atom("V_" ++ atom_to_list(Field)).

from_json_prelude(Line) ->
    {clause,
     Line,
     [{nil, Line}, {var, Line, 'Acc'}],
     [],
     [{var, Line, 'Acc'}]}.

from_json_coda(Line) ->
    {clause,
     Line,
     [{cons, Line, {var, Line, '_'}, {var, Line, 'Json'}},
      {var, Line, 'Acc'}],
     [],
     [{call,
       Line,
       {atom, Line, from_json},
       [%     {var, Line, 'Json'} % here is fix for recursive binarized preprocessing to raw X:from_json
        {call,
         Line,
         {remote,
          Line,
          {atom, Line, ?MODULE},
          {atom, Line, binarize}},
         [{var, Line, 'Json'}]},
        {var, Line, 'Acc'}]}]}.

from_json_clauses(_, _, []) -> [];
from_json_clauses(Line, Record, [Field | Fields]) ->
    [{clause,
      Line,
      [{cons,
        Line,
        {tuple,
         Line,
         [{bin,
           Line,
           [{bin_element,
             Line,
             {string, Line, atom_to_list(Field)},
             default,
             default}]},
          {var, Line, field_var(Field)}]},
        {var, Line, 'Json'}},
       {var, Line, 'Acc'}],
      [],
      [{call,
        Line,
        {atom, Line, from_json},
        [{var, Line, 'Json'},
         {record,
          Line,
          {var, Line, 'Acc'},
          Record,
          [{record_field,
            Line,
            {atom, Line, Field},
            {call,
             Line,
             {remote,
              Line,
              {atom, Line, ?MODULE},
              {atom, Line, from_json}},
             [{var, Line, field_var(Field)},
              {atom,
               Line,
               case get({Record, Field}) of
                   undefined -> Record;
                   FieldType -> FieldType
               end}]}}]}]}]}
     | from_json_clauses(Line, Record, Fields)].

generate_from_json({eof, Line}, Record, Fields) ->
    [{function,
      Line,
      from_json,
      2,
      [from_json_prelude(Line)] ++
          from_json_clauses(Line, Record, Fields) ++
              [from_json_coda(Line)]},
     {eof, Line + 1}];
generate_from_json(Form, _, _) -> Form.

export({attribute, LastExportLine, export, Exports},
       Fun, LastExportLine) ->
    {attribute, LastExportLine, export, [Fun | Exports]};
export(Form, _, _) -> Form.

to_json_cons(Line, []) -> {nil, Line};
to_json_cons(Line, [Field | Fields]) ->
    {cons,
     Line,
     {tuple,
      Line,
      [{atom, Line, Field},
       {call,
        Line,
        {remote,
         Line,
         {atom, Line, ?MODULE},
         {atom, Line, to_json}},
        [{var, Line, field_var(Field)}]}]},
     to_json_cons(Line, Fields)}.

generate_to_json({eof, Line}, Record, Fields) ->
    [{function,
      Line,
      to_json,
      1,
      [{clause,
        Line,
        [{record,
          Line,
          Record,
          [{record_field,
            Line,
            {atom, Line, F},
            {var, Line, field_var(F)}}
           || F <- Fields]}],
        [],
        [to_json_cons(Line, Fields)]}]},
     {eof, Line + 1}];
generate_to_json(Form, _, _) -> Form.

from_json(<<Data/binary>>, _) -> binary_to_list(Data);
from_json({struct, Props}, X) -> from_json(Props, X);
from_json([{Key, _} | _] = Props, X)
    when Key =/= struct ->
    X:from_json(binarize(Props), X:new());
from_json(Any, X) -> Any.

atomize([{Key, _} | _] = Props) when Key =/= struct ->
    lists:map(fun ({K, V}) when is_atom(K) -> {K, V};
                  ({K, V}) when is_binary(K) ->
                      {list_to_existing_atom(binary_to_list(K)), V}
              end,
              Props);
atomize(X) -> X.

binarize([{Key, _} | _] = Props) when Key =/= struct ->
    lists:map(fun ({K, V}) when is_atom(K) ->
                      {list_to_binary(atom_to_list(K)), allowed_value(V)};
                  ({K, V}) when is_binary(K) -> {K, allowed_value(V)}
              end,
              Props);
binarize(X) -> X.

allowed_value(X) when is_reference(X) -> [];
allowed_value(X) -> X.

to_json(X) when is_tuple(X) ->
    Module = hd(tuple_to_list(X)),
    Module:to_json(X);
to_json(Data) ->
    case is_string(Data) of
        true -> rest:to_binary(Data);
        false -> json_match(Data)
    end.

json_match([{_, _} | _] = Props) ->
    [{rest:to_binary(Key), to_json(Value)}
     || {Key, Value} <- Props];
json_match([_ | _] = NonEmptyList) ->
    [to_json(X) || X <- NonEmptyList];
json_match(Any) -> Any.

is_char(C) ->
    is_integer(C) andalso C >= 0 andalso C =< 255.

is_string([N | _] = PossibleString) when is_number(N) ->
    lists:all(fun is_char/1, PossibleString);
is_string(_) -> false.

to_binary(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
to_binary(B) when is_binary(B) -> B;
to_binary(I) when is_integer(I) ->
    to_binary(integer_to_list(I));
to_binary(F) when is_float(F) ->
    float_to_binary(F, [{decimals, 9}, compact]);
to_binary(L) when is_list(L) -> iolist_to_binary(L).

parse(String) when is_binary(String) ->
    parse(binary_to_list(String));
parse(String) ->
    {ok, Tokens, _EndLine} = erl_scan:string(String ++ "."),
    {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
    {value, Value, _Bs} = erl_eval:exprs(AbsForm,
                                         erl_eval:new_bindings()),
    Value.
