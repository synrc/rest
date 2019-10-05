-module(ts).
-include_lib("bpe/include/bpe.hrl").
-compile(export_all).
new() -> {ts,{{0,0,0},{0,0,0}}}.
to_json(#ts{time=X}) -> [{<<"time">>,lists:flatten(io_lib:format("~p.",[X]))}].
from_json([{<<"time">>,X}],_) -> #ts{time = rest:parse(X) };
from_json([{time,X}],_) -> #ts{time = rest:parse(X) }.


