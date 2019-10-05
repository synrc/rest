-module(timeout).
-include_lib("bpe/include/bpe.hrl").
-compile(export_all).
new() -> {timeout,{0,{0,0,0}}}.
to_json(#timeout{spec=X}) -> [{<<"timeout">>,iolist_to_binary(lists:flatten(io_lib:format("~p",[X])))}].
from_json([{<<"timeout">>,X}],_) -> #timeout{spec = rest:parse(X) };
from_json([{timeout,X}],_) -> #timeout{spec = rest:parse(X) }.
