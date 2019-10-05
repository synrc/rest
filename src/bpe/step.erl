-module(step).
-include_lib("bpe/include/bpe.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-rest_record(step).
new() -> #step{}.

uri({step,No,Proc}) when is_integer(Proc) -> {step,No,integer_to_list(Proc)};
uri(X) -> X.
