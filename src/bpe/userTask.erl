-module(userTask).
-include_lib("bpe/include/bpe.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-rest_record(userTask).
new() -> #userTask{}.
