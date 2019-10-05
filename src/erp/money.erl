-module(money).
-include_lib("dec/include/dec.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-rest_record(money).
new() -> #money{}.
