-module(messageEvent).
-include_lib("bpe/include/bpe.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-rest_record(messageEvent).
new() -> #messageEvent{}.
