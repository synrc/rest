-module(tx).
-include_lib("bpe/include/doc.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-rest_record(tx).
new() -> #tx{}.
