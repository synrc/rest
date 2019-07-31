-module('Organization').
-include_lib("erp/include/organization.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-rest_record('Organization').
new() -> #'Organization'{}.
