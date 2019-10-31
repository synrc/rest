-module('Payment').
-include_lib("erp/include/payment.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-rest_record('Payment').
new() -> #'Payment'{type=fiat}.
