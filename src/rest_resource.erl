%% @doc REST Resource Behavior
%% Defines the callback functions that REST resources must implement
-module(rest_resource).

-export([behaviour_info/1]).

%% @doc Behavior callback information
-spec behaviour_info(callbacks) -> [{atom(), arity()}];
                    (term()) -> undefined.
behaviour_info(callbacks) ->
    [
        {exists, 1},      % Check if resource exists
        {get, 0},         % Get all resources
        {get, 1},         % Get resource by ID
        {post, 1},        % Create new resource
        {delete, 1},      % Delete resource
        {from_json, 2},   % Convert from JSON to record
        {to_json, 1}      % Convert from record to JSON
    ];
behaviour_info(_) ->
    undefined.
