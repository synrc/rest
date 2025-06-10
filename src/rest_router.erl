%% @doc REST Framework Router
%% HTTP routing configuration for the REST framework with versioning support
-module(rest_router).

-export([get_routes/0, resolve_module/2]).

%% @doc Get the compiled routes for Cowboy
-spec get_routes() -> cowboy_router:dispatch_rules().
get_routes() ->
    cowboy_router:compile([
        {'_', [
            %% {"/rest/kvs/0/[...]", rest_kvs_handler, []},
            %% {"/rest/kvs/1/:id/[...]", rest_kvs_handler, []},
            % Versioned API routes
            {"/rest/:version/:resource", rest_resource_handler, []},
            {"/rest/:version/:resource/:id", rest_resource_handler, []}
        ]}
    ]).

%% @doc Resolve module name from version and resource configuration
-spec resolve_module(binary() | atom(), binary()) -> {ok, module()} | {error, not_found}.
resolve_module(Version, Resource) when is_binary(Version) ->
    resolve_module(binary_to_atom(Version, utf8), Resource);
resolve_module(Version, Resource) when is_binary(Resource) ->
    resolve_module(Version, binary_to_atom(Resource, utf8));
resolve_module(Version, Resource) when is_atom(Version), is_atom(Resource) ->
    case application:get_env(rest, modules) of
        {ok, ModulesConfig} ->
            resolve_from_config(Version, Resource, ModulesConfig);
        undefined ->
            %% Fallback to default naming convention
            resolve_default_module(Version, Resource)
    end.

%% @private
%% Resolve module from configuration
resolve_from_config(Version, Resource, ModulesConfig) ->
    case proplists:get_value(Version, ModulesConfig) of
        undefined ->
            {error, version_not_found};
        VersionConfig ->
            case proplists:get_value(Resource, VersionConfig) of
                undefined ->
                    %% Try default naming within version
                    DefaultModule = list_to_atom(
                        atom_to_list(Resource) ++ "_rest_" ++ atom_to_list(Version)
                    ),
                    case module_exists(DefaultModule) of
                        true -> {ok, DefaultModule};
                        false -> {error, not_found}
                    end;
                Module when is_atom(Module) ->
                    case module_exists(Module) of
                        true -> {ok, Module};
                        false -> {error, module_not_loaded}
                    end
            end
    end.

%% @private
%% Fallback to default module naming convention
resolve_default_module(Version, Resource) ->
    %% Try: resource_rest_version (e.g., user_rest_v1)
    DefaultModule = list_to_atom(
        atom_to_list(Resource) ++ "_rest_" ++ atom_to_list(Version)
    ),
    case module_exists(DefaultModule) of
        true ->
            {ok, DefaultModule};
        false ->
            %% Try: resource_resource (e.g., user_resource)
            SimpleModule = list_to_atom(atom_to_list(Resource) ++ "_resource"),
            case module_exists(SimpleModule) of
                true -> {ok, SimpleModule};
                false -> {error, not_found}
            end
    end.

%% @private
%% Check if module exists and is loaded
module_exists(Module) ->
    try
        _ = Module:module_info(),
        true
    catch
        error:undef -> false
    end.
