# REST Framework for Erlang

A modern, REST framework for Erlang with clean separation of concerns.

## Features

- **Clean Architecture**: Separated into focused modules
- **Parse Transform**: Automatic JSON serialization/deserialization
- **HTTP Routing**: Built on Cowboy for high performance
- **Resource Behavior**: Well-defined callbacks for REST resources
- **Error Handling**: Comprehensive error handling and logging
- **Type Safety**: Extensive use of type specifications
- **Undefined Value Omission**: Undefined record fields are automatically omitted from JSON output
- **API Versioning**: Support for versioned APIs with flexible module mapping
- **KVS Integration**: Built-in support for KVS metainfo schemas

## Quick Start

1. **Build the project:**
   ```bash
   make compile
   ```

2. **Start the shell:**
   ```bash
   make shell
   ```

3. **Test the API:**
   ```bash
   # Versioned API calls
   curl http://localhost:8005/rest/v1/users
   curl http://localhost:8005/rest/v1/users/1
   curl http://localhost:8005/rest/v2/users
   
   # Legacy unversioned calls (defaults to v1)
   curl http://localhost:8005/rest/user_resource
   curl http://localhost:8005/rest/user_resource/1
   
   # Create new user (v1)
   curl -X POST http://localhost:8005/rest/v1/users \
        -H "Content-Type: application/json" \
        -d '{"name":"Bob Wilson","email":"bob@example.com"}'
   
   # Delete user
   curl -X DELETE http://localhost:8005/rest/v1/users/1
   ```

## Architecture

### Modules

- **rest_app**: OTP application module
- **rest_sup**: Top-level supervisor
- **rest_router**: HTTP routing configuration
- **rest_resource**: Behavior definition for REST resources
- **rest_json**: JSON serialization utilities
- **rest_transform**: Parse transform for code generation
- **rest_resource_handler**: HTTP request handler
- **rest_kvs_handler**: KVS operations handler

### Creating a Resource

1. **Define your record:**
   ```erlang
   -record(my_resource, {
       id :: binary(),
       name :: binary(),
       data :: term()
   }).
   ```

2. **Add the parse transform directive:**
   ```erlang
   -compile([{parse_transform, rest_transform}]).
   -rest_record(my_resource).
   ```

3. **Implement the behavior:**
   ```erlang
   -behaviour(rest_resource).
   -export([exists/1, get/0, get/1, post/1, delete/1, new/0]).
   ```

The parse transform will automatically generate `from_json/2` and `to_json/1` functions.

### Undefined Value Handling

The framework automatically omits `undefined` values from JSON output:

```erlang
% Record with optional fields
-record(user, {
    id :: binary(),
    name :: binary(),
    email :: binary(),
    phone :: binary() | undefined,      % Optional
    avatar_url :: binary() | undefined  % Optional
}).

% Example record
User = #user{
    id = <<"123">>,
    name = <<"John">>,
    email = <<"john@example.com">>,
    phone = <<"555-1234">>,
    avatar_url = undefined  % This field will be omitted
}.

% JSON output will be:
{
    "id": "123",
    "name": "John", 
    "email": "john@example.com",
    "phone": "555-1234"
    // avatar_url is omitted, not null
}
```

## Configuration

### Basic Configuration

Configure in `config/sys.config`:

```erlang
{rest, [
    {port, 8005},           % HTTP port
    {start_http, true}      % Start HTTP server
]}
```

### Advanced: Versioned API Configuration

The framework supports sophisticated API versioning and custom module mappings:

```erlang
{rest, [
    {port, 8005},
    {start_http, true},
    {modules, [
        {v1, [
            {users, user_rest_v1},      % Map /rest/v1/users to user_rest_v1 module
            {posts, post_rest_v1},      % Map /rest/v1/posts to post_rest_v1 module
            {comments, comment_resource} % Can mix naming conventions
        ]},
        {v2, [
            {users, user_rest_v2},      % Map /rest/v2/users to user_rest_v2 module
            {posts, post_rest_v2}       % Map /rest/v2/posts to post_rest_v2 module
        ]}
    ]}
]}
```

### URL Patterns

The framework supports these URL patterns:

1. **Versioned APIs**: `/rest/{version}/{resource}[/{id}]`
   - `/rest/v1/users` → `user_rest_v1` module (from config)
   - `/rest/v2/users` → `user_rest_v2` module (from config)

2. **Legacy unversioned**: `/rest/{resource}[/{id}]` 
   - `/rest/users` → defaults to v1, maps to `user_rest_v1`
   - `/rest/user_resource` → direct module name mapping

3. **Fallback naming**: If not in config, tries these patterns:
   - `{resource}_rest_{version}` (e.g., `users_rest_v1`)
   - `{resource}_resource` (e.g., `users_resource`)

### Module Resolution Priority

1. **Explicit configuration** in `modules` section
2. **Default naming**: `{resource}_rest_{version}`
3. **Simple naming**: `{resource}_resource`
4. **Error**: Module not found

## Improvements from Original

1. **Separated Concerns**: Split monolithic module into focused modules
2. **Better Error Handling**: Comprehensive try-catch blocks and proper error responses
3. **Type Safety**: Added type specifications throughout
4. **Documentation**: Extensive documentation and examples
5. **Modern Practices**: Uses maps instead of property lists where appropriate
6. **Configuration**: Flexible configuration system
7. **Logging**: Proper error logging and monitoring
8. **Testing**: Structure prepared for unit and integration tests
9. **Undefined Value Omission**: Clean JSON output by omitting undefined fields
10. **KVS Integration**: Enhanced metainfo support with fallback mechanisms

## License

MIT License
