REST: framework with typed JSON
===============================

Usage
-----

Just plug REST endpoint directly to your Cowboy router:

```erlang
{"/rest/:resource", rest_cowboy, []},
{"/rest/:resource/:id", rest_cowboy, []},
```

Module
------

Sample REST service implementation:

```erlang
-module(users).
-behaviour(rest).
-compile({parse_transform, rest}).
-include("users.hrl").
-export([init/0, populate/1, exists/1, get/0, get/1, post/1, delete/1]).
-rest_record(user).

init() -> ets:new(users, [public, named_table, {keypos, #user.id}]).
populate(Users) -> ets:insert(users, Users).
exists(Id) -> ets:member(users, wf:to_list(Id)).
get() -> ets:tab2list(users).
get(Id) -> [User] = ets:lookup(users, wf:to_list(Id)), User.
delete(Id) -> ets:delete(users, wf:to_list(Id)).
post(#user{} = User) -> ets:insert(users, User);
post(Data) -> post(from_json(Data, #user{})).
```

Usage
-----

    curl -i -X POST -d "id=vlad" localhost:8000/rest/users
    curl -i -X POST -d "id=doxtop" localhost:8000/rest/users
    curl -i -X GET localhost:8000/rest/users
    curl -i -X PUT -d "id=5HT" localhost:8000/rest/users/vlad
    curl -i -X GET localhost:8000/rest/users/5HT
    curl -i -X DELETE localhost:8000/rest/users/5HT

Credits
-------

* Dmitry Bushmelev

OM A HUM
