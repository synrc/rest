REST: framework with typed JSON
===============================

[![Actions Status](https://github.com/synrc/rest/workflows/mix/badge.svg)](https://github.com/synrc/rest/actions)
[![Build Status](https://travis-ci.org/synrc/rest.svg?branch=master)](https://travis-ci.org/synrc/rest)
[![Hex pm](http://img.shields.io/hexpm/v/rest.svg?style=flat)](https://hex.pm/packages/rest)

Features and Goals
------------------

* Fastest possibe Record <-> Proplists transformations
* Smallest REST framework in the world
* ETS/KVS/Any storage selection by scaffolding

We've achived first goal by providing parse_transform code generation
for tuple transformations. And second requirement was achieved
by not including routing bullshit and other uncertain features.

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

```sh
$ curl -i -X POST -d "id=vlad" localhost:8005/rest/users
$ curl -i -X POST -d "id=doxtop" localhost:8005/rest/users
$ curl -i -X GET localhost:8005/rest/users
$ curl -i -X PUT -d "id=5HT" localhost:8005/rest/users/vlad
$ curl -i -X GET localhost:8005/rest/users/5HT
$ curl -i -X DELETE localhost:8005/rest/users/5HT
```

Credits
-------

* Dmitry Bushmelev — ETS
* Maxim Sokhatsky — KVS

OM A HUM
