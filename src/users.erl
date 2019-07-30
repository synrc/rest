-module(users).
-compile({parse_transform, rest}).
-record(user, {id,cn,name,type}).
-export([init/0, populate/1, exists/1, get/0, get/1, post/1, delete/1]).
-rest_record(user).

init()               -> ets:new(users, [public, named_table, {keypos, #user.id}]).
populate(Users)      -> ets:insert(users, Users).
exists(Id)           -> ets:member(users, n2o:to_binary(Id)).
get()                -> ets:tab2list(users).
get(Id)              -> #user{id=Id}.
delete(Id)           -> ets:delete(users, n2o:to_binary(Id)).
post(#user{} = User) -> ets:insert(users, User);
post(Data)           -> post(from_json(Data, #user{})).
