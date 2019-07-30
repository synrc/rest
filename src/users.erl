-module(users).
-compile({parse_transform, rest}).
-record(user, {id,cn,name,type}).
-export([init/0, populate/1, exists/1, get/0, get/1, post/1, delete/1]).
-rest_record(user).

init()               -> ets:new(users, [public, named_table, {keypos, #user.id}]).
populate(Users)      -> ets:insert(users, Users).
exists(Id)           -> X = ets:member(users, binary_to_list(Id)), io:format("Member: ~p~n",[X]), X.
get()                -> ets:tab2list(users).
get(Id)              -> #user{id=Id}.
delete(Id)           -> ets:delete(users, binary_to_list(Id)).
post(#user{} = User) -> ets:insert(users, User);
post(Data)           -> post(from_json(Data, #user{})).
