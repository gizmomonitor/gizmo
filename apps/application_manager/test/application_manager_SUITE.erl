-module(application_manager_SUITE).
-author('mkorszun@gmail.com').

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%% ###############################################################
%% CT CALLBACKS
%% ###############################################################

all() -> [
    test_create,
    test_exists_true,
    test_exists_false
].

init_per_suite(Config) ->
    ok = application:start(meck),
    Config.

end_per_suite(_) ->
    ok = application:stop(meck).

%% ###############################################################
%% TESTS
%% ###############################################################

test_create(_) ->
    UUID = uuid:uuid4(),
    Key = list_to_binary(uuid:to_string(UUID)),
    meck:new(uuid, [passthrough]),
    meck:new(riakc_pb_socket),
    meck:new(gizmo_backend_utils),
    meck:expect(uuid, uuid4, fun() -> UUID end),
    meck:expect(gizmo_backend_utils, db_execute, fun(F, _) -> F(conn) end),
    meck:expect(riakc_pb_socket, put, fun(_, A, _) -> {ok, A} end),
    {ok, Key} = application_manager:create([]),
    true = meck:validate(uuid),
    true = meck:validate(gizmo_backend_utils),
    true = meck:validate(riakc_pb_socket),
    meck:unload(uuid),
    meck:unload(gizmo_backend_utils),
    meck:unload(riakc_pb_socket).

test_exists_true(_) ->
    meck:new(gizmo_backend_utils),
    meck:new(riakc_pb_socket),
    meck:expect(gizmo_backend_utils, db_execute, fun(F, _) -> F(conn) end),
    meck:expect(riakc_pb_socket, get, fun(_, _, _) -> {ok, object} end),
    true = application_manager:exists(<<"Key">>),
    true = meck:validate(gizmo_backend_utils),
    true = meck:validate(riakc_pb_socket),
    meck:unload(gizmo_backend_utils),
    meck:unload(riakc_pb_socket).

test_exists_false(_) ->
    meck:new(gizmo_backend_utils),
    meck:new(riakc_pb_socket),
    meck:expect(gizmo_backend_utils, db_execute, fun(F, _) -> F(conn) end),
    meck:expect(riakc_pb_socket, get, fun(_, _, _) -> {error, notfound} end),
    false = application_manager:exists(<<"Key">>),
    true = meck:validate(gizmo_backend_utils),
    true = meck:validate(riakc_pb_socket),
    meck:unload(gizmo_backend_utils),
    meck:unload(riakc_pb_socket).

%% ###############################################################
%% ###############################################################
%% ###############################################################