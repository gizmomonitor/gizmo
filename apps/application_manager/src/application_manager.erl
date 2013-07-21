%%% @doc Application management API

-module(application_manager).
-author('mkorszun@gmail.com').

-export([create/1, exists/1]).

%% ###############################################################
%% MACROS
%% ###############################################################

-define(BUCKET, <<"applications">>).

%% ###############################################################
%% API
%% ###############################################################

%% @doc Creates new application
-spec create(term()) -> {ok, binary()} | {error, term()}.
create(Application) ->
    gizmo_backend_utils:db_execute(
        fun(Conn) ->
            Key = list_to_binary(uuid:to_string(uuid:uuid4())),
            App = riakc_obj:new(?BUCKET, Key, Application),
            {ok, Obj} = riakc_pb_socket:put(Conn, App, [return_head]),
            {ok, riakc_obj:key(Obj)}
        end,
    monitoring).

%% @doc Checks if given application exists
-spec exists(binary()) -> true | false.
exists(Key) ->
    gizmo_backend_utils:db_execute(
        fun(Conn) ->
            case riakc_pb_socket:get(Conn, ?BUCKET, Key) of
                {ok, _} ->
                    true;
                {error, notfound} ->
                    false
            end
        end,
    monitoring).

%% ###############################################################
%% ###############################################################
%% ###############################################################