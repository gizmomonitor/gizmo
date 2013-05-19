-module(application_obj).
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
-spec create(term()) -> binary().
create(Application) ->
    utils:db_execute(fun(Connection) ->
        Key = list_to_binary(uuid:to_string(uuid:uuid4())),
        App = riakc_obj:new(?BUCKET, Key, Application),
        case riakc_pb_socket:put(Connection, App, [return_head]) of
            {ok, Obj} ->
                {ok, riakc_obj:key(Obj)};
            {error, Error} ->
                {error, Error}
        end
    end, monitoring).

%% @doc Checks if given application exists
-spec exists(binary() | string()) -> true | false.
exists(Key) when is_list(Key) ->
    exists(list_to_binary(Key));
exists(Key) when is_binary(Key) ->
    utils:db_execute(fun(Connection) ->
        case read(Key, Connection) of
            {ok, _} ->
                true;
            {error, notfound} ->
                false;
            {error, Reason} ->
                {error, Reason}
        end
    end, monitoring).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

read(Key, Connection) ->
    case riakc_pb_socket:get(Connection, ?BUCKET, Key) of
        {ok, Obj} ->
            {ok, binary_to_term(riakc_obj:get_value(Obj))};
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################