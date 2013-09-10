%%% @doc Session stats db API

-module(session_counter_stats).
-author('mkorszun@gmail.com').

-export([save/3, read/4]).

%% ###############################################################
%% MACROS
%% ###############################################################

-define(BUCKET(Key), <<"time_metrics/", Key/binary>>).

%% ###############################################################
%% API
%% ###############################################################

%% @doc Saves session count at given timestamp for given application
-spec save(binary(), binary(), list()) -> ok.
save(Key, Timestamp, Count) ->
    gizmo_backend_utils:db_execute(
        fun(Connection) ->
            Obj = riakc_obj:new(?BUCKET(Key), Timestamp, Count),
            riakc_pb_socket:put(Connection, Obj)
        end, monitoring).

%% @doc Reads sessions count in given state and time window for given application
-spec read(binary(), term(), integer(), integer()) -> {ok, list()}.
read(Key, SessionState, Start, End) ->
    Map = {map, {qfun, fun(O,_,_) -> map(O, SessionState, Start, End) end}, none, true},
    gizmo_backend_utils:db_execute(
        fun(Connection) ->
            riakc_pb_socket:mapred_bucket(Connection, ?BUCKET(Key),[Map])
        end, stats, fun result/1).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

result({ok, [{0, Results}]}) -> {ok, Results};
result({ok, []}) -> {ok, []};
result({error, Error}) -> {error, Error}.

map(Object, SessionState, Start, End) ->
    case list_to_integer(binary_to_list(riak_object:key(Object))) of
        T when T >= Start andalso T =< End ->
            O = binary_to_term(riak_object:get_value(Object)),
            case SessionState of
                undefined ->
                    [{struct, [{T, {struct, O}}]}];
                _ ->
                    [{struct, [{T, proplists:get_value(SessionState, O)}]}]
            end;
        _ ->
            []
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################