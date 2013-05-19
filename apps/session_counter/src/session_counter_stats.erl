-module(session_counter_stats).
-author('mkorszun@gmail.com').

-export([save/3, read/3]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include_lib("utils/include/types.hrl").

%% ###############################################################
%% MACROS
%% ###############################################################

-define(BUCKET(Key), <<"time_metrics/", Key/binary>>).

%% ###############################################################
%% API
%% ###############################################################

%% @doc Saves session count at given timestamp for given application
-spec save(binary(), binary(), integer()) -> ok.
save(Key, Timestamp, Count) ->
    utils:db_execute(fun(Connection) ->
        Obj = riakc_obj:new(?BUCKET(Key), Timestamp, Count),
        riakc_pb_socket:put(Connection, Obj)
    end, monitoring).

%% @doc Reads sessions count in time period for given application
-spec read(binary(), integer(), integer()) -> {ok, list()}.
read(Key, Start, End) ->
    Map = {map, {qfun, fun(O,_,_) -> map(O, Start, End) end}, none, true},
    utils:db_execute(fun(Connection) ->
        riakc_pb_socket:mapred_bucket(Connection, ?BUCKET(Key),[Map])
    end, stats, fun result/1).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

result({ok, [{0, Results}]}) -> {ok, Results};
result({ok, []}) -> {ok, []}.

map(Object, Start, End) ->
    case list_to_integer(binary_to_list(riak_object:key(Object))) of
        T when T >= Start andalso T =< End ->
            [{struct, [{T, ?B2T(riak_object:get_value(Object))}]}];
        _ ->
            []
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################