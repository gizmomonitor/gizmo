%%% @doc Active session history API

-module(session_counter_api).
-author('mkorszun@gmail.com').

-export([active_sessions/4]).

%% ###############################################################
%% Types
%% ###############################################################

-type session_state() :: all_states | all | term().
-type active_sessions() :: integer() | list().
-type sw() :: integer() | undefined.
-type ew() :: integer() | undefined.

%% ###############################################################
%% API
%% ###############################################################

%% @doc Returns list of active sessions in given time period
-spec active_sessions(binary(), session_state(), sw(), ew()) -> {ok, active_sessions()} | {error, term()}.
active_sessions(Key, SessionState, Start, End) when is_integer(Start), is_integer(End) ->
    session_counter_stats:read(Key, SessionState, Start, End);
active_sessions(Key, SessionState, undefined, undefined) ->
    session_counter_gen:active_sessions(Key, SessionState).

%% ###############################################################
%% ###############################################################
%% ###############################################################