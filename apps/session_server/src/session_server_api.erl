-module(session_server_api).
-author('mkorszun@gmail.com').

-export([heartbeat/3]).

%% ###############################################################
%% MACROS
%% ###############################################################

-include_lib("gizmo_backend_utils/include/types.hrl").

%% ###############################################################
%% API
%% ###############################################################

%% @doc Sends heartbeat to given device process
-spec heartbeat(string(), string(), string() | undefined) -> ok | {error, term()}.
heartbeat(ApplicationKey, DeviceId, Timeout) ->
    case application_obj:exists(?L2B(ApplicationKey)) of
        true ->
            do_heartbeat(ApplicationKey, ?L2A(DeviceId), ?L2I(Timeout));
        false ->
            {error, application_not_found};
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

do_heartbeat(ApplicationKey, DeviceId, Timeout) ->
    case ensure_session_started(ApplicationKey, DeviceId, Timeout) of
        {ok, running} ->
            gen_fsm:send_event(DeviceId, {heartbeat, Timeout});
        {error, Error} ->
            {error, Error}
    end.

ensure_session_started(ApplicationKey, DeviceId, Timeout) ->
    case session_server_sup:start_child(ApplicationKey, DeviceId, Timeout) of
        {ok, Pid} -> {ok, running};
        {error, {already_started, _}} -> {ok, running};
        {error, Error} -> {error, Error}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################