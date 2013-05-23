-module(http_api_heartbeat).
-author('mkorszun@gmail.com').

-export([init/1, allowed_methods/2, check_path/1]).
-export([process_post/2]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include_lib("gizmo_backend_utils/include/logger.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% ###############################################################
%% CONTROL
%% ###############################################################

init([]) ->
    {ok, []}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

check_path(ReqData) ->
    case wrq:get_qs_value("device_id", ReqData) of
        undefined -> false;
        E when length(E) == 0 -> false;
        _ -> true
    end.

%% ###############################################################
%% RESOURCE
%% ###############################################################

%% ###############################################################
%% READ
%% ###############################################################

process_post(ReqData, State) ->
    Key = dict:fetch(application, wrq:path_info(ReqData)),
    Device = wrq:get_qs_value("device_id", ReqData),
    Timeout = wrq:get_qs_value("timeout", ReqData),
    case session_server_api:heartbeat(Key, Device, Timeout) of
        ok ->
            {true, ReqData, State};
        {error, application_not_found} ->
            ?ERR("Heartbeat for ~s/~s failed: application not found", [Key, Device]),
            http_api_error:error(application_not_found, 404, ReqData, State);
        {error, Reason} ->
            ?ERR("Heartbeat for ~s/~s failed: ~p", [Key, Device, Reason]),
            http_api_error:error(internal_error, 500, ReqData, State)
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################