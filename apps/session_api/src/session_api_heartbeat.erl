%%% @doc Heartbeat resource

-module(session_api_heartbeat).
-author('mkorszun@gmail.com').

-export([init/3, allowed_methods/2, content_types_provided/2]).
-export([resource_info/0]).
-export([resource_exists/2, heartbeat/2]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include_lib("gizmo_backend_utils/include/logger.hrl").
-include_lib("gizmo_backend_utils/include/types.hrl").

%% ###############################################################
%% CONTROL
%% ###############################################################

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"*">>, <<"*">>, []}, heartbeat}], Req, State}.

resource_info() ->
    Path = "/application/:application/device/:device/heartbeat",
    Constraints = [{device, function,
        fun(V) ->
            {true, ?B2A(<<<<"device">>/binary, V/binary>>)}
        end}],
    {Path, Constraints, ?MODULE, []}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

resource_exists(ReqData, State) ->
    session_api_utils:application_exists(ReqData, State).

heartbeat(ReqData, State) ->
    {Key, _} = cowboy_req:binding(application, ReqData),
    {Device, _} = cowboy_req:binding(device, ReqData),
    {Timeout, _} = cowboy_req:qs_val(<<"timeout">>, ReqData, undefined),
    {DeviceState, _} = cowboy_req:qs_val(<<"state">>, ReqData, undefined),
    session_heartbeat_fsm:heartbeat(Key, Device, ?B2I(Timeout), DeviceState),
    {halt, session_api_utils:no_content(ReqData), State}.

%% ###############################################################
%% ###############################################################
%% ###############################################################