-module(http_api_active_sessions).
-author('mkorszun@gmail.com').

-export([init/1, allowed_methods/2, content_types_provided/2]).
-export([to_json/2]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include_lib("utils/include/logger.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% ###############################################################
%% CONTROL
%% ###############################################################

init([]) ->
    {ok, []}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

%% ###############################################################
%% READ
%% ###############################################################

to_json(ReqData, State) ->
    Key = dict:fetch(application, wrq:path_info(ReqData)),
    Start = to_int(wrq:get_qs_value("start", ReqData)),
    End = to_int(wrq:get_qs_value("end", ReqData)),
    case session_counter_api:active_sessions(Key, Start, End) of
        {ok, Res} when is_integer(Res) ->
            {mochijson2:encode([{active_sessions, Res}]), ReqData, State};
        {ok, Res} when is_list(Res) ->
            {mochijson2:encode([{active_sessions, {array, Res}}]), ReqData, State};
        {error, application_not_found} ->
            ?ERR("Read active sessions for ~s: key not found", [Key]),
            http_api_error:error(application_not_found, 404, ReqData, State);
        {error, Reason} ->
            ?ERR("Read active sessions for ~s: ~p", [Key, Reason]),
            http_api_error:error(internal_error, 500, ReqData, State)
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

to_int(undefined) ->
    undefined;
to_int(E) when is_list(E) ->
    try list_to_integer(E) catch _:_ -> undefined end.

%% ###############################################################
%% ###############################################################
%% ###############################################################