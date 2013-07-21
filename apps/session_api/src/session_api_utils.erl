%%% @doc Utils

-module(session_api_utils).
-author('mkorszun@gmail.com').

-export([to_json/2, to_error/2, to_error/3, no_content/1, application_exists/2]).

%% ###############################################################
%% UTILS
%% ###############################################################

to_json(Content, ReqData) ->
    cowboy_req:set_resp_header(
        <<"content-type">>, <<"application/json">>,
        cowboy_req:set_resp_body(Content, ReqData)).

to_error(Reason, Code, ReqData) ->
    Body = mochijson2:encode({struct, [{error, Reason}]}),
    {ok, ReqData1} = cowboy_req:reply(Code, [{<<"content-type">>, <<"application/json">>}], Body, ReqData),
    ReqData1.

to_error(Reason, ReqData) ->
    Body = mochijson2:encode({struct, [{error, Reason}]}),
    to_json(Body, ReqData).

no_content(ReqData) ->
    {ok, ReqData1} = cowboy_req:reply(204, ReqData),
    ReqData1.

application_exists(ReqData, State) ->
    {Key, _} = cowboy_req:binding(application, ReqData),
    case application_manager:exists(Key) of
        true ->
            {true, ReqData, Key};
        false ->
            {false, to_error(application_not_found, ReqData), State}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################