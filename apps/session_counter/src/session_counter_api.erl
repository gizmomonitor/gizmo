-module(session_counter_api).
-author('mkorszun@gmail.com').

-export([add/2, active_sessions/3]).

-include_lib("utils/include/types.hrl").

%% ###############################################################
%% API
%% ###############################################################

%% @doc Updates session counter for given application
-spec add(string(), pid()) -> ok.
add(Key, SessionProcess) ->
    supervisor:start_child(session_counter_sup, [Key]),
    gen_server:cast(?L2A(Key), {add, SessionProcess}).

%% @doc Returns list of active sessions in given time period
-spec active_sessions(string(), integer() | undefined, integer() | undefined) ->
    {ok, integer()} | {ok, list()} | {error, term()}.
active_sessions(Key, Start, End) when is_integer(Start), is_integer(End) ->
    case application_obj:exists(Key) of
        true ->
            session_counter_stats:read(?L2B(Key), Start, End);
        false ->
            {error, application_not_found}
    end;

%% @doc Returns list of current active sessions
active_sessions(Key, _, _) ->
    case application_obj:exists(Key) of
        true ->
            try gen_server:call(?L2A(Key), get_counter) of
                Res -> {ok, Res}
            catch
                exit:{noproc, _} -> {ok, 0};
                _:Reason -> {error, Reason}
            end;
        false ->
            {error, application_not_found}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################