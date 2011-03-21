%%%-------------------------------------------------------------------
%%% @hidden
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc elog main supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(elog_sup).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(supervisor).

-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

%% API
-export([start_link/0, reload/0, add_exception/1]).

%% Supervisor callbacks
-export([init/1]).

%% @headerfile "elog.hrl"
-include("elog.hrl").

%% ===================================================================
%% API functions
%% ===================================================================
%% @hidden
-spec start_link() -> startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @hidden
-spec add_exception(elog:loglevel()) -> {ok, pid()} | {error, startlink_err()}.
add_exception(Level) ->
  Name = list_to_atom("exc-" ++ atom_to_list(Level)),
  ChildSpec =
    {Name, {elogger, start_link, [Level, just_exceptions]},
     transient, brutal_kill, worker, [elogger]},
  case supervisor:start_child(?MODULE, ChildSpec) of
    {error, already_present} ->
      supervisor:restart_child(?MODULE, Name);
    Other ->
      Other
  end.

%% @hidden
-spec reload() -> ok.
reload() ->
  ok = lists:foreach(fun({ChildId, _, _, _}) ->
                             ok = supervisor:terminate_child(?MODULE, ChildId)
                     end, supervisor:which_children(?MODULE)),
  lists:foreach(
    fun(LogLevel) ->
            io:format("elog Restarting ~p: ~p~n",
                      [LogLevel, supervisor:restart_child(?MODULE, LogLevel)])
    end, ?LOG_LEVELS).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%% @hidden
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
  Children =
    [{LogLevel, {elogger, start_link, [LogLevel]},
      transient, brutal_kill, worker, [elogger]} || LogLevel <- ?LOG_LEVELS],
  error_logger:info_msg("Inititalizing elog supervisor...~n"),
  {ok, {{one_for_one, 5, 10}, Children}}.