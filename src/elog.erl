%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2010 Fernando Benavides <greenmellon@gmail.com>
%%% @doc Yet another logging library for Erlang
%%% @end
%%%-------------------------------------------------------------------
-module(elog).
-author('Fernando Benavides <greenmellon@gmail.com>').

%% @headerfile "elog.hrl"
-include("elog.hrl").

-export([start/0]).
-export([level/1, level/2]).

%%% @doc  Starts the application
%%% @spec start() -> ok
-spec start() -> ok.
start() ->
  application:start(elog).

%%% @doc  Sets the global log level
%%% @spec level(loglevel()) -> ok
-spec level(loglevel()) -> ok.
level(Level) ->
  application:set_env(elog, log_level, Level),
  elog_sup:reload().

%%% @doc  Sets the log level for a particular module or regular expression
%%% @spec level(loglevel(), atom() | string()) -> ok
-spec level(loglevel(), atom() | string()) -> ok.
level(Level, ModuleOrRegExp) ->
  _ = lists:foldl(
        fun(L, reached) ->
                try elogger:add(L, ModuleOrRegExp)
                catch
                  _:{noproc, _} ->
                    {ok, _Pid} =
                      elogger:start_link(L, just_exceptions),
                    elogger:add(L, ModuleOrRegExp)
                end,
                reached;
           (L, not_reached) ->
                case L of
                  Level ->
                    try elogger:add(L, ModuleOrRegExp)
                    catch
                      _:{noproc, _} ->
                        {ok, _Pid} = elogger:start_link(Level, just_exceptions),
                        elogger:add(L, ModuleOrRegExp)
                    end,
                    reached;
                  _ ->
                    catch elogger:remove(L, ModuleOrRegExp),
                    not_reached
                end
        end, not_reached, ?LOG_LEVELS),
  ok.