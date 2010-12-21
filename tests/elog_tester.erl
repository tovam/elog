%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2010 Fernando Benavides <greenmellon@gmail.com>
%%% @doc Tester module for elog
%%% @end
%%%-------------------------------------------------------------------
-module(elog_tester).
-author('Fernando Benavides <greenmellon@gmail.com>').

-include("elog.hrl").

-export([log/3, start/4, stop/1]).

-spec start(pos_integer(), loglevel(), string(), [term()]) -> {ok, timer:tref()}.
start(Time, Level, Format, Args) ->
  timer:apply_interval(Time, ?MODULE, log, [Level, Format, Args]).

-spec stop(timer:tref()) -> {ok, cancel}.
stop(Timer) ->
  timer:cancel(Timer).

-spec log(loglevel(), string(), [term()]) -> ok.
log(?LOG_LEVEL_DEBUG, Format, Args) -> ?DEBUG(Format, Args);
log(?LOG_LEVEL_ERROR, Format, Args) -> ?ERROR(Format, Args);
log(?LOG_LEVEL_FATAL, Format, Args) -> ?FATAL(Format, Args);
log(?LOG_LEVEL_INFO, Format, Args) -> ?INFO(Format, Args);
log(?LOG_LEVEL_STAT, Format, Args) -> ?STAT(Format, Args);
log(?LOG_LEVEL_WARN, Format, Args) -> ?WARN(Format, Args).