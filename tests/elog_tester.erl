%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc Tester module for elog
%%% @end
%%%-------------------------------------------------------------------
-module(elog_tester).
-author('Fernando Benavides <greenmellon@gmail.com>').

-include("elog.hrl").

-export([log/3, start/4, stop/1, start/0, main/0]).

-define(SLEEP, 1000).

-spec main() -> ok.
main() ->
  start(),
  timer:sleep(?SLEEP),
  halt(0).

-spec start() -> ok.
start() ->
  ok = elog:level(debug),
  io:format("~n------------------------------------------------~n"
              "You should see 2 comments per each level..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:level(error),
  io:format("~n------------------------------------------------~n"
              "You should see 2 comments per each level >= error..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:level(fatal),
  io:format("~n------------------------------------------------~n"
              "You should only see fatal comments..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:level(debug, ?MODULE),
  io:format("~n------------------------------------------------~n"
              "You should see 1 comments per each level < fatal..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:level(fatal, ?MODULE),
  io:format("~n------------------------------------------------~n"
              "You should only see fatal comments..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:level(info, ?MODULE),
  ok = elog:level(info, elog_tester_helper),
  io:format("~n------------------------------------------------~n"
              "You should see 2 comments per each level >= info..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:level(warn),
  io:format("~n------------------------------------------------~n"
              "You should see 2 comments per each level >= warn..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:level(fatal),
  ok = elog:level(debug, "debug"),
  io:format("~n------------------------------------------------~n"
              "You should see 2 fatal and 2 debug comments..."
                "~n------------------------------------------------~n"),
  ok = log_all().

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

log_all() ->
  lists:foreach(
    fun(L) ->
            timer:sleep(100),
            log(L, "~p at ~p~n", [L, element(2, calendar:local_time())])
    end, ?LOG_LEVELS),
  timer:sleep(erlang:trunc(?SLEEP / 10)),
  ?THROW("~p at ~p~n", [?LOG_LEVEL_ERROR, element(2, calendar:local_time())]),
  elog_tester_helper:log_all().