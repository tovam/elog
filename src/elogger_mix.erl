%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc Mixed {@link elogger}. Let's you use many {@link elogger}s at the same time
%%% ==Usage==
%%%       To use it in elog <a href="overview-summary.html#configuration">config files</a> you need to set 
%%%       the corresponding <i>logger</i> env variable to: <code>{elogger_mix, [Option]}</code><br/>
%%%       (where <code>Option :: {Name::atom(), Value::term()}</code>). Availabe options include:
%%% <dl>
%%%   <dt><b>loggers</b> :: [{Name::atom(), InitArgs::term()}]</dt>
%%%     <dd>Loggers to use</dd>
%%% </dl>
%%% @end
%%%-------------------------------------------------------------------
-module(elogger_mix).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(elogger).

-include("elog.hrl").

-export([init/1, log/2, handle_info/2, terminate/2]).

-record(state, {loggers :: [{atom(), term()}]}).
-opaque state() :: #state{}.

%%% @hidden
-spec init([proplists:property()]) -> {ok, state()} | {stop, no_loggers}.
init(Props) ->
  Loggers = proplists:get_value(loggers, Props, []),
  InitializedLoggers =
    lists:reverse(
      lists:foldl(
        fun({Mod, InitArgs}, Acc) ->
                try Mod:init(InitArgs) of
                  {ok, ModSt} -> [{Mod, ModSt} | Acc];
                  ignore -> Acc;
                  {stop, ModReason} -> throw({stop, {Mod, ModReason}})
                catch
                  _:{ok, ModSt} -> [{Mod, ModSt} | Acc];
                  _:ignore -> Acc;
                  _:{stop, ModReason} -> throw({stop, {Mod, ModReason}})
                end
        end, [], Loggers)),
  case InitializedLoggers of
    [] ->
      {stop, no_loggers};
    InitializedLoggers ->
      {ok, #state{loggers = InitializedLoggers}}
  end.

%%% @hidden
-spec log(elogger:log(), state()) -> {ok, state()}.
log(Log, State = #state{loggers  = Loggers}) ->
  NewLoggers =
    lists:map(fun({Mod, ModSt}) ->
                      {ok, NewModSt} = Mod:log(Log, ModSt),
                      {Mod, NewModSt}
              end, Loggers),
  {ok, State#state{loggers = NewLoggers}}.

%%% @hidden
-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(Info, State = #state{loggers = Loggers}) ->
  NewLoggers =
    lists:map(fun({Mod, ModSt}) ->
                      {ok, NewModSt} = Mod:handle_info(Info, ModSt),
                      {Mod, NewModSt}
              end, Loggers),
  {ok, State#state{loggers = NewLoggers}}.

%%% @hidden
-spec terminate(normal | shutdown | term(), state()) -> ok.
terminate(_Reason, _State) -> ok.