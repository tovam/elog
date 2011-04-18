%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc Mixed {@link elogger}. Let's you use many {@link elogger}s at the same time
%%% ==Usage==
%%%       To use it in elog <a href="overview-summary.html#configuration">config files</a> you need to set 
%%%       the corresponding <i>logger</i> env variable to: <code>{elogger_mix, Loggers}</code><br/>
%%%       (where <code>Loggers :: [default_logger | {default_logger, {@link filter()}} | {@link logger()}]</code>).
%%% @end
%%%-------------------------------------------------------------------
-module(elogger_mix).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(elogger).

-include("elog.hrl").

-type filter() :: {cat | mod, atom()} | {re, string()}.
-type logger() :: {atom(), term()} | {atom(), term(), filter()}.
-export_type([logger/0, filter/0]).

-export([init/1, log/2, handle_info/2, terminate/2]).

-record(state, {loggers :: [logger()]}).
-opaque state() :: #state{}.

%%% @hidden
-spec init([logger()]) -> {ok, state()} | {stop, no_loggers}.
init(Loggers) ->
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
                end;
           ({Mod, InitArgs, {re, RegExpStr}}, Acc) ->
                {ok, RegExp} = re:compile(RegExpStr),
                try Mod:init(InitArgs) of
                  {ok, ModSt} -> [{Mod, ModSt, {re, RegExp}} | Acc];
                  ignore -> Acc;
                  {stop, ModReason} -> throw({stop, {Mod, ModReason}})
                catch
                  _:{ok, ModSt} -> [{Mod, ModSt, {re, RegExp}} | Acc];
                  _:ignore -> Acc;
                  _:{stop, ModReason} -> throw({stop, {Mod, ModReason}})
                end;
           ({Mod, InitArgs, Filter}, Acc) ->
                try Mod:init(InitArgs) of
                  {ok, ModSt} -> [{Mod, ModSt, Filter} | Acc];
                  ignore -> Acc;
                  {stop, ModReason} -> throw({stop, {Mod, ModReason}})
                catch
                  _:{ok, ModSt} -> [{Mod, ModSt, Filter} | Acc];
                  _:ignore -> Acc;
                  _:{stop, ModReason} -> throw({stop, {Mod, ModReason}})
                end
        end, [], expand_defaults(Loggers))),
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
                      {Mod, NewModSt};
                 ({Mod, ModSt, {cat, Cat}}) ->
                      case Log#log.category of
                        Cat ->
                          {ok, NewModSt} = Mod:log(Log, ModSt),
                          {Mod, NewModSt};
                        _ ->
                          {Mod, ModSt}
                      end;
                 ({Mod, ModSt, {mod, XMod}}) ->
                      case Log#log.module of
                        XMod ->
                          {ok, NewModSt} = Mod:log(Log, ModSt),
                          {Mod, NewModSt};
                        _ ->
                          {Mod, ModSt}
                      end;
                 ({Mod, ModSt, {re, RegExp}}) ->
                      case re:run(io_lib:format(Log#log.text, Log#log.args),
                                  RegExp, [{capture, none}]) of
                        match ->
                          {ok, NewModSt} = Mod:log(Log, ModSt),
                          {Mod, NewModSt};
                        _ ->
                          {Mod, ModSt}
                      end
              end, Loggers),
  {ok, State#state{loggers = NewLoggers}}.

%%% @hidden
-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(Info, State = #state{loggers = Loggers}) ->
  NewLoggers =
    lists:map(fun({Mod, ModSt}) ->
                      {ok, NewModSt} = Mod:handle_info(Info, ModSt),
                      {Mod, NewModSt};
                 ({Mod, ModSt, Filter}) ->
                      {ok, NewModSt} = Mod:handle_info(Info, ModSt),
                      {Mod, NewModSt, Filter}
              end, Loggers),
  {ok, State#state{loggers = NewLoggers}}.

%%% @hidden
-spec terminate(normal | shutdown | term(), state()) -> ok.
terminate(_Reason, _State) -> ok.

expand_defaults(Loggers) ->
  {Mod, Args} = elog:get_env(logger),
  lists:map(fun(default_logger) ->
                    {Mod, Args};
               ({default_logger, Filter}) ->
                    {Mod, Args, Filter};
               (Other) ->
                    Other
            end, Loggers).