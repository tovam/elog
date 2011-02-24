%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc File {@link elogger}.
%%% ==Usage==
%%%       To use it in elog <a href="overview-summary.html#configuration">config files</a> you need to set 
%%%       the corresponding <i>logger</i> env variable to: <code>{elogger_file, [Option]}</code><br/>
%%%       (where <code>Option :: {Name::atom(), Value::term()}</code>). Availabe options include:
%%% <dl>
%%%   <dt><b>file</b> :: string()</dt>
%%%     <dd>Default file name. It will be modified if it needs to be rotated. It may be an absolute or relative path</dd>
%%%   <dt><b>size_limit</b> :: infinity | pos_integer()</dt>
%%%     <dd>Size limite for log rotation, in kilobytes</dd>
%%%   <dt><b>date_break</b> :: boolean()</dt>
%%%     <dd>Should log be rotated everyday?</dd>
%%% </dl>
%%% @end
%%%-------------------------------------------------------------------
-module(elogger_file).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(elogger).

-include("elog.hrl").

-export([init/1, log/2, terminate/2]).

-record(state, {dir       = "."         :: string(),
                file      = "elog.log"  :: string(),
                size_limit= infinity    :: pos_integer() | infinity,
                date_break= false       :: boolean()}).
-opaque state() :: #state{}.

%%% @hidden
-spec init([proplists:property()]) -> {ok, state()} | {stop, atom()}.
init(Props) ->
  Filename = proplists:get_value(file, Props, "elog.log"),
  Dir = filename:dirname(Filename),
  File = filename:basename(Filename),
  State = #state{dir        = Dir,
                 file       = File,
                 size_limit = proplists:get_value(size_limit, Props, infinity),
                 date_break = proplists:get_bool(date_break, Props)},
  case filelib:ensure_dir(Filename) of
    ok ->
      {ok, State};
    {error, PosixError} ->
      {stop, PosixError}
  end.

%%% @hidden
-spec log(elogger:log(), state()) -> {ok, state()}.
log(#log{time        = {_,{HH,Mm,SS}},
         level       = Level,
         module      = Mod,
         line        = Line,
         pid         = Pid,
         node        = Node,
         text        = Text,
         args        = Args,
         stacktrace  = []}, State = #state{dir        = Dir,
                                           file       = File,
                                           date_break = Break,
                                           size_limit = Limit}) ->
  FileName =
    case Break of
      true ->
        {{Y, M, D}, _Time} = calendar:local_time(),
        io_lib:format("~s/~4..0b-~2..0b-~2..0b-~s", [Dir, Y, M, D, File]);
      false ->
        filename:join(Dir, File)
    end,
  {ok, Fd} = file:open(filename:absname(FileName), [write, append]),
  io:format(Fd, "~2..0b:~2..0b:~2..0b|~p|~s|~s:~p|~p: " ++ Text,
            [HH,Mm,SS, Pid, Node, Mod, Line, Level | Args]),
  ok = file:close(Fd),
  ok = limit_check(FileName, Limit),
  {ok, State};
log(#log{time        = {_,{HH,Mm,SS}},
         level       = Level,
         module      = Mod,
         line        = Line,
         pid         = Pid,
         node        = Node,
         text        = Text,
         args        = Args,
         stacktrace  = Stack}, State = #state{dir        = Dir,
                                              file       = File,
                                              date_break = Break,
                                              size_limit = Limit}) ->
  FileName =
    case Break of
      true ->
        {{Y, M, D}, _Time} = calendar:local_time(),
        io_lib:format("~s/~4..0b-~2..0b-~2..0b-~s", [Dir, Y, M, D, File]);
      false ->
        filename:join(Dir, File)
    end,
  {ok, Fd} = file:open(filename:absname(FileName), [write, append]),
  io:format(Fd, "~2..0b:~2..0b:~2..0b|~p|~s|~s:~p|~p: " ++ Text ++
              "~n\tStack Trace:~n\t\t~p~n",
            [HH,Mm,SS, Pid, Node, Mod, Line, Level | Args] ++ [Stack]),
  ok = limit_check(FileName, Limit),
  ok = file:close(Fd),
  {ok, State}.

%%% @hidden
-spec terminate(normal | shutdown | term(), {}) -> ok.
terminate(_Reason, _State) -> ok.

limit_check(_FileName, infinity) -> ok;
limit_check(FileName, Limit) ->
  case filelib:file_size(FileName) of
    X when X >= (Limit * 1000) ->
      {_, _, MicroSecs} = erlang:now(),
      Millis = erlang:trunc(MicroSecs/1000),
      Now = integer_to_list(calendar:datetime_to_gregorian_seconds(
                              calendar:universal_time()) * 1000 + Millis),
      file:rename(FileName, [FileName, $., Now]);
    _ ->
      ok
  end.
