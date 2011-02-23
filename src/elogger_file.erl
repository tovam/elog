%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc File {@link elogger}
%%% @end
%%%-------------------------------------------------------------------
-module(elogger_file).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(elogger).

-include("elog.hrl").

-export([init/1, log/2, terminate/2]).

-record(state, {file :: string()}).
-opaque state() :: #state{}.

%%% @hidden
-spec init(string()) -> elogger:init_result().
init(File) ->
  filelib:ensure_dir(File),
  {ok, #state{file = File}}.

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
         stacktrace  = []}, State = #state{file = FileName}) ->
  {ok, Fd} = file:open(filename:absname(FileName), [write, append]),
  io:format(Fd, "~2..0b:~2..0b:~2..0b|~p|~s|~s:~p|~p: " ++ Text,
            [HH,Mm,SS, Pid, fancy_node(Node), Mod, Line, Level | Args]),
  file:close(Fd),
  {ok, State};
log(#log{time        = {_,{HH,Mm,SS}},
         level       = Level,
         module      = Mod,
         line        = Line,
         pid         = Pid,
         node        = Node,
         text        = Text,
         args        = Args,
         stacktrace  = Stack}, State = #state{file = FileName}) ->
  {ok, Fd} = file:open(filename:absname(FileName), [write, append]),
  io:format(Fd, "~2..0b:~2..0b:~2..0b|~p|~s|~s:~p|~p: " ++ Text ++
              "~n\tStack Trace:~n\t\t~p~n",
            [HH,Mm,SS, Pid, fancy_node(Node), Mod, Line, Level | Args] ++ [Stack]),
  file:close(Fd),
  {ok, State}.

%%% @hidden
-spec terminate(normal | shutdown | term(), {}) -> ok.
terminate(_Reason, _State) -> ok.

fancy_node(SourceNode) ->
  case {string:tokens(atom_to_list(SourceNode), "@"),
        string:tokens(atom_to_list(node()), "@")} of
    {X,X} ->
      "";
    {[X,Y], [_Z,Y]} ->
      X;
    _ ->
      SourceNode
  end.