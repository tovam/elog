%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2010 Fernando Benavides <greenmellon@gmail.com>
%%% @doc The real logging module
%%% @end
%%%-------------------------------------------------------------------
-module(elogger).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(gen_server).

%% api
-export([start_link/1, start_link/2]).
-export([add/2, remove/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @headerfile "elog.hrl"
-include("elog.hrl").

-record(state, {what = all    :: all | just_exceptions,
                file = none   :: none | string(),
                modules = []  :: [atom()],
                regexps = []  :: [{re_pattern, integer(), integer(), binary()}]}).
-opaque state() :: #state{}.

%%====================================================================
%% API
%%====================================================================

%% hidden
-spec start_link(loglevel()) -> ignore | {ok, pid()}.
start_link(Level) ->
  start_link(Level, all).

%% hidden
-spec start_link(loglevel(), all|just_exceptions) -> ignore | {ok, pid()}.
start_link(Level, What) ->
  SystemLevel =
    case application:get_env(elog, log_level) of
      {ok, Value} -> Value;
      _ -> ?LOG_LEVEL_INFO
    end,
  case first(Level, SystemLevel, lists:reverse(?LOG_LEVELS)) of
    Level ->
      case gen_server:start_link({local, process_name(Level)}, ?MODULE, What, []) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
      end;
    _ ->
      ignore
  end.

%% hidden
-spec add(loglevel(), atom()|string()) -> ok.
add(Level, ModuleOrRegExp) ->
  gen_server:call(process_name(Level), {add, ModuleOrRegExp}).

%% hidden
-spec remove(loglevel(), atom()|string()) -> ok.
remove(Level, ModuleOrRegExp) ->
  gen_server:call(process_name(Level), {remove, ModuleOrRegExp}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%% hidden
-spec init(all | just_exceptions) -> {ok, state()}.
init(What) ->
  LogFile =
    case application:get_env(elog, log_file) of
      {ok, Value} -> Value;
      _ -> none
    end,
  {ok, #state{what= What, file = LogFile}}.

%% hidden
-spec handle_call({add, atom() | string()}, reference(), state()) -> {reply, ok, state()}.
handle_call({add, _}, _From, State = #state{what = all}) ->
  {reply, ok, State};
handle_call({add, Module}, _From, State) when is_atom(Module) ->
  {reply, ok, State#state{modules = [Module|State#state.modules]}};
handle_call({add, RegExpStr}, _From, State) ->
  {ok, RegExp} = re:compile(RegExpStr),
  {reply, ok, State#state{regexps = [RegExp|State#state.regexps]}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% hidden
-spec handle_cast(#log{} | {remove, atom() | string()}, state()) -> {noreply, state()}.
handle_cast({remove, Module}, State) when is_atom(Module) ->
  {noreply, State#state{modules = State#state.modules -- [Module]}};
handle_cast({remove, RegExpStr}, State) ->
  {ok, RegExp} = re:compile(RegExpStr),
  {noreply, State#state{regexps = State#state.regexps -- [RegExp]}};
handle_cast(Msg, State = #state{what = all, file = FileName}) ->
  ok = do_log(Msg, FileName),
  {noreply, State};
handle_cast(Msg = #log{module = Module,
                       text   = Text,
                       args   = Args},
            State = #state{file = FileName,
                           modules = Modules,
                           regexps = RegExps}) ->
  case lists:member(Module, Modules)
         orelse lists:any(fun(RegExp) ->
                                  match =:= re:run(io_lib:format(Text, Args), RegExp, [{capture, none}])
                          end, RegExps) of
    true ->
      ok = do_log(Msg, FileName);
    false ->
      ok
  end,
  {noreply, State};
handle_cast(_Msg, State) ->
  io:format("~p~n", [_Msg]),
  {noreply, State}.

%% hidden
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(Msg, State) ->
  error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
  {noreply, State}.

%% hidden
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.

%% hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal
%%====================================================================
do_log(#log{time        = {_,{HH,Mm,SS}},
            level       = Level,
            module      = Mod,
            line        = Line,
            pid         = Pid,
            node        = Node,
            text        = Text,
            args        = Args,
            stacktrace  = []}, none) ->
  io:format("~2..0b:~2..0b:~2..0b|~p|~s|~s:~p|~p: " ++ Text,
            [HH,Mm,SS, Pid, fancy_node(Node), Mod, Line, Level | Args]);
do_log(#log{time        = {_,{HH,Mm,SS}},
            level       = Level,
            module      = Mod,
            line        = Line,
            pid         = Pid,
            node        = Node,
            text        = Text,
            args        = Args,
            stacktrace  = Stack}, none) ->
  io:format("~2..0b:~2..0b:~2..0b|~p|~s|~s:~p|~p: " ++ Text ++
              "~n\tStack Trace:~n\t\t~p~n",
            [HH,Mm,SS, Pid, fancy_node(Node), Mod, Line, Level | Args] ++ [Stack]);
do_log(#log{time        = {_,{HH,Mm,SS}},
            level       = Level,
            module      = Mod,
            line        = Line,
            pid         = Pid,
            node        = Node,
            text        = Text,
            args        = Args,
            stacktrace  = []}, FileName) ->
  {ok, Fd} = file:open(filename:absname(FileName), [write, append]),
  io:format(Fd, "~2..0b:~2..0b:~2..0b|~p|~s|~s:~p|~p: " ++ Text,
            [HH,Mm,SS, Pid, fancy_node(Node), Mod, Line, Level | Args]),
  file:close(Fd);
do_log(#log{time        = {_,{HH,Mm,SS}},
            level       = Level,
            module      = Mod,
            line        = Line,
            pid         = Pid,
            node        = Node,
            text        = Text,
            args        = Args,
            stacktrace  = Stack}, FileName) ->
  {ok, Fd} = file:open(filename:absname(FileName), [write, append]),
  io:format(Fd, "~2..0b:~2..0b:~2..0b|~p|~s|~s:~p|~p: " ++ Text ++
              "~n\tStack Trace:~n\t\t~p~n",
            [HH,Mm,SS, Pid, fancy_node(Node), Mod, Line, Level | Args] ++ [Stack]),
  file:close(Fd).

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

%%% @hidden
-spec process_name(loglevel()) -> atom().
process_name(Level) ->
  list_to_atom("elogger-" ++ atom_to_list(Level)).

-spec first(X, X, [X]) -> none | X.
first(_X, _Y, []) -> none;
first(X, _Y, [X|_]) -> X;
first(_X, Y, [Y|_]) -> Y;
first(X, Y, [_Z|Rest]) -> first(X, Y, Rest).