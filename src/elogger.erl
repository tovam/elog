%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc The real logging module behaviour.
%%% Defines how the messages will be logged. The user module should export:
%%%<ul>
%%%   <li>
%%%   <pre>init(Args::term()) -> {@link init_result()}</pre>
%%%     Opens and/or initializes the client.<br/>
%%%   </li><li>
%%%   <pre>log(Message::{@link log()}, State::term()) -> {ok, NewState::term()}</pre>  
%%%     Called each time a message is received<br/>
%%%   </li><li>
%%%   <pre>handle_info(Message::term(), State::term()) -> {ok, NewState::term()}</pre>  
%%%     Called each time an erlang message is received<br/>
%%%   </li><li>
%%%   <pre>terminate(Reason :: normal | shutdown | term(), State) -> _</pre>
%%%     Let the user module clean up. Always called when server terminates.<br/>
%%%   </li></ul>
%%% @end
%%%-------------------------------------------------------------------
-module(elogger).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(gen_server).

%% BEHAVIOUR
-export([behaviour_info/1]).
%% API
-export([start_link/1, start_link/2]).
-export([add/3, remove/3]).
%% GEN SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @headerfile "elog.hrl"
-include("elog.hrl").

-type exception_class() :: mod | cat | re.
-type log() :: #log{}.
-type init_result() :: {ok, State::term()} | ignore | {stop, Reason::term()}.
-export_type([log/0, init_result/0, exception_class/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {what = all      :: all | just_exceptions,
                modules = []    :: [atom()],
                categories = [] :: [atom()],
                regexps = []    :: [{re_pattern, integer(), integer(), binary()}],
                module          :: atom(),
                mod_state       :: term()}).
-opaque state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% @hidden
-spec behaviour_info(callbacks | term()) -> undefined | [{atom(), non_neg_integer()}].
behaviour_info(callbacks) -> [{init, 1}, {log, 2}, {handle_info, 2}, {terminate, 2}];
behaviour_info(_Other) ->
  undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec start_link(elog:loglevel()) -> ignore | {ok, pid()}.
start_link(Level) ->
  start_link(Level, all).

%% @hidden
-spec start_link(elog:loglevel(), all|just_exceptions) -> ignore | {ok, pid()}.
start_link(Level, What) ->
  case elog:get_env(Level, logger) of
    {Mod, InitArgs} ->
      start_link(Level, What, Mod, InitArgs);
    Wrong ->
      throw({wrong_logger, Level, Wrong})
  end.

%% @hidden
-spec start_link(elog:loglevel(), all|just_exceptions, atom(), term()) -> ignore | {ok, pid()}.
start_link(Level, What, Mod, InitArgs) ->
  SystemLevel =
    case application:get_env(elog, log_level) of
      {ok, Value} -> Value;
      _ -> ?LOG_LEVEL_INFO
    end,
  case first(Level, SystemLevel, lists:reverse(?LOG_LEVELS)) of
    Level ->
      case gen_server:start_link({local, process_name(Level)}, ?MODULE, {What, Mod, InitArgs}, []) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
      end;
    _ ->
      case What of
        just_exceptions ->
          case gen_server:start_link({local, process_name(Level)}, ?MODULE, {What, Mod, InitArgs}, []) of
            {ok, Pid} -> {ok, Pid};
            {error, {already_started, Pid}} -> {ok, Pid}
          end;
        _ ->
          ignore
      end
  end.

%% @hidden
-spec add(elog:loglevel(), exception_class(), atom()|string()) -> ok.
add(Level, Class, Value) ->
  gen_server:call(process_name(Level), {add, Class,  Value}, infinity).

%% @hidden
-spec remove(elog:loglevel(), exception_class(), atom()|string()) -> ok.
remove(Level, Class, Value) ->
  gen_server:call(process_name(Level), {remove, Class,  Value}, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN SERVER CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec init({all | just_exceptions, atom(), term()}) -> {ok, state()} | init_result().
init({What, Mod, InitArgs}) ->
  case Mod:init(InitArgs) of
    {ok, ModState} ->
      {ok, #state{module    = Mod,
                  mod_state = ModState,
                  what      = What}};
    Other ->
      Other
  end.

%% @hidden
-spec handle_call({add, atom() | string()} | {remove, atom() | string()}, reference(), state()) -> {reply, ok, state()} | {stop, normal, ok, state()}.
handle_call({add, _, _}, _From, State = #state{what = all}) ->
  {reply, ok, State};
handle_call({add, mod, Module}, _From, State) ->
  {reply, ok, State#state{modules = [Module|State#state.modules]}};
handle_call({add, cat, Category}, _From, State) ->
  {reply, ok, State#state{categories = [Category|State#state.categories]}};
handle_call({add, re, RegExpStr}, _From, State) ->
  {ok, RegExp} = re:compile(RegExpStr),
  {reply, ok, State#state{regexps = [RegExp|State#state.regexps]}};
handle_call({remove, _, _}, _From, State = #state{what = all}) ->
  {reply, ok, State};
handle_call({remove, mod, Module}, _From, State = #state{modules = [Module], regexps = [], categories = []}) ->
  {stop, normal, ok, State};
handle_call({remove, mod, Module}, _From, State) ->
  {reply, ok, State#state{modules = State#state.modules -- [Module]}};
handle_call({remove, cat, Category}, _From, State = #state{categories = [Category], regexps = [], modules = []}) ->
  {stop, normal, ok, State};
handle_call({remove, cat, Category}, _From, State) ->
  {reply, ok, State#state{categories = State#state.categories -- [Category]}};
handle_call({remove, re, RegExpStr}, _From, State) ->
  {ok, RegExp} = re:compile(RegExpStr),
  case State#state.regexps -- [RegExp] of
    [] when State#state.modules =:= [] andalso State#state.categories =:= [] ->
      {stop, normal, ok, State};
    NewExps ->
      {reply, ok, State#state{regexps = NewExps}}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @hidden
-spec handle_cast(#log{}, state()) -> {noreply, state()}.
handle_cast(Msg, State = #state{what = all}) ->
  call_logger(Msg, State);
handle_cast(Msg = #log{module   = Module,
                       category = Category,
                       text     = Text,
                       args     = Args},
            State = #state{modules    = Modules,
                           categories = Categories,
                           regexps    = RegExps}) ->
  case lists:member(Module, Modules)
    orelse lists:member(Category, Categories)
         orelse lists:any(fun(RegExp) ->
                                  match =:= re:run(io_lib:format(Text, Args),
                                                   RegExp, [{capture, none}])
                          end, RegExps) of
    true ->
      call_logger(Msg, State);
    false ->
      {noreply, State}
  end;
handle_cast(Msg, State) ->
  call_logger(#log{args = [Msg],
                   level = elog:get_env(level),
                   line = ?LINE,
                   module = ?MODULE,
                   node = node(),
                   pid = self(),
                   text = "Unknown log: ~p"}, State).

%% @hidden
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(Info, State = #state{module = Mod, mod_state = ModState}) ->
  try Mod:handle_info(Info, ModState) of
    {ok, NewModSt} -> 
      {noreply, State#state{mod_state = NewModSt}};
    _ ->
      {noreply, State}
  catch
    _:{ok, NewModSt} -> 
      {noreply, State#state{mod_state = NewModSt}};
    _:_ ->
      {noreply, State}
  end.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(Reason, #state{module = Mod, mod_state = ModState}) ->
  Mod:terminate(Reason, ModState).

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal
%%====================================================================
%%% @hidden
-spec process_name(elog:loglevel()) -> atom().
process_name(Level) ->
  list_to_atom("elogger-" ++ atom_to_list(Level)).

-spec first(X, X, [X]) -> none | X.
first(_X, _Y, []) -> none;
first(X, _Y, [X|_]) -> X;
first(_X, Y, [Y|_]) -> Y;
first(X, Y, [_Z|Rest]) -> first(X, Y, Rest).

call_logger(Msg, State = #state{module = Mod, mod_state = ModState}) ->
  try Mod:log(Msg, ModState) of
    {ok, NewModState} ->
      {noreply, State#state{mod_state = NewModState}};
    Other ->
      io:format("~p:~p - Invalid response from ~p: ~p~n", [?MODULE, ?LINE, Mod, Other]),
      {noreply, State}
  catch
    _:Error ->
      error_logger:error_msg("~p:~p - Error: ~p~n\t~p~n", [?MODULE, ?LINE, Error,
                                                           erlang:get_stacktrace()]),
      {noreply, State}
  end.