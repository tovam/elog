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
-export([add/2, remove/2]).
%% GEN SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @headerfile "elog.hrl"
-include("elog.hrl").

%% @type log() = #log{time = {{pos_integer(),pos_integer(),pos_integer()},{pos_integer(),pos_integer(),pos_integer()}},
%%                    level = loglevel(),
%%                    module = atom(),
%%                    line = integer(),
%%                    pid = pid(),
%%                    node = node(),
%%                    stacktrace = [term()],
%%                    text = string(),
%%                    args = [term()]}
-type log() :: #log{}.

%% @type init_result()     = {ok, State::term()} | ignore | {stop, Reason::term()}.
%%       The expected result for Mod:init/1
-type init_result() :: {ok, State::term()} | ignore | {stop, Reason::term()}.

-export_type([log/0, init_result/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {what = all    :: all | just_exceptions,
                modules = []  :: [atom()],
                regexps = []  :: [{re_pattern, integer(), integer(), binary()}],
                module        :: atom(),
                mod_state     :: term()}).
-opaque state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% @hidden
-spec behaviour_info(callbacks | term()) -> undefined | [{atom(), non_neg_integer()}].
behaviour_info(callbacks) -> [{init, 1}, {log, 2}, {terminate, 2}];
behaviour_info(_Other) ->
  undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec start_link(loglevel()) -> ignore | {ok, pid()}.
start_link(Level) ->
  start_link(Level, all).

%% @hidden
-spec start_link(loglevel(), all|just_exceptions) -> ignore | {ok, pid()}.
start_link(Level, What) ->
  case elog:get_env(Level, logger) of
    {Mod, InitArgs} ->
      start_link(Level, What, Mod, InitArgs);
    Wrong ->
      throw({wrong_logger, Level, Wrong})
  end.

%% @hidden
-spec start_link(loglevel(), all|just_exceptions, atom(), term()) -> ignore | {ok, pid()}.
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
-spec add(loglevel(), atom()|string()) -> ok.
add(Level, ModuleOrRegExp) ->
  gen_server:call(process_name(Level), {add, ModuleOrRegExp}, infinity).

%% @hidden
-spec remove(loglevel(), atom()|string()) -> ok.
remove(Level, ModuleOrRegExp) ->
  gen_server:call(process_name(Level), {remove, ModuleOrRegExp}, infinity).

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
handle_call({add, _}, _From, State = #state{what = all}) ->
  {reply, ok, State};
handle_call({add, Module}, _From, State) when is_atom(Module) ->
  {reply, ok, State#state{modules = [Module|State#state.modules]}};
handle_call({add, RegExpStr}, _From, State) ->
  {ok, RegExp} = re:compile(RegExpStr),
  {reply, ok, State#state{regexps = [RegExp|State#state.regexps]}};
handle_call({remove, _}, _From, State = #state{what = all}) ->
  {reply, ok, State};
handle_call({remove, Module}, _From, State = #state{modules = [Module], regexps = []}) ->
  {stop, normal, ok, State};
handle_call({remove, Module}, _From, State) when is_atom(Module) ->
  {reply, ok, State#state{modules = State#state.modules -- [Module]}};
handle_call({remove, RegExpStr}, _From, State = #state{what = just_exceptions}) ->
  {ok, RegExp} = re:compile(RegExpStr),
  case State#state.regexps -- [RegExp] of
    [] when State#state.modules =:= [] ->
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
handle_cast(Msg = #log{module = Module,
                       text   = Text,
                       args   = Args},
            State = #state{modules = Modules,
                           regexps = RegExps}) ->
  case lists:member(Module, Modules)
         orelse lists:any(fun(RegExp) ->
                                  match =:= re:run(io_lib:format(Text, Args), RegExp, [{capture, none}])
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
handle_info(Msg, State) ->
  error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
  {noreply, State}.

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
-spec process_name(loglevel()) -> atom().
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
      io:format("~p:~p - Invalid response: ~p~n", [?MODULE, ?LINE, Other]),
      {noreply, State}
  catch
    _:Error ->
      io:format("~p:~p - Error: ~p~n", [?MODULE, ?LINE, Error]),
      {noreply, State}
  end.