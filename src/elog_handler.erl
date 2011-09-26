%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc <b>elog</b> error handler for {@link error_logger}
%%% @end
%%%-------------------------------------------------------------------
-module(elog_handler).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(gen_event).

-export([replace_tty/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% @headerfile "elog.hrl"
-include("elog.hrl").

-record(state, {sasl :: ignore | report}).
-opaque state() :: #state{}.

%% ====================================================================
%% External functions
%% ====================================================================
%% @equiv replace_tty([])
-spec replace_tty() -> ok.
replace_tty() ->
  replace_tty([]).

%% @doc  replaces the default tty error logger
-spec replace_tty([ignore_sasl]) -> ok.
replace_tty(Options) ->
  error_logger:delete_report_handler(error_logger),
  error_logger:delete_report_handler(error_logger_tty_h),
  error_logger:add_report_handler(?MODULE, Options).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init([ignore_sasl]) -> {ok, state()}.
init(Options) ->
  ?INFO("Elog Handler added~n", []),
  {ok, #state{sasl = case lists:member(ignore_sasl, Options) of
                       true -> ignore;
                       false -> report
                     end}}.

%% @hidden
-spec handle_event({atom(), pid(), {pid(), string() | atom(), any()}}, state()) -> {ok, state()}.
handle_event(Event = {_Type, GL, _Msg}, State = #state{sasl = ignore}) when node(GL) /= node() ->
  {ok, #state{sasl = report}} = handle_event(Event, #state{sasl = report}),
  {ok, State};
handle_event(Event = {error_report, _GL, {_Pid, Type, _Report}}, State = #state{sasl = ignore}) ->
  try riak_err_stdlib:is_my_error_report(Type) of
    true ->
      {ok, State};
    false ->
      {ok, #state{sasl = report}} = handle_event(Event, #state{sasl = report}),
      {ok, State}
  catch
    _:undef ->
      {ok, #state{sasl = report}} = handle_event(Event, #state{sasl = report}),
      {ok, State}
  end;
handle_event(Event = {info_report, _GL, {_Pid, Type, _Report}}, State = #state{sasl = ignore}) ->
  try riak_err_stdlib:is_my_info_report(Type) of
    true ->
      {ok, State};
    false ->
      {ok, #state{sasl = report}} = handle_event(Event, #state{sasl = report}),
      {ok, State}
  catch
    _:undef ->
      {ok, #state{sasl = report}} = handle_event(Event, #state{sasl = report}),
      {ok, State}
  end;
handle_event(Event = {warning_report, _GL, _Report}, State = #state{sasl = ignore}) ->
  {ok, #state{sasl = report}} = handle_event(Event, #state{sasl = report}),
  {ok, State};
handle_event(_Event, State = #state{sasl = ignore}) ->
  {ok, State};
handle_event({error, _GLeader, {_Pid, Text, Args}}, State) ->
  try {string:str(Text, "{undef,[{ssl_session_cache,delete,"),
       string:str(Text, "module: misultin_socket")} of
    {0, 0} ->
      ?LOG('elogger-error', ?LOG_LEVEL_ERROR, ?MODULE, Text, Args, []),
      {ok, State};
    _ ->
      %%XXX: A bug in ssl raises this errors, nothing to worry about really
      %%XXX: When misultin sockets are brutally closed by client, they report an error... again nothing to worry about really
      {ok, State}
  catch
    _:_ ->
      ?LOG('elogger-error', ?LOG_LEVEL_ERROR, ?MODULE, Text, Args, [])
  end;
handle_event({error_report, _GLeader, {_Pid, Type, [Int|_] = Report}}, State) when is_integer(Int) ->
  ?LOG('elogger-error', ?LOG_LEVEL_ERROR, ?MODULE, "~s:~n\t~s~n", [Type, Report], []),
  {ok, State};
handle_event({error_report, _GLeader, {_Pid, Type, Report}}, State) ->
  ?LOG('elogger-error', ?LOG_LEVEL_ERROR, ?MODULE, "~s:~n\t~p~n", [Type, Report], []),
  {ok, State};
handle_event({warning_msg, _GLeader, {_Pid, Text, Args}}, State) ->
  ?WARN(Text, Args),
  {ok, State};
handle_event({warning_report, _GLeader, {_Pid, stat, [Int|_] = Report}}, State) when is_integer(Int) ->
  ?STAT("~s~n", [Report]),
  {ok, State};
handle_event({warning_report, _GLeader, {_Pid, stat, Report}}, State) ->
  ?STAT("Report:~n~p~n", [Report]),
  {ok, State};
handle_event({warning_report, _GLeader, {_Pid, Type, [Int|_] = Report}}, State) when is_integer(Int) ->
  ?WARN("~s:~n\t~s~n", [Type, Report]),
  {ok, State};
handle_event({warning_report, _GLeader, {_Pid, Type, Report}}, State) ->
  ?WARN("~s:~n\t~p~n", [Type, Report]),
  {ok, State};
handle_event({info_msg, _GLeader, {_Pid, Text, Args}}, State) ->
  ?INFO(Text, Args),
  {ok, State};
handle_event({info_report, _GLeader, {_Pid, progress, [Int|_] = Report}}, State) when is_integer(Int) ->
  ?DEBUG("~s~n", [Report]),
  {ok, State};
handle_event({info_report, _GLeader, {_Pid, progress, Report}}, State) ->
  ?DEBUG("~n\t~p~n", [Report]),
  {ok, State};
handle_event({info_report, _GLeader, {_Pid, Type, [Int|_] = Report}}, State) when is_integer(Int) ->
  ?INFO("~s:~n\t~s~n", [Type, Report]),
  {ok, State};
handle_event({info_report, _GLeader, {_Pid, Type, Report}}, State) ->
  ?INFO("~s:~n\t~p~n", [Type, Report]),
  {ok, State};
handle_event(Event, State) ->
  ?LOG('elogger-error', ?LOG_LEVEL_ERROR, ?MODULE, "Unknown error logged:~n\t~p~n", [Event], []),
  {ok, State}.

%% @hidden
-spec handle_call(term(), state()) -> {ok, ok, state()}.
handle_call(_Request, State) -> {ok, ok, State}.
%% @hidden
-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(_Info, State) -> {ok, State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(Reason, _State) ->
  ?LOG('elogger-error', ?LOG_LEVEL_ERROR, ?MODULE, "elog handler removed: ~p~n", [Reason], []).

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.