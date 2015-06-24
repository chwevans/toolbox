-module(toolbox_monitor).

-behaviour(gen_server).

%% API
-export([
  stop/1
]).

%% gen_server callbacks
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TIMEOUT, application:get_env(toolbox, monitor_timeout, 5000)).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(atom(), pid()) -> {ok, pid()}.
start_link(Name, Pid) when is_atom(Name) andalso is_pid(Pid) ->
  case process_info(Pid) of
    undefined -> {error, notfound};
    _ -> gen_server:start_link({local, monitor_name(Name)}, ?MODULE, {Name, Pid}, [])
  end.

-spec stop(atom()) -> ok.
stop(Name) ->
  whereis(monitor_name(Name)) ! stop.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init({atom(), pid()}) -> {ok, map(), 0}.
init({Name, Pid}) ->
  lager:info("Starting ~p for ~p...", [?MODULE, Name]),
  erlang:monitor(process, Pid),
  Start = toolbox:get_metrics(Pid),
  {ok, #{pid => Pid, last => Start, name => Name}, 0}.

-spec handle_call(term(), pid(), map()) -> {stop, normal, map()}.
handle_call(Request, From, State = #{name := Name}) ->
  lager:info("Unexpected call(~p): ~p from ~p", [Name, Request, process_info(From)]),
  {stop, normal, State}.

-spec handle_cast(term(), map()) -> {stop, normal, map()}.
handle_cast(Msg, State = #{name := Name}) ->
  lager:info("Unexpected cast(~p): ~p", [Name, Msg]),
  {stop, normal, State}.

-spec handle_info(term(), map()) -> {noreply, map(), pos_integer()} | {stop, term(), map()}.
handle_info({'DOWN', _Ref, process, Pid, Reason}, State = #{pid := Pid}) ->
  {stop, Reason, State};

handle_info(timeout, State = #{name := Name, pid := Pid, last := Last}) ->
  Next = toolbox:get_metrics(Pid),
  Diff = toolbox:diff_metrics(Last, Next),
  apply(toolbox:metrics_fun(), [Name, Diff]),
  {noreply, State#{last => Next}, ?TIMEOUT};

handle_info(stop, State) ->
  {stop, normal, State};

handle_info(Msg, State = #{name := Name}) ->
  lager:info("Unexpected info(~p): ~p", [Name, Msg]),
  {stop, normal, State}.

-spec terminate(term(), map()) -> ok.
terminate(Reason, #{name := Name, pid := _Pid}) ->
  lager:info("Terminating: ~p for ~p", [Reason, Name]),
  supervisor:terminate_child(toolbox_monitor_sup, self()),
  ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, Pid, _Extra) ->
  {ok, Pid}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec monitor_name(atom()) -> atom().
monitor_name(Name) when is_atom(Name) ->
  list_to_atom("toolbox_monitor_" ++ atom_to_list(Name)).
