-module(toolbox_node_stats).

-behaviour(gen_server).

%% API
-export([
]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, map(), 0}.
init([]) ->
  lager:info("Starting ~p...", [?MODULE]),
  Incrementals = gather_incrementals(),
  {ok, Incrementals, 0}.

-spec handle_call(term(), pid(), map()) -> {stop, normal, map()}.
handle_call(Request, From, State) ->
  lager:info("Unexpected call: ~p from ~p", [Request, process_info(From)]),
  {stop, normal, State}.

-spec handle_cast(term(), map()) -> {stop, normal, map()}.
handle_cast(Msg, State) ->
  lager:info("Unexpected cast: ~p", [Msg]),
  {stop, normal, State}.

-spec handle_info(term(), map()) -> {noreply, map(), pos_integer()} | {stop, normal, map()}.
handle_info(timeout, Last) ->
  Next = gather_incrementals(),
  Diff = toolbox:diff_metrics(Last, Next),
  Absolutes = gather_absolutes(),
  apply(toolbox:metrics_fun(), [node_stats, #{incrementals => Diff, absolutes => Absolutes}]),
  {noreply, Next, ?TIMEOUT};

handle_info(Msg, State) ->
  lager:info("Unexpected info: ~p", [Msg]),
  {stop, normal, State}.

-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
gather_incrementals() ->
  {{input, In}, {output, Out}} = erlang:statistics(io),
  {AbsoluteReductions, _IncrementalReductions} = erlang:statistics(reductions),
  #{bytes_in => In, bytes_out => Out, reductions => AbsoluteReductions}.

gather_absolutes() ->
  maps:from_list([{{memory, Key}, Value} || {Key, Value} <- erlang:memory()]).
