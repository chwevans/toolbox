-module(toolbox).

-behaviour(application).

-export([
  start/0,
  start/2,
  stop/1
]).

-export([
  trace/1,
  trace/2,
  measure/2,
  measure/3,
  log/2
]).

-export([
  get_metrics/1,
  metrics_fun/0,
  diff_metrics/2
]).

-spec start() -> ok.
start() -> application:ensure_all_started(?MODULE).

-spec start(term(), term()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) -> toolbox_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) -> ok.

%% @doc
%% Gather metrics and report them to the `metrics_fun' defined in configuration.
%% This gathers the metrics defined in the `metrics' configuration before and after `F'.
%% @end
-spec measure(atom(), fun(() -> term())) -> term().
measure(Name, F) ->
  StartMetrics = get_metrics(self()),
  {Time, R} = timer:tc(F),
  EndMetrics = get_metrics(self()),
  apply(metrics_fun(), [Name, maps:merge(#{time => Time}, diff_metrics(StartMetrics, EndMetrics))]),
  R.

%% @doc
%% Like `measure/2', however it takes an MFA and executes that to gather metrics.
%% The name reported to the `metrics_fun' is {M, F}.
%% @end
-spec measure(atom(), atom(), list(term())) -> term().
measure(M, F, A) ->
  StartMetrics = get_metrics(self()),
  {Time, R} = timer:tc(M, F, A),
  EndMetrics = get_metrics(self()),
  apply(metrics_fun(), [{M, F}, maps:merge(#{time => Time}, diff_metrics(StartMetrics, EndMetrics))]),
  R.

-spec trace(atom()) -> ok.
trace(Name) when is_atom(Name) ->
  Pid = whereis(Name),
  [{registered_name, RegisteredName}] = process_info(Pid, registered_name),
  trace(RegisteredName, Pid),
  ok.

-spec trace(term(), pid()) -> ok.
trace(Name, Pid) when is_pid(Pid) ->
  toolbox_monitor_sup:trace_process(Name, Pid),
  ok.

-spec get_metrics(pid()) -> map().
get_metrics(Pid) ->
  maps:from_list(process_info(Pid, application:get_env(?MODULE, metrics, [reductions, message_queue_len]))).

-spec diff_metrics(map(), map()) -> map().
diff_metrics(Start, End) ->
  maps:fold(fun(K, StartV, Acc) ->
    EndV = maps:get(K, End),
    maps:put(K, EndV - StartV, Acc)
  end, #{}, Start).

%% @doc
%% Gets the `metric_fun' defined in configuration as a tuple of module and function.
%% It should reference a 2 arity function that takes in a map of metrics.
%% @end
-spec metrics_fun() -> {atom(), atom()}.
metrics_fun() ->
  {M, F} = application:get_env(?MODULE, metrics_fun, {?MODULE, log}),
  fun M:F/2.

%% @doc
%% Default metrics function, this simply logs an io:format of the metrics gathered.
%% @end
-spec log(atom(), map()) -> ok.
log(Name, Metrics) -> lager:info("~p: ~p", [Name, Metrics]).
