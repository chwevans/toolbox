-module(toolbox_monitor_sup).

-behaviour(supervisor).

-define(CHILD(Mod), {Mod, {Mod, start_link, []}, permanent, brutal_kill, worker, [Mod]}).
-define(CHILD(Mod, Args), {Mod, {Mod, start_link, Args}, permanent, brutal_kill, worker, [Mod]}).

%% API
-export([trace_process/2, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-spec trace_process(term(), pid()) -> ok.
trace_process(Name, Pid) ->
  supervisor:start_child(?MODULE, [Name, Pid]),
  ok.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  %lager:info("Starting ~p...", [?MODULE]),
  Children = [?CHILD(toolbox_monitor)],
  {ok, {{simple_one_for_one, 5, 1}, Children}}.
