-module(toolbox_sup).

-behaviour(supervisor).

-define(CHILD(Mod), {Mod, {Mod, start_link, []}, permanent, brutal_kill, worker, [Mod]}).
-define(CHILD(Mod, Args), {Mod, {Mod, start_link, Args}, permanent, brutal_kill, worker, [Mod]}).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  lager:info("Starting ~p...", [?MODULE]),
  Children = [
    ?CHILD(toolbox_monitor_sup)
  ],
  FinalChildren = case application:get_env(toolbox, enable_node_stats, false) of
    true -> [?CHILD(toolbox_node_stats) | Children];
    false -> Children
  end,
  {ok, {{one_for_one, 5, 1}, FinalChildren}}.
