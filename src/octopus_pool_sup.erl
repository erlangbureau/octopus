-module(octopus_pool_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%% API
-spec start_link(PoolId) -> {ok, Pid}
when
    PoolId  :: atom(),
    Pid     :: pid().

start_link(PoolId) ->
    supervisor:start_link(?MODULE, [PoolId]).


%% supervisor callbacks
init([PoolId]) ->
    Procs = [
        #{
            id          => octopus_pool_workers_sup,
            start       => {octopus_pool_workers_sup, start_link, [PoolId]},
            restart     => transient,
            shutdown    => infinity,
            type        => supervisor,
            modules     => [octopus_pool_workers_sup]
        },
        #{
            id          => octopus_pool_task_server,
            start       => {octopus_pool_task_server, start_link, [PoolId]},
            restart     => transient,
            shutdown    => 1000,
            type        => worker,
            modules     => [octopus_pool_task_server]
        },
        #{
            id          => octopus_pool_config_server,
            start       => {octopus_pool_config_server, start_link, [PoolId]},
            restart     => transient,
            shutdown    => 1000,
            type        => worker,
            modules     => [octopus_pool_config_server]
        }
    ],
    {ok, {{one_for_all, 5, 1}, Procs}}.
