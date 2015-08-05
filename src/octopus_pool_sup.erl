-module(octopus_pool_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%% API
start_link(PoolId) ->
	supervisor:start_link(?MODULE, [PoolId]).

%% supervisor callbacks
init([PoolId]) ->
    Procs = [
        {octopus_pool_workers_sup, {octopus_pool_workers_sup, start_link, [PoolId]},
		    transient, infinity, supervisor, [octopus_pool_workers_sup]},
        {octopus_pool_config_server, {octopus_pool_config_server, start_link, [PoolId]},
		    transient, infinity, worker, [octopus_pool_config_server]},
        {octopus_pool_task_server, {octopus_pool_task_server, start_link, [PoolId]},
		    transient, infinity, worker, [octopus_pool_task_server]}
    ],
	{ok, {{one_for_all, 1, 5}, Procs}}.
