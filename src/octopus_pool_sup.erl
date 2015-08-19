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
-spec init([PoolId]) -> {ok, {{Strategy, MaxR, MaxT}, [ChildSpec]}}
when
    PoolId      :: atom(),
    Strategy    :: supervisor:strategy(),
    MaxR        :: non_neg_integer(),
    MaxT        :: pos_integer(),
    ChildSpec   :: supervisor:child_spec().

init([PoolId]) ->
    Procs = [
        {octopus_pool_workers_sup,
            {octopus_pool_workers_sup, start_link, [PoolId]},
            transient, infinity, supervisor, [octopus_pool_workers_sup]},
        {octopus_pool_config_server,
            {octopus_pool_config_server, start_link, [PoolId]},
            transient, 1000, worker, [octopus_pool_config_server]},
        {octopus_pool_task_server,
            {octopus_pool_task_server, start_link, [PoolId]},
            transient, 1000, worker, [octopus_pool_task_server]}
    ],
    {ok, {{one_for_all, 5, 1}, Procs}}.
