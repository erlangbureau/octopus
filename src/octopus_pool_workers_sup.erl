-module(octopus_pool_workers_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([start_worker/2]).
-export([stop_worker/2]).
-export([restart_worker/2]).

%% supervisor callbacks
-export([init/1]).

%% API
start_link(PoolId) ->
	supervisor:start_link(?MODULE, [PoolId]).

start_worker(PoolId, WorkerId) ->
    {ok, WorkersSupPid} = octopus_namespace:lookup({?MODULE, PoolId}),
    {WorkerModule, WorkerOpts} = get_worker_config(PoolId, WorkerId),
    WorkerMFA = {WorkerModule, start_link, [WorkerOpts]},
    Spec = {WorkerId,
        {octopus_pool_task_server, worker_init, [PoolId, WorkerId, WorkerMFA]},
		transient, infinity, worker, [WorkerModule]},
    {ok, _} = supervisor:start_child(WorkersSupPid, Spec),
    ok.

%% TODO soft stop
stop_worker(PoolId, WorkerId) ->
    {ok, WorkersSupPid} = octopus_namespace:lookup({?MODULE, PoolId}),
    _ = supervisor:terminate_child(WorkersSupPid, WorkerId),
    _ = supervisor:delete_child(WorkersSupPid, WorkerId),
    ok.

%% TODO soft restart
restart_worker(PoolId, WorkerId) ->
    ok = stop_worker(PoolId, WorkerId),
    ok = start_worker(PoolId, WorkerId),
    ok.

%% supervisor callbacks
init([PoolId]) ->
    ok = octopus_namespace:register({?MODULE, PoolId}),
    Procs = [],
	{ok, {{one_for_one, 1, 1}, Procs}}.

%% internal
get_worker_config(PoolId, WorkerId) ->
    {PoolId, PoolOpts, WorkerOpts} = octopus:get_pool_config(PoolId),
    InitType = proplists:get_value(init_type, PoolOpts, sync),
    WorkerModule = proplists:get_value(worker, PoolOpts),
    WorkerOpts2 = [
        {pool_id, PoolId}, 
        {worker_id, WorkerId}, 
        {init_type, InitType},
        {ready_mfa, {octopus_pool_task_server, ready, [PoolId, WorkerId]}}
        |WorkerOpts],
    {WorkerModule, WorkerOpts2}.
