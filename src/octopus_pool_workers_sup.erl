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
-spec start_link(PoolId) -> {ok, Pid}
when
    PoolId  :: atom(),
    Pid     :: pid().

start_link(PoolId) ->
    supervisor:start_link(?MODULE, [PoolId]).


-spec start_worker(PoolId, WorkerId) -> ok
when
    PoolId      :: atom(),
    WorkerId    :: term().

start_worker(PoolId, WorkerId) ->
    {ok, SupPid} = octopus_pool_processes_cache:lookup({?MODULE, PoolId}),
    Spec = {WorkerId,
        {octopus_pool_task_server, worker_start, [PoolId, WorkerId]},
        transient, 1000, worker, [octopus_pool_task_server]},
    {ok, _} = supervisor:start_child(SupPid, Spec),
    ok.


%% TODO soft stop
-spec stop_worker(PoolId, WorkerId) -> ok
when
    PoolId      :: atom(),
    WorkerId    :: term().

stop_worker(PoolId, WorkerId) ->
    {ok, SupPid} = octopus_pool_processes_cache:lookup({?MODULE, PoolId}),
    _ = supervisor:terminate_child(SupPid, WorkerId),
    _ = supervisor:delete_child(SupPid, WorkerId),
    ok.


%% TODO soft restart
-spec restart_worker(PoolId, WorkerId) -> ok
when
    PoolId      :: atom(),
    WorkerId    :: term().

restart_worker(PoolId, WorkerId) ->
    ok = stop_worker(PoolId, WorkerId),
    ok = start_worker(PoolId, WorkerId),
    ok.


%% supervisor callbacks
-spec init(Opts) -> {ok, {Strategy, MaxR, MaxT}, [ChildSpec]}
when
    Opts        :: list(),
    Strategy    :: supervisor:strategy(),
    MaxR        :: non_neg_integer(),
    MaxT        :: pos_integer(),
    ChildSpec   :: supervisor:child_spec().

init([PoolId]) ->
    ok = octopus_pool_processes_cache:register({?MODULE, PoolId}),
    Procs = [],
    {ok, {{one_for_one, 100, 1}, Procs}}.
