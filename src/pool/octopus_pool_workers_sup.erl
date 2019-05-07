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
    SupName = octopus_name_resolver:get(PoolId, ?MODULE),
    supervisor:start_link({local, SupName}, ?MODULE, []).


-spec start_worker(PoolId, WorkerId) -> ok
when
    PoolId      :: atom(),
    WorkerId    :: term().

start_worker(PoolId, WorkerId) ->
    SupName = octopus_name_resolver:get(PoolId, ?MODULE),
    Spec = {WorkerId,
        {octopus_pool_task_server, worker_start, [PoolId, WorkerId]},
        transient, 1000, worker, [octopus_pool_task_server]},
    {ok, _} = supervisor:start_child(SupName, Spec),
    ok.


%% TODO soft stop
-spec stop_worker(PoolId, WorkerId) -> ok
when
    PoolId      :: atom(),
    WorkerId    :: term().

stop_worker(PoolId, WorkerId) ->
    SupName = octopus_name_resolver:get(PoolId, ?MODULE),
    _ = supervisor:terminate_child(SupName, WorkerId),
    _ = supervisor:delete_child(SupName, WorkerId),
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
-spec init([PoolId]) -> {ok, {{Strategy, MaxR, MaxT}, [ChildSpec]}}
when
    PoolId      :: atom(),
    Strategy    :: supervisor:strategy(),
    MaxR        :: non_neg_integer(),
    MaxT        :: pos_integer(),
    ChildSpec   :: supervisor:child_spec().

init([]) ->
    Procs = [],
    {ok, {{one_for_one, 100, 1}, Procs}}.
