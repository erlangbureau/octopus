-module(octopus_app).
-behaviour(application).

%% application callbacks
-export([start/2]).
-export([stop/1]).

%% application callbacks
-spec start(StartType, StartArgs) -> {ok, Pid} |
                                     {ok, Pid, State} |
                                     {error, Reason}
when
    StartType   :: application:start_type(),
    StartArgs   :: term(),
    Pid         :: pid(),
    State       :: term(),
    Reason      :: term().

start(_StartType, _StartArgs) ->
    ok = octopus_pool_processes_cache:init(),
    ok = octopus_pool_workers_cache:init(),
    Result = octopus_sup:start_link(),
    ok = start_pools(),
    ok = create_groups(),
    Result.


-spec stop(State) -> ok
when
    State   :: term().

stop(_State) ->
    ok.

%% internal
start_pools() ->
    {ok, Pools} = application:get_env(octopus, pools),
    _ = [ok = octopus:start_pool(PoolName, PoolOpts, WorkerOpts)
        || {PoolName, PoolOpts, WorkerOpts} <- Pools],
    ok.

create_groups() ->
    {ok, Groups} = application:get_env(octopus, groups),
    _ = [ok = octopus:set_group(Group, PoolList)
        || {Group, PoolList} <- Groups],
    ok.
