-module(octopus_app).
-behaviour(application).

%% application callbacks
-export([start/2]).
-export([stop/1]).

%% application callbacks
start(_StartType, _StartArgs) ->
    Result = octopus_sup:start_link(),
    ok = start_pools_from_config(),
    Result.

stop(_State) ->
    ok.

%% internal
start_pools_from_config() ->
    Pools = application:get_env(octopus, pools, []),
    _ = [ok = octopus:start_pool(PoolName, PoolOpts, WorkerOpts)
        || {PoolName, PoolOpts, WorkerOpts} <- Pools],
    ok.
