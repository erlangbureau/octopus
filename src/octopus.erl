-module(octopus).

%% API
-export([start_pool/3, stop_pool/1]).
-export([worker_lockout/1, worker_lockin/1]).
-export([perform/2]).
-export([get_env/2, set_env/2]).
-export([get_pool_config/1]).

%% API
start_pool(PoolId, PoolOpts, WorkerOpts) ->
    case is_started(PoolId) of
        true ->
            {error, already_started};
        false ->
            ok = add_pool_config(PoolId, PoolOpts, WorkerOpts),
            case octopus_sup:start_pool(PoolId) of
                {ok, _Pid} -> ok;
                Error -> Error
            end
    end.

stop_pool(PoolId) ->
    _ = octopus_sup:stop_pool(PoolId),
    _ = delete_pool_config(PoolId),
    ok.

worker_lockout(PoolId) ->
   octopus_pool_task_server:worker_lockout(PoolId).

worker_lockin(PoolId) ->
   octopus_pool_task_server:worker_lockin(PoolId).

perform(PoolId, Fun) ->
    case worker_lockout(PoolId) of
        {ok, Pid} ->
            try
                Fun(Pid)
            after
                ok = worker_lockin(PoolId)
            end;
        Error ->
            Error
    end.

get_env(Key, Default) ->
    case application:get_env(?MODULE, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

set_env(Key, Value) ->
    application:set_env(?MODULE, Key, Value).

get_pool_config(PoolId) ->
    Pools = octopus:get_env(pools, []),
    lists:keyfind(PoolId, 1, Pools).

%% internal
add_pool_config(PoolId, PoolOpts, WorkerOpts) ->
    Pools = octopus:get_env(pools, []),
    Pools2 = lists:keydelete(PoolId, 1, Pools),
    PoolCfg = {PoolId, PoolOpts, WorkerOpts},
    octopus:set_env(pools, [PoolCfg|Pools2]).

delete_pool_config(PoolId) ->
    Pools = get_env(pools, []),
    Pools2 = lists:keydelete(PoolId, 1, Pools),
    octopus:set_env(pools, Pools2).

is_started(PoolId) ->
    Pools = get_env(pools, []),
    lists:keymember(PoolId, 1, Pools).
