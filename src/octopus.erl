-module(octopus).

%% API
-export([start_pool/3, stop_pool/1]).
-export([worker_lockout/1, worker_lockin/1]).
-export([perform/2]).
-export([get_env/2, set_env/2]).
-export([get_pool_config/1, set_pool_config/3, set_pool_config/4]).

%% API
-spec start_pool(PoolId, PoolOpts, WorkerOpts) -> ok | {error, Reason}
when
    PoolId      :: atom(),
    PoolOpts    :: proplists:proplist(),
    WorkerOpts  :: proplists:proplist(),
    Reason      :: term().

start_pool(PoolId, PoolOpts, WorkerOpts) ->
    ok = add_pool_config(PoolId, PoolOpts, WorkerOpts),
    case octopus_sup:start_pool(PoolId) of
        {ok, _Pid}                          -> ok;
        {ok, _Pid, _Info}                   -> ok;
        {error, {already_started, _Pid}}    -> {error, already_started};
        Error                               -> Error
    end.


-spec stop_pool(PoolId) -> ok
when
    PoolId  :: atom().

stop_pool(PoolId) ->
    _ = octopus_sup:stop_pool(PoolId),
    _ = delete_pool_config(PoolId),
    ok.


-spec worker_lockout(PoolId) -> {ok, Pid} | {error, Reason}
when
    PoolId  :: atom(),
    Pid     :: pid(),
    Reason  :: term().

worker_lockout(PoolId) ->
   octopus_pool_task_server:worker_lockout(PoolId).


-spec worker_lockin(PoolId) -> ok
when
    PoolId  :: atom().

worker_lockin(PoolId) ->
   octopus_pool_task_server:worker_lockin(PoolId).


-spec perform(PoolId, Fun) -> FunResult | {error, Reason}
when
    PoolId      :: atom(),
    Fun         :: fun((pid()) -> term()),
    FunResult   :: term(),
    Reason      :: term().

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


-spec get_env(Key, Default) -> Value
when
    Key     :: atom(),
    Default :: term(),
    Value   :: term().

get_env(Key, Default) ->
    case application:get_env(?MODULE, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.


-spec set_env(Key, Value) -> ok
when
    Key     :: atom(),
    Value   :: term().

set_env(Key, Value) ->
    application:set_env(?MODULE, Key, Value).


-spec get_pool_config(PoolId) -> Tuple | false
when
    PoolId  :: atom(),
    Tuple   :: tuple().

get_pool_config(PoolId) ->
    Pools = octopus:get_env(pools, []),
    lists:keyfind(PoolId, 1, Pools).


-spec set_pool_config(PoolId, PoolOpts, WorkerOpts) -> ok
when
    PoolId      :: atom(),
    PoolOpts    :: proplists:proplist(),
    WorkerOpts  :: proplists:proplist().

set_pool_config(PoolId, PoolOpts, WorkerOpts) ->
    set_pool_config(PoolId, PoolOpts, WorkerOpts, []).


-spec set_pool_config(PoolId, PoolOpts, WorkerOpts, ChangeOpts) -> ok
when
    PoolId      :: atom(),
    PoolOpts    :: proplists:proplist(),
    WorkerOpts  :: proplists:proplist(),
    ChangeOpts  :: proplists:proplist().

set_pool_config(PoolId, PoolOpts, WorkerOpts, ChangeOpts) ->
    ok = add_pool_config(PoolId, PoolOpts, WorkerOpts),
    octopus_pool_config_server:config_change(PoolId, ChangeOpts).

%% internal
add_pool_config(PoolId, PoolOpts, WorkerOpts) ->
    Pools = octopus:get_env(pools, []),
    PoolCfg = {PoolId, PoolOpts, WorkerOpts},
    Pools2 = lists:keystore(PoolId, 1, Pools, PoolCfg),
    octopus:set_env(pools, [PoolCfg|Pools2]).

delete_pool_config(PoolId) ->
    Pools = get_env(pools, []),
    Pools2 = lists:keydelete(PoolId, 1, Pools),
    octopus:set_env(pools, Pools2).
