-module(octopus).

%% API
-export([start_pool/3, stop_pool/1]).
-export([worker_lockout/1, worker_lockin/1]).
-export([perform/2]).
-export([get_pool_config/1, set_pool_config/3, set_pool_config/4]).
-export([pool_info/1, pool_info/2]).
-export([get_group/1, set_group/2]).
-export([map/2, reduce/3]).

%% API
-spec start_pool(PoolId, PoolOpts, WorkerOpts) -> ok | {error, Reason}
when
    PoolId      :: atom(),
    PoolOpts    :: proplists:proplist(),
    WorkerOpts  :: proplists:proplist(),
    Reason      :: term().

start_pool(PoolId, PoolOpts, WorkerOpts) when
        is_list(PoolOpts), is_list(WorkerOpts) ->
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


-spec pool_info(PoolId) -> [{Item, non_neg_integer()}]
when
    PoolId      :: atom(),
    Item    :: init | ready | busy.

pool_info(PoolId) ->
    octopus_pool_task_server:pool_info(PoolId).


-spec pool_info(PoolId, Item) -> {Item, non_neg_integer()}
when
    PoolId  :: atom(),
    Item    :: init | ready | busy.

pool_info(PoolId, Item) ->
    octopus_pool_task_server:pool_info(PoolId, Item).


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


-spec get_pool_config(PoolId) -> Tuple | false
when
    PoolId  :: atom(),
    Tuple   :: tuple().

get_pool_config(PoolId) ->
    Pools = get_env(pools, []),
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


-spec get_group(GroupId) -> PoolList when
    GroupId     :: atom(),
    PoolList    :: list().

get_group(GroupId) ->
    Groups = get_env(groups, []),
    proplists:get_value(GroupId, Groups, []).


-spec set_group(GroupId, PoolList) -> ok when
    GroupId     :: atom(),
    PoolList    :: list().

set_group(GroupId, PoolList) ->
    Groups = get_env(groups, []),
    Groups2 = lists:keystore(GroupId, 1, Groups, {GroupId, PoolList}),
    set_env(groups, Groups2).


-spec map(GroupId, Fun) -> list() when
    GroupId :: atom(),
    PoolId  :: atom(),
    Fun     :: fun((PoolId) -> term()).

map(GroupId, Fun) ->
    PoolList = get_group(GroupId),
    [Fun(PoolId) || PoolId <- PoolList].

-spec reduce(GroupId, Fun, InitState) -> term() when
    GroupId     :: atom(),
    PoolId      :: atom(),
    InitState   :: term(),
    Fun         :: fun((PoolId, InitState) -> term()).

reduce(GroupId, Fun, InitState) ->
    PoolList = get_group(GroupId),
    lists:foldl(Fun, InitState, PoolList).

%% internal
add_pool_config(PoolId, PoolOpts, WorkerOpts) ->
    Pools = get_env(pools, []),
    PoolCfg = {PoolId, PoolOpts, WorkerOpts},
    Pools2 = lists:keystore(PoolId, 1, Pools, PoolCfg),
    set_env(pools, [PoolCfg|Pools2]).

delete_pool_config(PoolId) ->
    Pools = get_env(pools, []),
    Pools2 = lists:keydelete(PoolId, 1, Pools),
    set_env(pools, Pools2).

get_env(Key, Default) ->
    case application:get_env(?MODULE, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

set_env(Key, Value) ->
    application:set_env(?MODULE, Key, Value).
