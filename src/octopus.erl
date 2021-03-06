-module(octopus).

% API
-export([start_pool/3]).
-export([stop_pool/1]).
-export([worker_lockout/1, worker_lockout/2]).
-export([worker_lockin/1]).
-export([perform/2, perform/3]).
-export([get_pool_config/1, set_pool_config/3, set_pool_config/4]).
-export([pool_info/1, pool_info/2]).
-export([get_group/1, set_group/2]).
-export([map/2, reduce/3]).

-define(DEFAULT_TIMEOUT, 5000).

% API
-spec start_pool(PoolId, PoolOpts, WorkerArgs) -> ok | {error, Reason}
when
    PoolId      :: atom(),
    PoolOpts    :: proplists:proplist(),
    WorkerArgs  :: proplists:proplist(),
    Reason      :: term().

start_pool(PoolId, PoolOpts, WorkerArgs) when
        is_atom(PoolId), is_list(PoolOpts), is_list(WorkerArgs) ->
    ok = add_pool_config(PoolId, PoolOpts, WorkerArgs),
    case octopus_sup:start_pool(PoolId) of
        {ok, _Pid}                          -> ok;
        {ok, _Pid, _Info}                   -> ok;
        {error, {already_started, _Pid}}    -> {error, already_started};
        Error                               -> Error
    end.


-spec stop_pool(PoolId) -> ok
when
    PoolId  :: atom().

stop_pool(PoolId) when is_atom(PoolId) ->
    _ = octopus_sup:stop_pool(PoolId),
    _ = delete_pool_config(PoolId),
    ok.


-spec pool_info(PoolId) -> #{Item => non_neg_integer()}
when
    PoolId  :: atom(),
    Item    :: init | ready | busy.

pool_info(PoolId) when is_atom(PoolId) ->
    octopus_pool_task_server:pool_info(PoolId).


-spec pool_info(PoolId, Item) -> non_neg_integer()
when
    PoolId  :: atom(),
    Item    :: init | ready | busy.

pool_info(PoolId, Item) when is_atom(PoolId) ->
    Info = octopus_pool_task_server:pool_info(PoolId),
    maps:get(Item, Info).


-spec worker_lockout(PoolId) -> {ok, Pid} | {error, Reason}
when
    PoolId  :: atom(),
    Pid     :: pid(),
    Reason  :: term().

worker_lockout(PoolId) when is_atom(PoolId) ->
   worker_lockout(PoolId, ?DEFAULT_TIMEOUT).

-spec worker_lockout(PoolId, Timeout) -> {ok, Pid} | {error, Reason}
when
    PoolId  :: atom(),
    Timeout :: timeout(),
    Pid     :: pid(),
    Reason  :: term().

worker_lockout(PoolId, Timeout) when is_atom(PoolId) ->
   octopus_pool_task_server:worker_lockout(PoolId, Timeout).


-spec worker_lockin(PoolId) -> ok
when
    PoolId  :: atom().

worker_lockin(PoolId) when is_atom(PoolId) ->
   octopus_pool_task_server:worker_lockin(PoolId).


-spec perform(PoolId, Fun) -> FunResult | {error, Reason}
when
    PoolId      :: atom(),
    Fun         :: fun((pid()) -> term()),
    FunResult   :: term(),
    Reason      :: term().

perform(PoolId, Fun) when is_atom(PoolId) ->
    perform(PoolId, Fun, ?DEFAULT_TIMEOUT).

-spec perform(PoolId, Fun, Timeout) -> FunResult | {error, Reason}
when
    PoolId      :: atom(),
    Fun         :: fun((pid()) -> term()),
    Timeout     :: timeout(),
    FunResult   :: term(),
    Reason      :: term().

perform(PoolId, Fun, Timeout) ->
    case worker_lockout(PoolId, Timeout) of
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

get_pool_config(PoolId) when is_atom(PoolId) ->
    Pools = application:get_env(?MODULE, pools, []),
    lists:keyfind(PoolId, 1, Pools).


-spec set_pool_config(PoolId, PoolOpts, WorkerArgs) -> ok
when
    PoolId      :: atom(),
    PoolOpts    :: proplists:proplist(),
    WorkerArgs  :: proplists:proplist().

set_pool_config(PoolId, PoolOpts, WorkerArgs) when is_atom(PoolId) ->
    set_pool_config(PoolId, PoolOpts, WorkerArgs, []).


-spec set_pool_config(PoolId, PoolOpts, WorkerArgs, ChangeOpts) -> ok
when
    PoolId      :: atom(),
    PoolOpts    :: proplists:proplist(),
    WorkerArgs  :: proplists:proplist(),
    ChangeOpts  :: proplists:proplist().

set_pool_config(PoolId, PoolOpts, WorkerArgs, ChangeOpts) when is_atom(PoolId) ->
    ok = add_pool_config(PoolId, PoolOpts, WorkerArgs),
    octopus_pool_config_server:config_change(PoolId, ChangeOpts).


-spec get_group(GroupId) -> PoolList when
    GroupId     :: atom(),
    PoolList    :: list().

get_group(GroupId) ->
    Groups = application:get_env(?MODULE, groups, []),
    proplists:get_value(GroupId, Groups, []).


-spec set_group(GroupId, PoolList) -> ok when
    GroupId     :: atom(),
    PoolList    :: list().

set_group(GroupId, PoolList) ->
    Groups = application:get_env(?MODULE, groups, []),
    Groups2 = lists:keystore(GroupId, 1, Groups, {GroupId, PoolList}),
    application:set_env(?MODULE, groups, Groups2).


-spec map(GroupId, Fun) -> list() when
    GroupId :: atom(),
    PoolId  :: atom(),
    Fun     :: fun((PoolId) -> term()).

map(GroupId, Fun) when is_function(Fun) ->
    PoolList = get_group(GroupId),
    [Fun(PoolId) || PoolId <- PoolList].

-spec reduce(GroupId, Fun, InitState) -> term() when
    GroupId     :: atom(),
    PoolId      :: atom(),
    InitState   :: term(),
    Fun         :: fun((PoolId, InitState) -> term()).

reduce(GroupId, Fun, InitState) when is_function(Fun) ->
    PoolList = get_group(GroupId),
    lists:foldl(Fun, InitState, PoolList).

% internal
add_pool_config(PoolId, PoolOpts, WorkerArgs) ->
    Pools = application:get_env(?MODULE, pools, []),
    PoolCfg = {PoolId, PoolOpts, WorkerArgs},
    Pools2 = lists:keystore(PoolId, 1, Pools, PoolCfg),
    application:set_env(?MODULE, pools, Pools2).

delete_pool_config(PoolId) ->
    Pools = application:get_env(?MODULE, pools, []),
    Pools2 = lists:keydelete(PoolId, 1, Pools),
    application:set_env(?MODULE, pools, Pools2).
