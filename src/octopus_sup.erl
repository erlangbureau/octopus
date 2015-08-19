-module(octopus_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_pool/1, stop_pool/1]).

%% supervisor callbacks
-export([init/1]).

%% API
-spec start_link() -> {ok, Pid}
when
    Pid     :: pid().

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec start_pool(PoolId) -> Result
when
    PoolId  :: atom(),
    Result  :: supervisor:startchild_ret().

start_pool(PoolId) ->
    Spec = {PoolId,
        {octopus_pool_sup, start_link, [PoolId]},
        transient, 5000, supervisor, [octopus_pool_sup]},
    supervisor:start_child(?MODULE, Spec).


-spec stop_pool(PoolId) -> ok
when
    PoolId  :: atom().

stop_pool(PoolId) ->
    _ = supervisor:terminate_child(?MODULE, PoolId),
    _ = supervisor:delete_child(?MODULE, PoolId),
    ok.


%% supervisor callbacks
-spec init([]) -> {ok, {{Strategy, MaxR, MaxT}, [ChildSpec]}}
when
    Strategy    :: supervisor:strategy(),
    MaxR        :: non_neg_integer(),
    MaxT        :: pos_integer(),
    ChildSpec   :: supervisor:child_spec().

init([]) ->
    {ok, {{one_for_one, 1, 5}, []}}.
