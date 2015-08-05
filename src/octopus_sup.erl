-module(octopus_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_pool/1, stop_pool/1]).

%% supervisor callbacks
-export([init/1]).

%% API
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_pool(PoolId) ->
    Spec = {PoolId,
        {octopus_pool_sup, start_link, [PoolId]},
		transient, 5000, supervisor, [octopus_pool_sup]},
    supervisor:start_child(?MODULE, Spec).

stop_pool(PoolId) ->
    _ = supervisor:terminate_child(?MODULE, PoolId),
    _ = supervisor:delete_child(?MODULE, PoolId),
    ok.

%% supervisor callbacks
init([]) ->
	{ok, {{one_for_one, 1, 5}, []}}.
