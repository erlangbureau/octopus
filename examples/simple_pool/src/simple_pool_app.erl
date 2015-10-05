-module(simple_pool_app).
-behaviour(application).

%% API
-export([start/2]).
-export([stop/1]).

%% private
-export([sql_query/0]).

%% API
start(_Type, _Args) ->
    Opts = [
        {host, "jamdb-sybase-dev.erlangbureau.dp.ua"},
        {port, 5000},
        {user, "jamdbtest"},
        {password, "jamdbtest"},
        {database, "jamdbtest"}
    ],
    ok = octopus:start_pool(test_pool, [{pool_size, 3}, {worker, jamdb_sybase}], [Opts]),
    _ = timer:apply_interval(3000, ?MODULE, sql_query, []),
    _ = sql_query(),
	simple_pool_sup:start_link().

stop(_State) ->
	ok.

%% private
sql_query() ->
    PoolInfo = octopus:pool_info(test_pool),
    io:format("Pool Info before lockout: ~p~n", [PoolInfo]),
    
    {ok, Pid} = octopus:worker_lockout(test_pool),
    PoolInfo2 = octopus:pool_info(test_pool),
    io:format("Pool Info after lockout: ~p~n~n", [PoolInfo2]),

    Query = "select 1 as one, 2 as two, 3 as three",
    Result = jamdb_sybase:sql_query(Pid, "select 1 as one, 2 as two, 3 as three"),
    io:format("Execute query: ~p~n", [Query]),
    io:format("  Result: ~p~n~n", [Result]),
    
    ok = octopus:worker_lockin(test_pool),
    PoolInfo3 = octopus:pool_info(test_pool),
    io:format("Pool Info after lockin: ~p~n~n", [PoolInfo3]),
    io:format("==============================~n~n", []),
    Result.
