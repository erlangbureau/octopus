Octopus
============

Octopus is a small and flexible pool manager written in Erlang.

Getting Started
===============

```erl

%% Start pool
1> octopus:start_pool(test_pool, [{pool_size, 3}, {worker, jamdb_sybase}], Opts).
ok

%% Execute task
2> {ok, Pid} = octopus:worker_lockout(test_pool),
{ok,<0.120.0>}
3> jamdb_sybase:sql_query(Pid, "select 1 as one, 2 as two, 3 as three") end).
{ok,[{result_set,[<<"one">>,<<"two">>,<<"three">>],
                 [],
                 [[1,2,3]]}]}
4> octopus:worker_lockin(test_pool).
ok

```

Project Chat Room
=================
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/erlangbureau/octopus?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

