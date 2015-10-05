Simple pool example
===================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the console mode:

``` bash
$ ./_rel/simple_pool_example/bin/simple_pool_example console
```

Example output
--------------

```
Pool Info before lockout: [{init,0},{ready,3},{busy,0}]
Pool Info after lockout: [{init,0},{ready,2},{busy,1}]

Execute query: "select 1 as one, 2 as two, 3 as three"
  Result: {ok,[{result_set,[<<"one">>,<<"two">>,<<"three">>],[],[[1,2,3]]}]}

Pool Info after lockin: [{init,0},{ready,3},{busy,0}]
```
