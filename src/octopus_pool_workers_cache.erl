-module(octopus_pool_workers_cache).

% API
-export([init/0]).
-export([lookup/1]).
-export([insert/2]).

%% API
-spec init() -> ok.

init() ->
    _ = ets:new(?MODULE, [named_table, public]),
    ok.

-spec insert(Key, Value) -> ok
when
    Key     :: term(),
    Value   :: term().

insert(Key, Value) ->
    true = ets:insert(?MODULE, {Key, Value}),
    ok.

-spec lookup(Key) -> ok
when
    Key     :: term().

lookup(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, List}] -> List;
        _ -> []
    end.
