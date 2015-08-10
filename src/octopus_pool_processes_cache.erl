-module(octopus_pool_processes_cache).

%% API
-export([init/0]).
-export([register/1]).
-export([lookup/1]).

%% API
-spec init() -> ok.

init() ->
    _ = ets:new(?MODULE, [named_table, public]),
    ok.

-spec register(Name) -> ok
when
    Name :: term().

register(Name) ->
    Pid = self(),
    true = ets:insert(?MODULE, {Name, Pid}),
    ok.


-spec lookup(Name) -> Result
when
    Name    :: term(),
    Result  :: {ok, pid()} | {error, any()}.

lookup(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, Pid}] -> {ok, Pid};
        _   -> {error, no_instance}
    end.
