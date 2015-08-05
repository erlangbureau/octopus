-module(octopus_namespace).

%% API
-export([init/0]).
-export([register/1]).
-export([lookup/1]).

%% API
init() ->
    _ = ets:new(?MODULE, [named_table, public]),
    ok.

register(Name) ->
    Pid = self(),
    true = ets:insert(?MODULE, {Name, Pid}),
    ok.

lookup(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, Pid}] -> {ok, Pid};
        _   -> {error, no_instance}
    end.
