-module(octopus_app).
-behaviour(application).

%% application callbacks
-export([start/2]).
-export([stop/1]).

%% application callbacks
-spec start(StartType, StartArgs) -> {ok, Pid} |
                                     {ok, Pid, State} |
                                     {error, Reason}
when
    StartType   :: application:start_type(),
    StartArgs   :: term(),
    Pid         :: pid(),
    State       :: term(),
    Reason      :: term().

start(_StartType, _StartArgs) ->
    ok = octopus_namespace:init(),
    octopus_sup:start_link().


-spec stop(State) -> ok
when
    State   :: term().

stop(_State) ->
    ok.
