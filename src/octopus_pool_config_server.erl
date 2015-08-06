-module(octopus_pool_config_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([config_change/2]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-record(state, {
    pool_id,
    pool_opts = [],
    worker_opts = []
}).

%% API
-spec start_link(PoolId) -> {ok, Pid}
when
    PoolId  :: atom(),
    Pid     :: pid().

start_link(PoolId) ->
    gen_server:start_link(?MODULE, [PoolId], []).


-spec config_change(PoolId, Opts) -> ok
when
    PoolId  :: atom(),
    Opts    :: proplists:proplist().

config_change(PoolId, Opts) ->
    {ok, ConfigServerPid} = octopus_namespace:lookup({?MODULE, PoolId}),
    gen_server:cast(ConfigServerPid, {config_change, Opts}).

%% gen_server callbacks
-spec init(Opts) -> {ok, State} |
                    {ok, State, Timeout} |
                    ignore |
                    {stop, Reason}
when
    Opts    :: list(),
    State   :: term(),
    Timeout :: timeout(),
    Reason  :: term().

init([PoolId]) ->
    ok = octopus_namespace:register({?MODULE, PoolId}),
    ok = config_change(PoolId, []),
    {ok, #state{pool_id = PoolId, pool_opts = [{pool_size, 0}]}}.


-spec handle_call(Request, From, State) ->  {reply, Reply, State} |
                                            {reply, Reply, State, Timeout} |
                                            {noreply, State} |
                                            {noreply, State, Timeout} |
                                            {stop, Reason, Reply, State}
when
    Request :: term(),
    From    :: {pid(), term()},
    State   :: term(),
    Reply   :: term(),
    Timeout :: timeout(),
    Reason  :: normal | shutdown.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


-spec handle_cast(Request, State) ->    {noreply, State} |
                                        {noreply, State, Timeout} |
                                        {stop, Reason, State}
when
    Request :: term(),
    State   :: term(),
    Timeout :: timeout(),
    Reason  :: normal | shutdown.

handle_cast({config_change, _Opts}, State = #state{pool_id = PoolId,
        pool_opts = OldPoolOpts, worker_opts = OldWorkerOpts}) ->
    State2 = case octopus:get_pool_config(PoolId) of
        {PoolId, NewPoolOpts, NewWorkerOpts} ->
            OldPoolSize = proplists:get_value(pool_size, OldPoolOpts),
            NewPoolSize = proplists:get_value(pool_size, NewPoolOpts),
            %% PoolSizeChange
            ok = pool_size_change(PoolId, OldPoolSize, NewPoolSize),
            %% WorkerConfigChange
            OldWorkerModule = proplists:get_value(worker, OldPoolOpts),
            NewWorkerModule = proplists:get_value(worker, NewPoolOpts),
            WorkerModuleChanged = NewWorkerModule =/= OldWorkerModule,
            WorkerOptsChanged = NewWorkerOpts =/= OldWorkerOpts,
            WorkerConfigChanged = WorkerModuleChanged orelse WorkerOptsChanged,
            _ = [octopus_pool_workers_sup:restart_worker(PoolId, WorkerId)
                || WorkerId <- lists:seq(1, NewPoolSize), WorkerConfigChanged],
            State#state{pool_opts = NewPoolOpts, worker_opts = NewWorkerOpts};
        _ ->
            State
    end,
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.


-spec handle_info(Request, State) ->    {noreply, State} |
                                        {noreply, State, Timeout} |
                                        {stop, Reason, State}
when
    Request :: term(),
    State   :: term(),
    Timeout :: timeout(),
    Reason  :: normal | shutdown.

handle_info(_Info, State) ->
    {noreply, State}.


-spec terminate(Reason, State) -> ok
when
    Reason  :: term(),
    State   :: term().

terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn, State, Extra) -> {ok, State} | {error, Reason}
when
    OldVsn  :: term() | {down, term()},
    State   :: term(),
    Extra   :: term(),
    Reason  :: term().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
pool_size_change(PoolId, OldSize, NewSize) when OldSize =< NewSize ->
    %% when OldSize less or equal NewSize then try to start new workers
    [begin
        ok = octopus_pool_workers_sup:start_worker(PoolId, WorkerId)
    end || WorkerId <- lists:seq(OldSize + 1, NewSize)],
    ok;
pool_size_change(PoolId, OldSize, NewSize) ->
    %% when OldSize bigger NewSize then stop unnecessary workers
    [begin
        ok = octopus_pool_workers_sup:stop_worker(PoolId, WorkerId)
    end || WorkerId <- lists:seq(NewSize + 1, OldSize)],
    ok.
