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
    worker_args = []
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
    {ok, ServerPid} = octopus_pool_processes_cache:lookup({?MODULE, PoolId}),
    gen_server:cast(ServerPid, {config_change, Opts}).

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
    ok = octopus_pool_processes_cache:register({?MODULE, PoolId}),
    State = case octopus:get_pool_config(PoolId) of
        {PoolId, PoolOpts, WorkerArgs} ->
            ok = config_change(PoolId, [], [], PoolOpts, WorkerArgs),
            #state{
                pool_id = PoolId,
                pool_opts = PoolOpts,
                worker_args = WorkerArgs
            };
        _ ->
            #state{pool_id = PoolId}
    end,
    {ok, State}.


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
        pool_opts = OldPoolOpts, worker_args = OldWorkerArgs}) ->
    State2 = case octopus:get_pool_config(PoolId) of
        {PoolId, NewPoolOpts, NewWorkerArgs} ->
            ok = config_change(PoolId, OldPoolOpts, OldWorkerArgs,
                NewPoolOpts, NewWorkerArgs),
            State#state{pool_opts = NewPoolOpts, worker_args = NewWorkerArgs};
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
config_change(PoolId, OldPoolOpts, OldWorkerArgs, NewPoolOpts, NewWorkerArgs) ->
    OldPoolSize = proplists:get_value(pool_size, OldPoolOpts, 0),
    NewPoolSize = proplists:get_value(pool_size, NewPoolOpts, 0),
    %% PoolSizeChange
    ok = pool_size_change(PoolId, OldPoolSize, NewPoolSize),
    %% WorkerConfigChange
    OldWorkerModule = proplists:get_value(worker, OldPoolOpts),
    NewWorkerModule = proplists:get_value(worker, NewPoolOpts),
    WorkerModuleChanged = NewWorkerModule =/= OldWorkerModule,
    WorkerArgsChanged = NewWorkerArgs =/= OldWorkerArgs,
    WorkerConfigChanged = WorkerModuleChanged orelse WorkerArgsChanged,
    _ = [octopus_pool_workers_sup:restart_worker(PoolId, WorkerId)
        || WorkerId <- lists:seq(1, OldPoolSize), WorkerConfigChanged],
    ok.

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
