-module(octopus_pool_task_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([worker_init/2]).
-export([worker_ready/2]).
-export([worker_lockout/1]).
-export([worker_lockin/1]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-record(state, {
    pool_id,
    worker_init = sync,
    workers_init = [],
    workers_ready = [],
    workers_busy = []
}).

%% API
-spec start_link(PoolId) -> {ok, Pid}
when
    PoolId  :: atom(),
    Pid     :: pid().

start_link(PoolId) ->
    gen_server:start_link({local, PoolId}, ?MODULE, [PoolId], []).


-spec worker_init(PoolId, WorkerId) ->  {pk, Pid} |
                                        {ok, Pid, Extra} |
                                        {error, Reason} |
                                        term()
when
    PoolId      :: atom(),
    WorkerId    :: term(),
    Extra       :: term(),
    Reason      :: term().

worker_init(PoolId, WorkerId) ->
    {WorkerModule, WorkerOpts} = get_worker_config(PoolId, WorkerId),
    case apply(WorkerModule, start_link, [WorkerOpts]) of
        {ok, Pid} when is_pid(Pid) ->
            _ = PoolId ! {init, WorkerId, Pid},
            {ok, Pid};
        {ok, Pid, Extra} when is_pid(Pid) ->
            _ = PoolId ! {init, WorkerId, Pid},
            {ok, Pid, Extra};
        Other ->
            Other
    end.


-spec worker_ready(PoolId, WorkerId) -> ok
when
    PoolId      :: atom(),
    WorkerId    :: term().

worker_ready(PoolId, WorkerId) ->
    _ = PoolId ! {ready, WorkerId},
    ok.

-spec worker_lockout(PoolId) -> {ok, Pid}
when
    PoolId  :: atom(),
    Pid     :: pid().

worker_lockout(PoolId) ->
    gen_server:call(PoolId, worker_lockout).


-spec worker_lockin(PoolId) -> ok
when
    PoolId  :: atom().

worker_lockin(PoolId) ->
    gen_server:cast(PoolId, {worker_lockin, self()}).


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
    {PoolId, PoolOpts, _WorkerOpts} = octopus:get_pool_config(PoolId),
    WorkerInit = proplists:get_value(worker_init, PoolOpts, sync),
    {ok, #state{pool_id = PoolId, worker_init = WorkerInit}}.


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

handle_call(worker_lockout, {From, _}, #state{workers_ready = WorkersReady,
        workers_busy = WorkersBusy} = State) ->
    WorkersReady2 = lists:reverse(WorkersReady),
    case WorkersReady2 of
        [{WorkerId, Pid}|WorkersReady3] ->
            _ = erlang:monitor(process, From),
            WorkersReady4 = lists:reverse(WorkersReady3),
            WorkersBusy2 = lists:keystore(
                WorkerId, 1, WorkersBusy, {WorkerId, Pid, From}),
            State2 = State#state{
                workers_ready = WorkersReady4,
                workers_busy = WorkersBusy2
            },
            {reply, {ok, Pid}, State2};
        [] ->
            {reply, {error, no_workers}, State}
    end;
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

handle_cast({worker_lockin, From}, #state{workers_ready = WorkersReady,
        workers_busy = WorkersBusy} = State) ->
    {value, Tuple, WorkersBusy2} = lists:keytake(From, 3, WorkersBusy),
    {WorkerId, Pid, From} = Tuple,
    WorkersReady2 = lists:keystore(
        WorkerId, 1, WorkersReady, {WorkerId, Pid}),
    State2 = State#state{
        workers_ready = WorkersReady2,
        workers_busy = WorkersBusy2
    },
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

handle_info({init, WorkerId, Pid}, #state{worker_init = WorkerInit,
        workers_init = WorkersInit, workers_ready = WorkersReady} = State) ->
    _ = erlang:monitor(process, Pid),
    State2 = case WorkerInit of
        sync ->
            WorkersReady2 = lists:keystore(
                WorkerId, 1, WorkersReady, {WorkerId, Pid}),
            State#state{workers_ready = WorkersReady2};
        async ->
            WorkersReady2 = lists:keydelete(WorkerId, 1, WorkersReady),
            WorkersInit2 = lists:keystore(
                WorkerId, 1, WorkersInit, {WorkerId, Pid}),
            State#state{
                workers_init = WorkersInit2,
                workers_ready = WorkersReady2
            }
    end,
    {noreply, State2};
handle_info({ready, WorkerId}, #state{workers_init = WorkersInit,
        workers_ready = WorkersReady} = State) ->
    WorkersInit2 = lists:keytake(WorkerId, 1, WorkersInit),
    State2 = case lists:keytake(WorkerId, 1, WorkersInit) of
        {value, Tuple, WorkersInit2} ->
            WorkersReady2 = lists:keystore(WorkerId, 1, WorkersReady, Tuple),
            State#state{
                workers_init = WorkersInit2,
                workers_ready = WorkersReady2
            };
        false ->
            State
    end,
    {noreply, State2};
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, #state{
        workers_init = WorkersInit, workers_ready = WorkersReady,
        workers_busy = WorkersBusy} = State) ->
    State2 = case lists:keytake(Pid, 3, WorkersBusy) of
        {value, Tuple, WorkersBusy2} ->
            {WorkerId, WorkerPid, Pid} = Tuple,
            WorkersReady2 = lists:keystore(
                WorkerId, 1, WorkersReady, {WorkerId, WorkerPid}),
            State#state{
                workers_ready = WorkersReady2,
                workers_busy = WorkersBusy2
            };
        false ->
            WorkersInit2 = lists:keydelete(Pid, 2, WorkersInit),
            WorkersReady2 = lists:keydelete(Pid, 2, WorkersReady),
            WorkersBusy2 = lists:keydelete(Pid, 2, WorkersBusy),
            State#state{
                workers_init = WorkersInit2,
                workers_ready = WorkersReady2,
                workers_busy = WorkersBusy2
            }
    end,
    {noreply, State2};
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
get_worker_config(PoolId, WorkerId) ->
    {PoolId, PoolOpts, WorkerOpts} = octopus:get_pool_config(PoolId),
    InitType = proplists:get_value(init_type, PoolOpts, sync),
    WorkerModule = proplists:get_value(worker, PoolOpts),
    WorkerOpts2 = [
        {pool_id, PoolId},
        {worker_id, WorkerId},
        {init_type, InitType},
        {ready_callback, {?MODULE, ready, [PoolId, WorkerId]}}
        |WorkerOpts],
    {WorkerModule, WorkerOpts2}.
