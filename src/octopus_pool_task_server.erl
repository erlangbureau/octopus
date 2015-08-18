-module(octopus_pool_task_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([worker_start/2]).
-export([worker_init/2]).
-export([worker_ready/2]).
-export([worker_lockout/1]).
-export([worker_lockin/1]).
-export([pool_info/1, pool_info/2]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-record(state, {
    pool_id,
    init_type = sync
}).

-define(DEF_INIT_TYPE, sync).

%% API
-spec start_link(PoolId) -> {ok, Pid}
when
    PoolId  :: atom(),
    Pid     :: pid().

start_link(PoolId) ->
    gen_server:start_link({local, PoolId}, ?MODULE, [PoolId], []).


-spec worker_start(PoolId, WorkerId) ->  {pk, Pid} |
                                        {ok, Pid, Extra} |
                                        {error, Reason} |
                                        term()
when
    PoolId      :: atom(),
    WorkerId    :: term(),
    Extra       :: term(),
    Reason      :: term().


worker_start(PoolId, WorkerId) ->
    {WorkerModule, WorkerArgs} = get_worker_config(PoolId, WorkerId),
    case apply(WorkerModule, start_link, WorkerArgs) of
        {ok, Pid} when is_pid(Pid) ->
            _ = PoolId ! {start, WorkerId, Pid},
            {ok, Pid};
        {ok, Pid, Extra} when is_pid(Pid) ->
            _ = PoolId ! {start, WorkerId, Pid},
            {ok, Pid, Extra};
        Other ->
            Other
    end.

-spec worker_init(PoolId, WorkerId) -> ok
when
    PoolId      :: atom(),
    WorkerId    :: term().

worker_init(PoolId, WorkerId) ->
    _ = PoolId ! {init, WorkerId},
    ok.

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

-spec pool_info(PoolId) -> [{Item, non_neg_integer()}]
when
    PoolId      :: atom(),
    Item    :: init | ready | busy.

pool_info(PoolId) ->
    gen_server:call(PoolId, pool_info).

-spec pool_info(PoolId, Item) -> {Item, non_neg_integer()}
when
    PoolId  :: atom(),
    Item    :: init | ready | busy.

pool_info(PoolId, Item) when Item =:= init; Item =:= ready; Item =:= busy ->
    ItemList = octopus_pool_workers_cache:lookup({PoolId, Item}),
    {Item, length(ItemList)}.

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
    {PoolId, PoolOpts, _WorkerArgs} = octopus:get_pool_config(PoolId),
    InitType = proplists:get_value(init_type, PoolOpts, ?DEF_INIT_TYPE),
    {ok, #state{pool_id = PoolId, init_type = InitType}}.


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

handle_call(worker_lockout, {From, _}, #state{pool_id = PoolId} = State) ->
    Ready = octopus_pool_workers_cache:lookup({PoolId, ready}),
    Busy = octopus_pool_workers_cache:lookup({PoolId, busy}),
    case Ready of
        [{WorkerId, Pid}|Ready2] ->
            _ = erlang:monitor(process, From),
            Busy2 = lists:keystore(WorkerId, 1, Busy, {WorkerId, Pid, From}),
            ok = octopus_pool_workers_cache:insert({PoolId, ready}, Ready2),
            ok = octopus_pool_workers_cache:insert({PoolId, busy}, Busy2),
            {reply, {ok, Pid}, State};
        [] ->
            {reply, {error, no_workers}, State}
    end;
handle_call(pool_info, _From, #state{pool_id = PoolId} = State) ->
    Init = octopus_pool_workers_cache:lookup({PoolId, init}),
    Ready = octopus_pool_workers_cache:lookup({PoolId, ready}),
    Busy = octopus_pool_workers_cache:lookup({PoolId, busy}),
    PoolInfo = [
        {init, length(Init)},
        {ready, length(Ready)},
        {busy, length(Busy)}
    ],
    {reply, PoolInfo, State};
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

handle_cast({worker_lockin, From}, #state{pool_id = PoolId} = State) ->
    Ready = octopus_pool_workers_cache:lookup({PoolId, ready}),
    Busy = octopus_pool_workers_cache:lookup({PoolId, busy}),
    case lists:keytake(From, 3, Busy) of
        {value, {WorkerId, Pid, From}, Busy2} ->
            Ready2 = lists:keystore(WorkerId, 1, Ready, {WorkerId, Pid}),
            ok = octopus_pool_workers_cache:insert({PoolId, ready}, Ready2),
            ok = octopus_pool_workers_cache:insert({PoolId, busy}, Busy2);
        false ->
            ok
    end,
    {noreply, State};
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

handle_info({start, WorkerId, Pid}, #state{pool_id = PoolId,
        init_type = InitType} = State) ->
    _ = erlang:monitor(process, Pid),
    Init = octopus_pool_workers_cache:lookup({PoolId, init}),
    Ready = octopus_pool_workers_cache:lookup({PoolId, ready}),
    case InitType of
        sync ->
            Ready2 = lists:keystore(WorkerId, 1, Ready, {WorkerId, Pid}),
            _ = octopus_pool_workers_cache:insert({PoolId, ready}, Ready2);
        async ->
            Ready2 = lists:keydelete(WorkerId, 1, Ready),
            Init2 = lists:keystore(WorkerId, 1, Init, {WorkerId, Pid}),
            ok = octopus_pool_workers_cache:insert({PoolId, init}, Init2),
            ok = octopus_pool_workers_cache:insert({PoolId, ready}, Ready2)
    end,
    {noreply, State};
handle_info({init, WorkerId}, #state{pool_id = PoolId} = State) ->
    Init = octopus_pool_workers_cache:lookup({PoolId, init}),
    Ready = octopus_pool_workers_cache:lookup({PoolId, ready}),
    case lists:keytake(WorkerId, 1, Ready) of
        {value, Tuple, Ready2} ->
            Init2 = lists:keystore(WorkerId, 1, Init, Tuple),
            ok = octopus_pool_workers_cache:insert({PoolId, init}, Init2),
            ok = octopus_pool_workers_cache:insert({PoolId, ready}, Ready2);
        false ->
            ok
    end,
    {noreply, State};
handle_info({ready, WorkerId}, #state{pool_id = PoolId} = State) ->
    Init = octopus_pool_workers_cache:lookup({PoolId, init}),
    Ready = octopus_pool_workers_cache:lookup({PoolId, ready}),
    case lists:keytake(WorkerId, 1, Init) of
        {value, Tuple, Init2} ->
            Ready2 = lists:keystore(WorkerId, 1, Ready, Tuple),
            ok = octopus_pool_workers_cache:insert({PoolId, init}, Init2),
            ok = octopus_pool_workers_cache:insert({PoolId, ready}, Ready2);
        false ->
            ok
    end,
    {noreply, State};
handle_info({'DOWN', _MonitorRef, process, Pid, _Info},
        #state{pool_id = PoolId} = State) ->
    Ready = octopus_pool_workers_cache:lookup({PoolId, ready}),
    Busy = octopus_pool_workers_cache:lookup({PoolId, busy}),
    case lists:keytake(Pid, 3, Busy) of
        {value, Tuple, Busy2} ->
            {WorkerId, WorkerPid, Pid} = Tuple,
            Ready2 = lists:keystore(WorkerId, 1, Ready, {WorkerId, WorkerPid}),
            ok = octopus_pool_workers_cache:insert({PoolId, ready}, Ready2),
            ok = octopus_pool_workers_cache:insert({PoolId, busy}, Busy2);
        false ->
            Init = octopus_pool_workers_cache:lookup({PoolId, init}),
            Init2 = lists:keydelete(Pid, 2, Init),
            Ready2 = lists:keydelete(Pid, 2, Ready),
            Busy2 = lists:keydelete(Pid, 2, Busy),
            ok = octopus_pool_workers_cache:insert({PoolId, init}, Init2),
            ok = octopus_pool_workers_cache:insert({PoolId, ready}, Ready2),
            ok = octopus_pool_workers_cache:insert({PoolId, busy}, Busy2)
    end,
    {noreply, State};
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
    {PoolId, PoolOpts, WorkerArgs} = octopus:get_pool_config(PoolId),
    InitType = proplists:get_value(init_type, PoolOpts, ?DEF_INIT_TYPE),
    WorkerModule = proplists:get_value(worker, PoolOpts),
    case InitType of
        sync ->
            {WorkerModule, WorkerArgs};
        async ->
            [WorkerOpts] = WorkerArgs,
            WorkerOpts2 = [
                {pool_id, PoolId},
                {worker_id, WorkerId},
                {init_type, InitType},
                {init_callback, {?MODULE, worker_init, [PoolId, WorkerId]}},
                {ready_callback, {?MODULE, worker_ready, [PoolId, WorkerId]}}
            |WorkerOpts],
            {WorkerModule, [WorkerOpts2]}
    end.
