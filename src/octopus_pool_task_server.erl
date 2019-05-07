-module(octopus_pool_task_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([worker_start/2]).
-export([worker_init/2]).
-export([worker_ready/2]).
-export([worker_lockout/2]).
-export([worker_lockin/1]).
-export([pool_info/1]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-record(state, {
    pool_id,
    init_type = sync,
    queue
}).

-define(DEF_INIT_TYPE, sync).

%% API
-spec start_link(PoolId) -> {ok, Pid}
when
    PoolId  :: atom(),
    Pid     :: pid().

start_link(PoolId) ->
    ServerName = octopus_name_resolver:get(PoolId, ?MODULE),
    gen_server:start_link({local, ServerName}, ?MODULE, [PoolId], []).


-spec worker_start(PoolId, WorkerId) ->
        {pk, Pid} |
        {ok, Pid, Extra} |
        {error, Reason} |
        term()
when
    PoolId      :: atom(),
    WorkerId    :: term(),
    Extra       :: term(),
    Reason      :: term().


worker_start(PoolId, WorkerId) ->
    ServerName = octopus_name_resolver:get(PoolId, ?MODULE),
    {WorkerModule, WorkerArgs} = get_worker_config(PoolId, WorkerId),
    case apply(WorkerModule, start_link, WorkerArgs) of
        {ok, Pid} when is_pid(Pid) ->
            _ = ServerName ! {start, WorkerId, Pid},
            {ok, Pid};
        {ok, Pid, Extra} when is_pid(Pid) ->
            _ = ServerName ! {start, WorkerId, Pid},
            {ok, Pid, Extra};
        Other ->
            Other
    end.

-spec worker_init(PoolId, WorkerId) -> ok
when
    PoolId      :: atom(),
    WorkerId    :: term().

worker_init(PoolId, WorkerId) ->
    ServerName = octopus_name_resolver:get(PoolId, ?MODULE),
    _ = ServerName ! {init, WorkerId},
    ok.

-spec worker_ready(PoolId, WorkerId) -> ok
when
    PoolId      :: atom(),
    WorkerId    :: term().

worker_ready(PoolId, WorkerId) ->
    ServerName = octopus_name_resolver:get(PoolId, ?MODULE),
    _ = ServerName ! {ready, WorkerId},
    ok.

-spec worker_lockout(PoolId, Timeout) -> {ok, Pid} | {error, Reason}
when
    PoolId  :: atom(),
    Timeout :: timeout(),
    Pid     :: pid(),
    Reason  :: term().

worker_lockout(PoolId, Timeout) ->
    ServerName = octopus_name_resolver:get(PoolId, ?MODULE),
    try
        gen_server:call(ServerName, worker_lockout, Timeout)
    catch
        _:_ ->
            gen_server:cast(ServerName, {remove_from_queue, self()}),
            {error, timeout}
    end.


-spec worker_lockin(PoolId) -> ok
when
    PoolId  :: atom().

worker_lockin(PoolId) ->
    ServerName = octopus_name_resolver:get(PoolId, ?MODULE),
    gen_server:cast(ServerName, {worker_lockin, self()}).

-spec pool_info(PoolId) -> #{Item => non_neg_integer()}
when
    PoolId  :: atom(),
    Item    :: init | ready | busy.

pool_info(PoolId) ->
    ServerName = octopus_name_resolver:get(PoolId, ?MODULE),
    gen_server:call(ServerName, pool_info).

%% gen_server callbacks
init([PoolId]) ->
    _ = erlang:process_flag(trap_exit, true),
    {PoolId, PoolOpts, _WorkerArgs} = octopus:get_pool_config(PoolId),
    InitType = proplists:get_value(init_type, PoolOpts, ?DEF_INIT_TYPE),
    State = #state{
        pool_id = PoolId,
        init_type = InitType,
        queue = queue:new()
    },
    {ok, State}.

handle_call(worker_lockout, From, #state{pool_id = PoolId, queue = Queue} = State) ->
    Ready = lookup({PoolId, ready}),
    case Ready of
        [{WorkerId, Pid}|Ready2] ->
            ok = lockout(PoolId, WorkerId, Pid, From),
            ok = insert({PoolId, ready}, Ready2),
            {noreply, State};
        [] ->
            {noreply, State#state{queue = queue:in(From, Queue)}}
    end;
handle_call(pool_info, _From, #state{pool_id = PoolId} = State) ->
    Init = lookup({PoolId, init}),
    Ready = lookup({PoolId, ready}),
    Busy = lookup({PoolId, busy}),
    PoolInfo = #{
        init    => length(Init),
        ready   => length(Ready),
        busy    => length(Busy)
    },
    {reply, PoolInfo, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({worker_lockin, FromPid}, #state{pool_id = PoolId, queue = Queue} = State) ->
    Busy = lookup({PoolId, busy}),
    Queue2 = case lists:keytake(FromPid, 3, Busy) of
        {value, {WorkerId, Pid, FromPid, Monitor}, Busy2} ->
            true = erlang:demonitor(Monitor),
            case queue:out(Queue) of
                {{value, From}, NewQueue} ->
                    ok = lockout(PoolId, WorkerId, Pid, From),
                    NewQueue;
                {empty, _Queue2} ->
                    Ready = lookup({PoolId, ready}),
                    Ready2 = lists:keystore(WorkerId, 1, Ready, {WorkerId, Pid}),
                    ok = insert({PoolId, ready}, Ready2),
                    ok = insert({PoolId, busy}, Busy2),
                    Queue
            end;
        false ->
            Queue
    end,
    {noreply,  State#state{queue = Queue2}};
handle_cast({remove_from_queue, FromPid}, #state{queue = Queue} = State) ->
    Queue2 = queue:filter(fun({Pid, _Tag}) -> Pid =/= FromPid end, Queue),
    _ = case Queue =:= Queue2 of
        true ->
            gen_server:cast(self(), {worker_lockin, FromPid});
        false ->
            ignore
    end,
    {noreply, State#state{queue = Queue2}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({start, WorkerId, Pid}, #state{pool_id = PoolId,
        init_type = InitType} = State) ->
    _ = erlang:monitor(process, Pid),
    Init = lookup({PoolId, init}),
    Ready = lookup({PoolId, ready}),
    case InitType of
        sync ->
            Ready2 = lists:keystore(WorkerId, 1, Ready, {WorkerId, Pid}),
            _ = insert({PoolId, ready}, Ready2);
        async ->
            Ready2 = lists:keydelete(WorkerId, 1, Ready),
            Init2 = lists:keystore(WorkerId, 1, Init, {WorkerId, Pid}),
            ok = insert({PoolId, init}, Init2),
            ok = insert({PoolId, ready}, Ready2)
    end,
    {noreply, State};
handle_info({init, WorkerId}, #state{pool_id = PoolId} = State) ->
    Init = lookup({PoolId, init}),
    Ready = lookup({PoolId, ready}),
    case lists:keytake(WorkerId, 1, Ready) of
        {value, Tuple, Ready2} ->
            Init2 = lists:keystore(WorkerId, 1, Init, Tuple),
            ok = insert({PoolId, init}, Init2),
            ok = insert({PoolId, ready}, Ready2);
        false ->
            ok
    end,
    {noreply, State};
handle_info({ready, WorkerId}, #state{pool_id = PoolId} = State) ->
    Init = lookup({PoolId, init}),
    Ready = lookup({PoolId, ready}),
    case lists:keytake(WorkerId, 1, Init) of
        {value, Tuple, Init2} ->
            Ready2 = lists:keystore(WorkerId, 1, Ready, Tuple),
            ok = insert({PoolId, init}, Init2),
            ok = insert({PoolId, ready}, Ready2);
        false ->
            ok
    end,
    {noreply, State};
handle_info({'DOWN', MonitorRef, process, Pid, _Info},
        #state{pool_id = PoolId} = State) ->
    Ready = lookup({PoolId, ready}),
    Busy = lookup({PoolId, busy}),
    case lists:keytake(Pid, 3, Busy) of
        {value, Tuple, Busy2} ->
            {WorkerId, WorkerPid, Pid, MonitorRef} = Tuple,
            Ready2 = lists:keystore(WorkerId, 1, Ready, {WorkerId, WorkerPid}),
            ok = insert({PoolId, ready}, Ready2),
            ok = insert({PoolId, busy}, Busy2);
        false ->
            Init = lookup({PoolId, init}),
            Init2 = lists:keydelete(Pid, 2, Init),
            Ready2 = lists:keydelete(Pid, 2, Ready),
            Busy2 = lists:keydelete(Pid, 2, Busy),
            ok = insert({PoolId, init}, Init2),
            ok = insert({PoolId, ready}, Ready2),
            ok = insert({PoolId, busy}, Busy2)
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
lockout(PoolId, WorkerId, Pid, {FromPid, _} = From) ->
    Busy = lookup({PoolId, busy}),
    Monitor = erlang:monitor(process, FromPid),
    Busy2 = lists:keystore(WorkerId, 1, Busy, {WorkerId, Pid, FromPid, Monitor}),
    ok = insert({PoolId, busy}, Busy2),
    _ = gen_server:reply(From, {ok, Pid}),
    ok.

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

insert(Key, Value) ->
    _ = put(Key, Value),
    ok.

lookup(Key) ->
    case get(Key) of
        undefined   -> [];
        Value       -> Value
    end.
