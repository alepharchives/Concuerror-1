%%%----------------------------------------------------------------------
%%% Copyright (c) 2011, Alkis Gotovos <el3ctrologos@hotmail.com>,
%%%                     Maria Christakis <mchrista@softlab.ntua.gr>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>.
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%%----------------------------------------------------------------------
%%% Authors     : Alkis Gotovos <el3ctrologos@hotmail.com>
%%%               Maria Christakis <mchrista@softlab.ntua.gr>
%%% Description : Scheduler
%%%----------------------------------------------------------------------

-module(sched).

%% UI related exports
-export([analyze/3, replay/1]).

%% Internal exports
-export([block/0, notify/2, wait/0, wakeup/0, no_wakeup/0, lid_from_pid/1]).

-export_type([analysis_target/0, analysis_ret/0, bound/0]).

-include("gen.hrl").

%%%----------------------------------------------------------------------
%%% Definitions
%%%----------------------------------------------------------------------

-define(INFINITY, 1000000).
-define(NO_ERROR, undef).

%%%----------------------------------------------------------------------
%%% Records
%%%----------------------------------------------------------------------

%% Scheduler state
%%
%% active  : A set containing all processes ready to be scheduled.
%% blocked : A set containing all processes that cannot be scheduled next
%%          (e.g. waiting for a message on a `receive`).
%% current : The LID of the currently running or last run process.
%% details : A boolean being false when running a normal run and
%%           true when running a replay and need to send detailed
%%           info to the replay_logger.
%% error   : A term describing the error that occurred.
%% state   : The current state of the program.
-record(context, {active         :: ?SET_TYPE(lid:lid()),
                  blocked        :: ?SET_TYPE(lid:lid()),
                  current        :: lid:lid(),
                  details        :: boolean(),
                  error          :: ?NO_ERROR | error:error(),
                  state          :: state:state()}).

%% Internal message format
%%
%% msg    : An atom describing the type of the message.
%% pid    : The sender's LID.
%% misc   : Optional arguments, depending on the message type.
-record(sched, {msg  :: atom(),
                lid  :: lid:lid(),
                misc  = empty :: term()}).

%% Special internal message format (fields same as above).
-record(special, {msg :: atom(),
                  lid :: lid:lid() | 'not_found',
                  misc = empty :: term()}).

%%%----------------------------------------------------------------------
%%% Types
%%%----------------------------------------------------------------------

-type analysis_info() :: {analysis_target(), non_neg_integer()}.

-type analysis_options() :: ['details' |
                             {'init_state', state:state()} |
                             {'preb',  bound()}].

%% Analysis result tuple.
-type analysis_ret() :: {'ok', analysis_info()} |
                        {'error', 'instr', analysis_info()} |
                        {'error', 'analysis', analysis_info(),
                         [ticket:ticket()]}.

%% Module-Function-Arguments tuple.
-type analysis_target() :: {module(), atom(), [term()]}.

-type bound() :: 'inf' | non_neg_integer().

%% Scheduler notification.

-type notification() :: 'after' | 'block' | 'demonitor' | 'ets_delete' |
                        'ets_foldl' | 'ets_insert' | 'ets_insert_new' |
                        'ets_lookup' | 'ets_match_delete' | 'ets_match_object' |
                        'ets_select_delete' | 'fun_exit' | 'halt' |
                        'is_process_alive' | 'link' | 'monitor' |
                        'process_flag' | 'receive' | 'receive_no_instr' |
                        'register' | 'send' | 'spawn' | 'spawn_link' |
                        'spawn_monitor' | 'spawn_opt' | 'unlink' |
                        'unregister' | 'whereis'.

%%%----------------------------------------------------------------------
%%% User interface
%%%----------------------------------------------------------------------

%% @spec: analyze(analysis_target(), [file()], options()) -> analysis_ret()
%% @doc: Produce all interleavings of running `Target'.
-spec analyze(analysis_target(), [file()], analysis_options()) -> analysis_ret().

analyze(Target, Files, Options) ->
    Ret =
        case instr:instrument_and_compile(Files) of
            {ok, Bin} ->
                %% Note: No error checking for load
                ok = instr:load(Bin),
                log:log("Running analysis...~n~n"),
                {T1, _} = statistics(wall_clock),
                ISOption = {init_state, {state:empty(), 0}},
                Result = interleave(Target, [ISOption|Options]),
                {T2, _} = statistics(wall_clock),
                {Mins, Secs} = elapsed_time(T1, T2),
                case Result of
                    {ok, RunCount} ->
                        log:log("Analysis complete (checked ~w interleaving(s) "
                                "in ~wm~.2fs):~n", [RunCount, Mins, Secs]),
                        log:log("No errors found.~n"),
                        {ok, {Target, RunCount}};
                    {error, RunCount, Tickets} ->
                        TicketCount = length(Tickets),
                        log:log("Analysis complete (checked ~w interleaving(s) "
                                "in ~wm~.2fs):~n", [RunCount, Mins, Secs]),
                        log:log("Found ~p erroneous interleaving(s).~n",
                                [TicketCount]),
                        {error, analysis, {Target, RunCount}, Tickets}
                end;
            error -> {error, instr, {Target, 0}}
        end,
    Ret.

%% @spec: replay(analysis_target(), state()) -> [proc_action()]
%% @doc: Replay the given state and return detailed information about the
%% process interleaving.
-spec replay(ticket:ticket()) -> [proc_action:proc_action()].

replay(Ticket) ->
    _ = replay_logger:start(),
    replay_logger:start_replay(),
    Target = ticket:get_target(Ticket),
    State = ticket:get_state(Ticket),
    Options = [details, {init_state, State}],
    interleave(Target, Options),
    Result = replay_logger:get_replay(),
    replay_logger:stop(),
    Result.

%% Produce all possible process interleavings of (Mod, Fun, Args).
%% Options:
%%   {init_state, InitState}: State to replay (default: state_init()).
%%   details: Produce detailed interleaving information (see `replay_logger`).
interleave(Target, Options) ->
    Self = self(),
    spawn_link(fun() -> interleave_aux(Target, Options, Self) end),
    receive
        {interleave_result, Result} -> Result
    end.

interleave_aux(Target, Options, Parent) ->
    register(?RP_SCHED, self()),
    %% The mailbox is flushed mainly to discard possible `exit` messages
    %% before enabling the `trap_exit` flag.
    util:flush_mailbox(),
    process_flag(trap_exit, true),
    %% Initialize state table.
    state_start(),
    %% Save empty replay state for the first run.
    {init_state, InitState} = lists:keyfind(init_state, 1, Options),
    state_save([InitState]),
    PreBound =
        case lists:keyfind(preb, 1, Options) of
            {preb, inf} -> ?INFINITY;
            {preb, Bound} -> Bound;
            false -> ?INFINITY
        end,
    {RunCnt, Tickets, _Stop} = interleave_loop(Target, 1, [], PreBound, Options),
    Result = interleave_outer_loop_ret(Tickets, RunCnt),
    state_stop(),
    unregister(?RP_SCHED),
    Parent ! {interleave_result, Result}.

interleave_outer_loop_ret([], RunCnt) ->
    {ok, RunCnt};
interleave_outer_loop_ret(Tickets, RunCnt) ->
    {error, RunCnt, ticket:sort(Tickets)}.

%% Main loop for producing process interleavings.
%% The first process (FirstPid) is created linked to the scheduler,
%% so that the latter can receive the former's exit message when it
%% terminates. In the same way, every process that may be spawned in
%% the course of the program shall be linked to the scheduler process.
interleave_loop(Target, RunCnt, Tickets, PreBound, Options) ->
    Det = lists:member(details, Options),
    %% Lookup state to replay.
    case state_load() of
        no_state -> {RunCnt - 1, Tickets, false};
        ReplayState ->
            ?debug_1("Running interleaving ~p~n", [RunCnt]),
            ?debug_1("----------------------~n"),
            lid:start(),
            %% Save current process list (any process created after
            %% this will be cleaned up at the end of the run)
            ProcBefore = processes(),
            %% Spawn initial user process
            {Mod, Fun, Args} = Target,
            NewFun = fun() -> wait(), apply(Mod, Fun, Args) end,
            FirstPid = spawn_link(NewFun),
            %% Initialize scheduler context
            FirstLid = lid:new(FirstPid, noparent),
            Active = ?SETS:add_element(FirstLid, ?SETS:new()),
            Blocked = ?SETS:new(),
            State = state:empty(),
            Context = #context{active = Active, blocked = Blocked,
                               state = State, details = Det},
            %% Interleave using driver
            Ret = driver(Context, ReplayState, PreBound),
            %% Cleanup
            proc_cleanup(processes() -- ProcBefore),
            lid:stop(),
            NewTickets =
                case Ret of
                    {error, Error, ErrorState} ->
                        Ticket = ticket:new(Target, Error, ErrorState),
                        case Det of
                            true -> continue;
                            false -> log:show_error(Ticket)
                        end,
                        [Ticket|Tickets];
                    _OtherRet1 -> Tickets
                end,
            NewRunCnt =
                case Ret of
                    abort ->
                        ?debug_1("-----------------------~n"),
                        ?debug_1("Run aborted.~n~n"),
                        RunCnt;
                    _OtherRet2 ->
                        ?debug_1("-----------------------~n"),
                        ?debug_1("Run terminated.~n~n"),
                        RunCnt + 1
                end,
            receive
                stop_analysis -> {NewRunCnt - 1, NewTickets, true}
            after 0 ->
                    interleave_loop(Target, NewRunCnt, NewTickets, PreBound, Options)
            end
    end.

%%%----------------------------------------------------------------------
%%% Core components
%%%----------------------------------------------------------------------

driver(Context, {ReplayState, Pre}, Bound) ->
    case state:is_empty(ReplayState) of
        true -> driver_normal(Context, 0, Bound);
        false -> driver_replay(Context, ReplayState, Pre, Bound)
    end.

driver_replay(Context, ReplayState, Pre, Bound) ->
    {Next, Rest} = state:trim_head(ReplayState),
    NewContext = run(Context#context{current = Next, error = ?NO_ERROR}),
    #context{blocked = NewBlocked} = NewContext,
    case state:is_empty(Rest) of
        true ->
            case ?SETS:is_element(Next, NewBlocked) of
                %% If the last action of the replayed state prefix is a block,
                %% we can safely abort.
                true -> abort;
                %% Replay has finished; proceed in normal mode, after checking
                %% for errors during the last replayed action.
                false -> check_for_errors(NewContext, Pre, Bound)
            end;
        false ->
            case ?SETS:is_element(Next, NewBlocked) of
                true -> log:internal("Proc. ~p should be active.", [Next]);
                false -> driver_replay(NewContext, Rest, Pre, Bound)
            end
    end.

driver_normal(#context{active = Active, current = LastLid,
                       state = State} = Context, Pre, Bound) ->
    Next =
        case ?SETS:is_element(LastLid, Active) of
            true ->
                TmpActive = ?SETS:to_list(?SETS:del_element(LastLid, Active)),
                {LastLid,TmpActive, next};
            false ->
                [Head|TmpActive] = ?SETS:to_list(Active),
                {Head, TmpActive, current}
        end,
    {NewContext, Insert} = run_no_block(Context, Next),
    insert_states(State, Insert, Pre, Bound),
    check_for_errors(NewContext, Pre, Bound).

%% Handle four possible cases:
%% - An error occured during the execution of the last process =>
%%   Terminate the run and report the erroneous interleaving sequence.
%% - Only blocked processes exist =>
%%   Terminate the run and report a deadlock.
%% - No active or blocked processes exist =>
%%   Terminate the run without errors.
%% - There exists at least one active process =>
%%   Continue run.
check_for_errors(#context{active = NewActive, blocked = NewBlocked,
                          error = NewError, state = NewState} = NewContext, Pre, Bound) ->
    case NewError of
        ?NO_ERROR ->
            case ?SETS:size(NewActive) of
                0 ->
                    case ?SETS:size(NewBlocked) of
                        0 -> ok;
                        _NonEmptyBlocked ->
                            Deadlock = error:new({deadlock, NewBlocked}),
                            {error, Deadlock, NewState}
                    end;
                _NonEmptyActive -> driver_normal(NewContext, Pre, Bound)
            end;
        _Other -> {error, NewError, NewState}
    end.

run_no_block(#context{state = State} = Context, {Next, Rest, W}) ->
    NewContext = run(Context#context{current = Next, error = ?NO_ERROR}),
    #context{blocked = NewBlocked} = NewContext,
    case ?SETS:is_element(Next, NewBlocked) of
        true ->
            case Rest of
                [] -> {NewContext#context{state = State}, {[], W}};
                [RH|RT] ->
                    NextContext = NewContext#context{state = State},
                    run_no_block(NextContext, {RH, RT, current})
            end;
        false -> {NewContext, {Rest, W}}
    end.

insert_states(State, {Lids, current}, Pre, _Bound) ->
    Extend = lists:map(fun(L) -> {state:extend(State, L), Pre} end, Lids),
    state_save(Extend);
insert_states(State, {Lids, next}, Pre, Bound) when Bound==inf; Pre<Bound->
    Extend = lists:map(fun(L) -> {state:extend(State, L), Pre+1} end, Lids),
    state_save(Extend);
insert_states(_State, _Lids, _Pre, _Bound) -> ok.

%% Run process Lid in context Context until it encounters a preemption point.
run(#context{current = Lid, state = State} = Context) ->
    ?debug_2("Running process ~s.~n", [lid:to_string(Lid)]),
    %% Create new state by adding this process.
    NewState = state:extend(State, Lid),
    %% Send message to "unblock" the process.
    continue(Lid),
    %% Dispatch incoming notifications to the appropriate handler.
    NewContext = dispatch(Context#context{state = NewState}),
    %% Update context due to wakeups caused by the last action.
    ?SETS:fold(fun check_wakeup/2, NewContext, NewContext#context.blocked).

check_wakeup(Lid, #context{active = Active, blocked = Blocked} = Context) ->
    continue(Lid),
    receive
        #special{msg = wakeup} ->
            NewBlocked = ?SETS:del_element(Lid, Blocked),
            NewActive = ?SETS:add_element(Lid, Active),
            Context#context{active = NewActive, blocked = NewBlocked};
        #special{msg = no_wakeup} -> Context
    end.

%% Delegate notifications sent by instrumented client code to the appropriate
%% handlers.
dispatch(Context) ->
    receive
        #sched{msg = Type, lid = Lid, misc = Misc} ->
            handler(Type, Lid, Context, Misc);
        %% Ignore unknown processes.
        {'EXIT', Pid, Reason} ->
            case lid:from_pid(Pid) of
                not_found -> dispatch(Context);
                Lid -> handler(exit, Lid, Context, Reason)
            end
    end.

%%%----------------------------------------------------------------------
%%% Handlers
%%%----------------------------------------------------------------------

handler('after', Lid, #context{details = Det} = Context, _Misc) ->
    log_details(Det, {'after', Lid}),
    Context;

%% Move the process to the blocked set.
handler(block, Lid,
        #context{active = Active, blocked = Blocked, details = Det} = Context,
        _Misc) ->
    NewActive = ?SETS:del_element(Lid, Active),
    NewBlocked = ?SETS:add_element(Lid, Blocked),
    log_details(Det, {block, Lid}),
    Context#context{active = NewActive, blocked = NewBlocked};

handler(demonitor, Lid, #context{details = Det} = Context, _Ref) ->
    %% TODO: Get LID from Ref?
    TargetLid = lid:mock(0),
    log_details(Det, {demonitor, Lid, TargetLid}),
    Context;

%% Remove the exited process from the active set.
%% NOTE: This is called after a process has exited, not when it calls
%%       exit/1 or exit/2.
handler(exit, Lid, #context{active = Active, details = Det} = Context,
        Reason) ->
    NewActive = ?SETS:del_element(Lid, Active),
    %% Cleanup LID stored info.
    lid:cleanup(Lid),
    %% Handle and propagate errors.
    case Reason of
        normal ->
            log_details(Det, {exit, Lid, normal}),
            Context#context{active = NewActive};
        _Else ->
            Error = error:new(Reason),
            log_details(Det, {exit, Lid, error:type(Error)}),
            Context#context{active = NewActive, error = Error}
    end;

%% Return empty active and blocked queues to force run termination.
handler(halt, Lid, #context{details = Det} = Context, Misc) ->
    Halt =
        case Misc of
            empty -> {halt, Lid};
            Status -> {halt, Lid, Status}
        end,
    log_details(Det, Halt),
    Context#context{active = ?SETS:new(), blocked = ?SETS:new()};

handler(is_process_alive, Lid, #context{details = Det} = Context, TargetPid) ->
    TargetLid = lid:from_pid(TargetPid),
    log_details(Det, {is_process_alive, Lid, TargetLid}),
    Context;

handler(link, Lid, #context{details = Det} = Context, TargetPid) ->
    TargetLid = lid:from_pid(TargetPid),
    log_details(Det, {link, Lid, TargetLid}),
    Context;

handler(monitor, Lid, #context{details = Det} = Context, {Item, _Ref}) ->
    TargetLid = lid:from_pid(Item),
    log_details(Det, {monitor, Lid, TargetLid}),
    Context;

handler(process_flag, Lid, #context{details = Det} = Context, {Flag, Value}) ->
    log_details(Det, {process_flag, Lid, Flag, Value}),
    Context;

%% Normal receive message handler.
handler('receive', Lid, #context{details = Det} = Context, {From, Msg}) ->
    log_details(Det, {'receive', Lid, From, Msg}),
    Context;

%% Receive message handler for special messages, like 'EXIT' and 'DOWN',
%% which don't have an associated sender process.
handler('receive_no_instr', Lid, #context{details = Det} = Context, Msg) ->
    log_details(Det, {'receive_no_instr', Lid, Msg}),
    Context;

handler(register, Lid, #context{details = Det} = Context, {RegName, RegLid}) ->
    log_details(Det, {register, Lid, RegName, RegLid}),
    Context;

handler(send, Lid, #context{details = Det} = Context, {DstPid, Msg}) ->
    DstLid = lid:from_pid(DstPid),
    log_details(Det, {send, Lid, DstLid, Msg}),
    Context;

%% Link the newly spawned process to the scheduler process and add it to the
%% active set.
handler(spawn, ParentLid,
        #context{active = Active, details = Det} = Context, ChildPid) ->
    link(ChildPid),
    ChildLid = lid:new(ChildPid, ParentLid),
    log_details(Det, {spawn, ParentLid, ChildLid}),
    NewActive = ?SETS:add_element(ChildLid, Active),
    Context#context{active = NewActive};

%% FIXME: Refactor this (it's exactly the same as 'spawn')
handler(spawn_link, ParentLid,
        #context{active = Active, details = Det} = Context, ChildPid) ->
    link(ChildPid),
    ChildLid = lid:new(ChildPid, ParentLid),
    log_details(Det, {spawn_link, ParentLid, ChildLid}),
    NewActive = ?SETS:add_element(ChildLid, Active),
    Context#context{active = NewActive};

%% FIXME: Refactor this (it's almost the same as 'spawn')
handler(spawn_monitor, ParentLid,
        #context{active = Active, details = Det} = Context, {ChildPid, _Ref}) ->
    link(ChildPid),
    ChildLid = lid:new(ChildPid, ParentLid),
    log_details(Det, {spawn_monitor, ParentLid, ChildLid}),
    NewActive = ?SETS:add_element(ChildLid, Active),
    Context#context{active = NewActive};

%% Similar to above depending on options.
handler(spawn_opt, ParentLid,
        #context{active = Active, details = Det} = Context, {Ret, Opt}) ->
    {ChildPid, _Ref} =
        case Ret of
            {_C, _R} = CR -> CR;
            C -> {C, noref}
        end,
    link(ChildPid),
    ChildLid = lid:new(ChildPid, ParentLid),
    Opts = sets:to_list(sets:intersection(sets:from_list([link, monitor]),
                                          sets:from_list(Opt))),
    log_details(Det, {spawn_opt, ParentLid, ChildLid, Opts}),
    NewActive = ?SETS:add_element(ChildLid, Active),
    Context#context{active = NewActive};

handler(unlink, Lid, #context{details = Det} = Context, TargetPid) ->
    TargetLid = lid:from_pid(TargetPid),
    log_details(Det, {unlink, Lid, TargetLid}),
    Context;

handler(unregister, Lid, #context{details = Det} = Context, RegName) ->
    log_details(Det, {unregister, Lid, RegName}),
    Context;

handler(whereis, Lid, #context{details = Det} = Context, {RegName, Result}) ->
    ResultLid = lid:from_pid(Result),
    log_details(Det, {whereis, Lid, RegName, ResultLid}),
    Context;

%% Handler for anything "non-special". It just passes the arguments
%% for logging.
%% TODO: We may be able to delete some of the above that can be handled
%%       by this generic handler.
handler(CallMsg, Lid, #context{details = Det} = Context, Args) ->
    log_details(Det, {CallMsg, Lid, Args}),
    Context.

%%%----------------------------------------------------------------------
%%% Helper functions
%%%----------------------------------------------------------------------

%% Kill any remaining processes.
%% If the run was terminated by an exception, processes linked to
%% the one where the exception occurred could have been killed by the
%% exit signal of the latter without having been deleted from the pid/lid
%% tables. Thus, 'EXIT' messages with any reason are accepted.
proc_cleanup(ProcList) ->
    Link_and_kill = fun(P) -> link(P), exit(P, kill) end,
    lists:foreach(Link_and_kill, ProcList),
    wait_for_exit(ProcList).

wait_for_exit([]) -> ok;
wait_for_exit([P|Rest]) ->
    receive {'EXIT', P, _Reason} -> wait_for_exit(Rest) end.

%% Calculate and print elapsed time between T1 and T2.
elapsed_time(T1, T2) ->
    ElapsedTime = T2 - T1,
    Mins = ElapsedTime div 60000,
    Secs = (ElapsedTime rem 60000) / 1000,
    ?debug_1("Done in ~wm~.2fs\n", [Mins, Secs]),
    {Mins, Secs}.

%% Print debug messages and send them to replay_logger if Det is true.
log_details(Det, Action) ->
    ?debug_1(proc_action:to_string(Action) ++ "~n"),
    case Det of
        true -> replay_logger:log(Action);
        false -> continue
    end.

%% Remove and return a state.
%% If no states available, return 'no_state'.
state_load() ->
%    {Len1, Len2} = get(?NT_STATELEN),
    case get(?NT_STATE1) of
        [{State,Preb} | Rest] ->
            put(?NT_STATE1, Rest),
%            log:progress(log, Len1-1),
%            put(?NT_STATELEN, {Len1-1, Len2}),
            {state:pack(State), Preb};
        [] -> no_state
    end.

%% Add some states to the current `state` table.
state_save(State) ->
%    Size = length(State),
%    {Len1, Len2} = get(?NT_STATELEN),
%    put(?NT_STATELEN, {Len1+Size, Len2}),
    put(?NT_STATE1, State ++ get(?NT_STATE1)).

%% Initialize state tables.
state_start() ->
    put(?NT_STATE1, []),
%    put(?NT_STATE2, []),
%    put(?NT_STATELEN, {0, 0}),
    ok.

%% Clean up state table.
state_stop() ->
    ok.


%%%----------------------------------------------------------------------
%%% Instrumentation interface
%%%----------------------------------------------------------------------

%% Notify the scheduler of a blocked process.
-spec block() -> 'ok'.

block() ->
    notify(block, []).

%% Prompt process Pid to continue running.
continue(Pid) when is_pid(Pid) ->
    Pid ! #sched{msg = continue},
    ok;
continue(Lid) ->
    Pid = lid:get_pid(Lid),
    Pid ! #sched{msg = continue},
    ok.

%% Notify the scheduler of an event.
%% If the calling user process has an associated LID, then send
%% a notification and yield. Otherwise, for an unknown process
%% running instrumented code completely ignore this call.
-spec notify(notification(), any()) -> 'ok'.

notify(Msg, Misc) ->
    case lid_from_pid(self()) of
        not_found -> ok;
        Lid ->
            ?RP_SCHED_SEND ! #sched{msg = Msg, lid = Lid, misc = Misc},
            wait()
    end.

%% TODO: Maybe move into lid module.
-spec lid_from_pid(pid()) -> lid:lid() | 'not_found'.

lid_from_pid(Pid) ->
    lid:from_pid(Pid).

-spec wakeup() -> 'ok'.

wakeup() ->
    %% TODO: Depending on how 'receive' is instrumented, a check for
    %% whether the caller is a known process might be needed here.
    ?RP_SCHED_SEND ! #special{msg = wakeup},
    wait().

-spec no_wakeup() -> 'ok'.

no_wakeup() ->
    %% TODO: Depending on how 'receive' is instrumented, a check for
    %% whether the caller is a known process might be needed here.
    ?RP_SCHED_SEND ! #special{msg = no_wakeup},
    wait().

%% Wait until the scheduler prompts to continue.
-spec wait() -> 'ok'.

wait() ->
    receive
        #sched{msg = continue} -> ok
    end.
