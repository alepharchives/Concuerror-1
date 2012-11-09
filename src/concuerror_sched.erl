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

-module(concuerror_sched).

%% UI related exports
-export([analyze/3]).

%% Internal exports
-export([block/0, notify/2, wait/0, wakeup/0, no_wakeup/0, lid_from_pid/1]).

-export([notify/3, wait_poll_or_continue/0]).

-export_type([analysis_target/0, analysis_ret/0, bound/0]).

-include("gen.hrl").

%%%----------------------------------------------------------------------
%%% Definitions
%%%----------------------------------------------------------------------

-define(INFINITY, 1000000).
-define(NO_ERROR, undef).

%%-define(F_DEBUG, true).
-ifdef(F_DEBUG).
-define(f_debug(A,B), concuerror_log:log(A,B)).
-define(f_debug(A), concuerror_log:log(A,[])).
-define(DEPTH, 12).
-else.
-define(f_debug(_A,_B), ok).
-define(f_debug(A), ok).
-endif.

%%%----------------------------------------------------------------------
%%% Records
%%%----------------------------------------------------------------------

%% Scheduler state
%%
%% active  : A set containing all processes ready to be scheduled.
%% blocked : A set containing all processes that cannot be scheduled next
%%          (e.g. waiting for a message on a `receive`).
%% current : The LID of the currently running or last run process.
%% actions : The actions performed (concuerror_proc_action:proc_action()).
%% error   : A term describing the error that occurred.
%% state   : The current state of the program.
-record(context, {active         :: ?SET_TYPE(concuerror_lid:lid()),
                  blocked        :: ?SET_TYPE(concuerror_lid:lid()),
                  current        :: concuerror_lid:lid(),
                  actions        :: [concuerror_proc_action:proc_action()],
                  error          :: ?NO_ERROR | concuerror_error:error(),
                  state          :: concuerror_state:state()}).


%% 'next' messages are about next instrumented instruction not yet dispatched
%% 'prev' messages are about additional effects of a dispatched instruction
%% 'async' messages are about receives which have become enabled
-type sched_msg_type() :: 'next' | 'prev' | 'async'.

%% Internal message format
%%
%% msg    : An atom describing the type of the message.
%% pid    : The sender's LID.
%% misc   : Optional arguments, depending on the message type.
-record(sched, {msg          :: atom(),
                lid          :: concuerror_lid:lid(),
                misc = empty :: term(),
                type = next  :: sched_msg_type()}).

%% Special internal message format (fields same as above).
-record(special, {msg :: atom(),
                  lid :: concuerror_lid:lid() | 'not_found',
                  misc = empty :: term()}).

%%%----------------------------------------------------------------------
%%% Types
%%%----------------------------------------------------------------------

-type analysis_info() :: {analysis_target(), non_neg_integer()}.

-type analysis_options() :: [{'preb', bound()} |
                             {'include', [file:name()]} |
                             {'define', concuerror_instr:macros()}].


%% Analysis result tuple.
-type analysis_ret() ::
    {'ok', analysis_info()} |
    {'error', 'instr', analysis_info()} |
    {'error', 'analysis', analysis_info(), [concuerror_ticket:ticket()]}.

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

%% @spec: analyze(analysis_target(), [file:filename()], analysis_options()) ->
%%          analysis_ret()
%% @doc: Produce all interleavings of running `Target'.
-spec analyze(analysis_target(), [file:filename()], analysis_options()) ->
            analysis_ret().

analyze(Target, Files, Options) ->
    PreBound =
        case lists:keyfind(preb, 1, Options) of
            {preb, inf} -> ?INFINITY;
            {preb, Bound} -> Bound;
            false -> ?DEFAULT_PREB
        end,
    Include =
        case lists:keyfind('include', 1, Options) of
            {'include', I} -> I;
            false -> ?DEFAULT_INCLUDE
        end,
    Define =
        case lists:keyfind('define', 1, Options) of
            {'define', D} -> D;
            false -> ?DEFAULT_DEFINE
        end,
    {Dpor, DporFlavor} =
        case lists:keyfind(dpor, 1, Options) of
            {dpor, Flavor} -> {true, Flavor};
            false -> {false, none}
        end,
    Ret =
        case concuerror_instr:instrument_and_compile(Files, Include, Define, Dpor) of
            {ok, Bin} ->
                %% Note: No error checking for load
                ok = concuerror_instr:load(Bin),
                concuerror_log:log("Running analysis...~n~n"),
                {T1, _} = statistics(wall_clock),
                Result = interleave(Target, PreBound, Dpor, DporFlavor),
                {T2, _} = statistics(wall_clock),
                {Mins, Secs} = elapsed_time(T1, T2),
                case Result of
                    {ok, RunCount} ->
                        concuerror_log:log("Analysis complete (checked ~w "
                                "interleaving(s) in ~wm~.2fs):~n",
                                [RunCount, Mins, Secs]),
                        concuerror_log:log("No errors found.~n"),
                        {ok, {Target, RunCount}};
                    {error, RunCount, Tickets} ->
                        TicketCount = length(Tickets),
                        concuerror_log:log("Analysis complete (checked ~w "
                                "interleaving(s) in ~wm~.2fs):~n",
                                [RunCount, Mins, Secs]),
                        concuerror_log:log(
                                "Found ~p erroneous interleaving(s).~n",
                                [TicketCount]),
                        {error, analysis, {Target, RunCount}, Tickets}
                end;
            error -> {error, instr, {Target, 0}}
        end,
    concuerror_instr:delete_and_purge(Files),
    Ret.

%% Produce all possible process interleavings of (Mod, Fun, Args).
interleave(Target, PreBound, Dpor, DporFlavor) ->
    Self = self(),
    Fun =
        fun() ->
                case Dpor of
                    true -> interleave_dpor(Target, PreBound, Self, DporFlavor);
                    false -> interleave_aux(Target, PreBound, Self)
                end
        end,
    spawn_link(Fun),
    receive
        {interleave_result, Result} -> Result
    end.

interleave_aux(Target, PreBound, Parent) ->
    register(?RP_SCHED, self()),
    %% The mailbox is flushed mainly to discard possible `exit` messages
    %% before enabling the `trap_exit` flag.
    concuerror_util:flush_mailbox(),
    process_flag(trap_exit, true),
    %% Initialize state table.
    state_start(),
    %% Save empty replay state for the first run.
    InitState = concuerror_state:empty(),
    state_save([InitState]),
    Result = interleave_outer_loop(Target, 0, [], -1, PreBound),
    state_stop(),
    unregister(?RP_SCHED),
    Parent ! {interleave_result, Result}.

interleave_dpor(Target, PreBound, Parent, DporFlavor) ->
    ?f_debug("Dpor is not really ready yet...\n"),
    register(?RP_SCHED, self()),
    Result = interleave_dpor(Target, PreBound, DporFlavor),
    Parent ! {interleave_result, Result}.

interleave_outer_loop(_T, RunCnt, Tickets, MaxBound, MaxBound) ->
    interleave_outer_loop_ret(Tickets, RunCnt);
interleave_outer_loop(Target, RunCnt, Tickets, CurrBound, MaxBound) ->
    {NewRunCnt, TotalTickets, Stop} = interleave_loop(Target, 1, Tickets),
    TotalRunCnt = NewRunCnt + RunCnt,
    state_swap(),
    case state_peak() of
        no_state -> interleave_outer_loop_ret(TotalTickets, TotalRunCnt);
        _State ->
            case Stop of
                true -> interleave_outer_loop_ret(TotalTickets, TotalRunCnt);
                false ->
                    interleave_outer_loop(Target, TotalRunCnt, TotalTickets,
                                          CurrBound + 1, MaxBound)
            end
    end.

interleave_outer_loop_ret([], RunCnt) ->
    {ok, RunCnt};
interleave_outer_loop_ret(Tickets, RunCnt) ->
    {error, RunCnt, Tickets}.

%%------------------------------------------------------------------------------

-type s_i()        :: non_neg_integer().
-type instr()      :: term().
-type transition() :: {concuerror_lid:lid(), instr()}.
-type clock_map()  :: dict(). %% dict(concuerror_lid:lid(), clock_vector()).
%% -type clock_vector() :: dict(). %% dict(concuerror_lid:lid(), s_i()).

-record(trace_state, {
          i         = 0                 :: s_i(),
          last      = init_tr()         :: transition(),
          enabled   = ordsets:new()     :: ordsets:ordset(concuerror_lid:lid()),
          blocked   = ordsets:new()     :: ordsets:ordset(concuerror_lid:lid()),
          pollable  = ordsets:new()     :: ordsets:ordset(concuerror_lid:lid()),
          backtrack = ordsets:new()     :: ordsets:ordset(concuerror_lid:lid()),
          done      = ordsets:new()     :: ordsets:ordset(concuerror_lid:lid()),
          sleep_set = ordsets:new()     :: ordsets:ordset(concuerror_lid:lid()),
          nexts     = dict:new()        :: dict(), %% dict(concuerror_lid:lid(), instr()),
          error_nxt = none              :: concuerror_lid:lid() | 'none',
          clock_map = empty_clock_map() :: clock_map(),
          preemptions = 0               :: non_neg_integer(),
          lid_trace = new_lid_trace()   :: queue() %% queue({transition(),
                                                   %%        clock_vector()})
         }).

init_tr() ->
	{concuerror_lid:root_lid(), init}.

empty_clock_map() -> dict:new().

new_lid_trace() ->
    queue:in({init_tr(), empty_clock_vector()}, queue:new()).

empty_clock_vector() -> dict:new().

-type trace_state() :: #trace_state{}.

-record(dpor_state, {
          target              :: analysis_target(),
          run_count   = 1     :: pos_integer(),
          tickets     = []    :: [concuerror_ticket:ticket()],
          trace       = []    :: [trace_state()],
          must_replay = false :: boolean(),
          proc_before = []    :: [pid()],
          dpor_flavor = full  :: 'full' | 'fake' | 'flanagan',
          preemption_bound = inf :: non_neg_integer() | 'inf'
         }).

interleave_dpor(Target, PreBound, DporFlavor) ->
    ?f_debug("Interleave dpor!\n"),
    Procs = processes(),
    %% To be able to clean up we need to be trapping exits...
    process_flag(trap_exit, true),
    Trace = start_target(Target),
    ?f_debug("Target started!\n"),
    NewState = #dpor_state{trace = Trace, target = Target, proc_before = Procs,
                           dpor_flavor = DporFlavor, preemption_bound = PreBound},
    explore(NewState).

start_target(Target) ->
    FirstLid = start_target_op(Target),
    Next = wait_next(FirstLid, init),
    New = ordsets:new(),
    MaybeEnabled = ordsets:add_element(FirstLid, New),
    {Pollable, Enabled, Blocked} =
        update_lid_enabled(FirstLid, Next, New, MaybeEnabled, New),
    %% FIXME: check_messages and poll should also be called here for
    %%        instrumenting "black" initial messages.
    TraceTop =
        #trace_state{nexts = dict:store(FirstLid, Next, dict:new()),
                     enabled = Enabled, blocked = Blocked, backtrack = Enabled,
                     pollable = Pollable},
    [TraceTop].

start_target_op(Target) ->
    concuerror_lid:start(),
    {Mod, Fun, Args} = Target,
    NewFun = fun() -> apply(Mod, Fun, Args) end,
    SpawnFun = fun() -> concuerror_rep:spawn_fun_wrapper(NewFun) end,
    FirstPid = spawn(SpawnFun),
    concuerror_lid:new(FirstPid, noparent).

explore(MightNeedReplayState) ->
    case select_from_backtrack(MightNeedReplayState) of
        {ok, {Lid, Cmd} = Selected, State} ->
            case Cmd of
                {error, _ErrorInfo} ->
                    NewState = report_error(Selected, State),
                    explore(NewState);
                _Else ->
                    ?f_debug("Plan: ~P\n",[Selected, ?DEPTH]),
                    Next = wait_next(Lid, Cmd),
                    UpdState = update_trace(Selected, Next, State),
                    AllAddState = add_all_backtracks(UpdState),
                    ?f_debug("Next: ~P\n",[Next, ?DEPTH]),
                    NewState = add_some_next_to_backtrack(AllAddState),
                    explore(NewState)
            end;
        none ->
            NewState = report_possible_deadlock(MightNeedReplayState),
            case finished(NewState) of
                false -> explore(NewState);
                true -> dpor_return(NewState)
            end
    end.

select_from_backtrack(#dpor_state{trace = Trace} = MightNeedReplayState) ->
    %% FIXME: Pick first and don't really subtract.
    %% FIXME: This is actually the trace bottom...
    [TraceTop|RestTrace] = Trace,
    Backtrack = TraceTop#trace_state.backtrack,
    Done = TraceTop#trace_state.done,
    ?f_debug("------------\nExplore ~p\n------------\n",
             [TraceTop#trace_state.i + 1]),
    case ordsets:subtract(Backtrack, Done) of
	[] ->
            ?f_debug("Backtrack set explored\n",[]),
            none;
        [SelectedLid|_RestLids] ->
            ?f_debug("Have to try ~p...\n",[SelectedLid]),
            State =
                case MightNeedReplayState#dpor_state.must_replay of
                    true -> replay_trace(MightNeedReplayState);
                    false -> MightNeedReplayState
                end,
	    Instruction = dict:fetch(SelectedLid, TraceTop#trace_state.nexts),
            NewDone = ordsets:add_element(SelectedLid, Done),
            NewTraceTop = TraceTop#trace_state{done = NewDone},
            NewState = State#dpor_state{trace = [NewTraceTop|RestTrace]},
	    {ok, {SelectedLid, Instruction}, NewState}
    end.

replay_trace(#dpor_state{proc_before = ProcBefore} = State) ->
    ?f_debug("\nReplay is required...\n"),
    [#trace_state{lid_trace = LidTrace}|_] = State#dpor_state.trace,
    Target = State#dpor_state.target,
    concuerror_lid:stop(),
    proc_cleanup(processes() -- ProcBefore),
    start_target_op(Target),
    replay_lid_trace(0, LidTrace),
    flush_mailbox(),
    RunCnt = State#dpor_state.run_count,
    ?f_debug("Done replaying...\n\n"),
    State#dpor_state{run_count = RunCnt + 1, must_replay = false}.

replay_lid_trace(N, Queue) ->
    {V, NewQueue} = queue:out(Queue),
    case V of
        {value, {{Lid, Command} = Transition, VC}} ->
            ?f_debug(" ~-4w: ~P",[N, Transition, ?DEPTH]),
            _ = wait_next(Lid, Command),
            ?f_debug("."),
            _ = handle_instruction_op(Transition),
            ?f_debug("."),
            replace_messages(Lid, VC),
            ?f_debug("\n"),
            replay_lid_trace(N+1, NewQueue);
        empty ->
            ok
    end.

wait_next(Lid, {exit, {normal, _Info}}) ->
    continue(Lid),
    exited;
wait_next(Lid, Prev) ->
    continue(Lid),
    Replace =
        case Prev of
            {Spawn, _Info}
              when Spawn =:= spawn; Spawn =:= spawn_link;
                   Spawn =:= spawn_monitor ->
                {true,
                 %% This interruption happens to make sure that a child has an
                 %% LID before the parent wants to do any operation with its PID.
                 receive
                     #sched{msg = spawned,
                            lid = Lid,
                            misc = Info,
                            type = prev} = Msg ->
                         case Spawn =:= spawn_monitor of
                             true ->
                                 {Pid, Ref} = Info,
                                 ChildLid = concuerror_lid:new(Pid, Lid),
                                 MonRef = concuerror_lid:ref_new(ChildLid, Ref),
                                 Msg#sched{misc = {ChildLid, MonRef}};
                             false ->
                                 Msg#sched{misc = concuerror_lid:new(Info, Lid)}
                         end
                 end};
            {ets, {new, _Info}} ->
                {true,
                 receive
                     #sched{msg = ets, lid = Lid, misc = {new, [Tid|Rest]},
                            type = prev} = Msg ->
                         NewMisc = {new, [concuerror_lid:ets_new(Tid)|Rest]},
                         Msg#sched{misc = NewMisc}
                 end};
            {monitor, _Info} ->
                {true,
                 receive
                     #sched{msg = monitor, lid = Lid, misc = {TLid, Ref},
                            type = prev} = Msg ->
                         NewMisc = {TLid, concuerror_lid:ref_new(TLid, Ref)},
                         Msg#sched{misc = NewMisc}
                 end};
            _Other ->
                false
        end,
    case Replace of
        {true, NewMsg} ->
            continue(Lid),
            self() ! NewMsg,
            get_next(Lid);
        false ->
            get_next(Lid)
    end.

get_next(Lid) ->
    receive
        #sched{msg = Type, lid = Lid, misc = Misc, type = next} ->
            {Type, Misc}
    end.

may_have_dependencies({_Lid, {error, _}}) -> false;
may_have_dependencies({_Lid, {Spawn, _}})
  when Spawn =:= spawn; Spawn =:= spawn_link; Spawn =:= spawn_monitor -> false;
may_have_dependencies({_Lid, {'receive', _}}) -> false;
may_have_dependencies({_Lid, exited}) -> false;
may_have_dependencies(_Else) -> true.


%% All Trace:
%%   dependent(New, Older)
%% Local Trace:
%%   dependent(Possible, Chosen)

dependent(A, B) ->
    dependent(A, B, false).

dependent({X, _}, {X, _}, false) ->
    %% You are already in the backtrack set.
    false;

%% Register and unregister have the same dependencies.
%% Use a unique value for the Pid to avoid checks there.
dependent({Lid, {unregister, RegName}}, B, false) ->
    dependent({Lid, {register, {RegName, make_ref()}}}, B, false);
dependent(A, {Lid, {unregister, RegName}}, false) ->
    dependent(A, {Lid, {register, {RegName, make_ref()}}}, false);

%% Sending requires at least a different message.
dependent({_Lid1, {send, {_Orig1, Lid, Msg1}}},
          {_Lid2, {send, {_Orig2, Lid, Msg2}}}, false) ->
    Msg1 =/= Msg2;


%% ETS operations live in their own small world.
dependent({_Lid1, {ets, Op1}}, {_Lid2, {ets, Op2}}, false) ->
    dependent_ets(Op1, Op2);

%% Registering a table with the same name as an existing one.
dependent({_Lid1, {ets, {new, [_Table, Name, Options]}}},
          {_Lid2, {exit, {normal, {Tables, _Name, _Links}}}},
          _Swap) ->
    PublicNamedTables = [N || {_Lid, {ok, N}} <- Tables],
    lists:all(fun(X) -> X end,
              [lists:member(E,L) || {E, L} <- [{named_table, Options},
                                               {public, Options},
                                               {Name, PublicNamedTables}]]);

%% Table owners exits mess things up.
dependent({_Lid1, {ets, {_Op, [Table|_Rest]}}},
          {_Lid2, {exit, {normal, {Tables, _Name, _Links}}}},
          _Swap) ->
    lists:keymember(Table, 1, Tables);

%% Sending to an activated after clause depends on that receive's patterns
dependent({_Lid1, {send, {_Orig, Lid, Msg}}}, {Lid, {'after', Fun}}, _Swap) ->
    Fun(Msg);


%% Registered processes:

%% Sending using name to a process that may exit and unregister.
dependent({_Lid1, {send, {Orig, _Lid, _Msg}}},
          {_Lid2, {exit, {normal, {_Tables, {ok, Orig}, _Links}}}}, _Swap) ->
    true;

%% Send using name before process has registered itself.
dependent({_Lid1, {register, {RegName, _PLid}}},
          {_Lid2, {send, {RegName, _Lid, _Msg}}}, _Swap) ->
    true;

%% Two registers using the same name.
dependent({_Lid1, {register, {RegName, _PLid1}}},
          {_Lid2, {register, {RegName, _PLid2}}}, false) ->
    true;

%% Two registers for the same process.
dependent({_Lid1, {register, {_RegName1, PLid}}},
          {_Lid2, {register, {_RegName2, PLid}}}, false) ->
    true;

%% Register a process that may exit.
dependent({_Lid1, {register, {_RegName, PLid}}},
          {PLid, {exit, {normal, _Info}}}, _Swap) ->
    true;

%% Register for a name that might be in use.
dependent({_Lid1, {register, {Name, _PLid}}},
          {_Lid2, {exit, {normal, {_Tables, {ok, Name}, _Links}}}}, _Swap) ->
    true;

%% Whereis using name before process has registered itself.
dependent({_Lid1, {register, {RegName, _PLid1}}},
          {_Lid2, {whereis, {RegName, _PLid2}}}, _Swap) ->
    true;

%% Process alive and exits
dependent({_Lid1, {is_process_alive, Lid}},
          {Lid, {exit, {normal, _Info}}}, _Swap) ->
    true;

%% Process registered and exits
dependent({_Lid1, {whereis, {Name, _PLid1}}},
          {_Lid2, {exit, {normal, {_Tables, {ok, Name}, _Links}}}}, _Swap) ->
    true;

%% Monitor/Demonitor and exit.
dependent({_Lid, {Chain, TLid}},
          {TLid, {exit, {normal, _Info}}}, _Swap)
  when Chain =:= demonitor; Chain =:= link; Chain =:= unlink ->
    true;
dependent({_Lid, {monitor, {TLid, _MonRef}}},
          {TLid, {exit, {normal, _Info}}}, _Swap) ->
    true;

%% Trap exits flag and linked process exiting.
dependent({_Lid1, {process_flag, {trap_exit, _Value, Links}}},
          {Lid2, {exit, {normal, {_Tables, _Name}}}}, _Swap) ->
    lists:member(Lid2, Links);

%% Swap the two arguments if the test is not symmetric by itself.
dependent(TransitionA, TransitionB, false) ->
    dependent(TransitionB, TransitionA, true);
dependent(_TransitionA, _TransitionB, true) ->
    false.


%% ETS table dependencies:

dependent_ets(Op1, Op2) ->
    dependent_ets(Op1, Op2, false).

dependent_ets({insert, [T, _, {K, V1}]}, {insert, [T, _, {K, V2}]}, false) ->
    V1 =/= V2;
dependent_ets({insert_new, [T, _, {K, _}]},
              {insert_new, [T, _, {K, _}]}, false) ->
    true;
dependent_ets({insert_new, [T, _, {K, _}]}, {insert, [T, _, {K, _}]}, _Swap) ->
    true;
dependent_ets({Insert, [T, _, {K, _}]}, {lookup, [T, _, K]}, _Swap)
  when Insert =:= insert; Insert =:= insert_new ->
    true;
dependent_ets({delete, [T, _]}, {_, [T|_]}, _Swap) ->
    true;
dependent_ets({new, [_Tid1, Name, Options1]},
              {new, [_Tid2, Name, Options2]}, false) ->
    lists:all(fun(X) -> X end,
              [lists:member(O, L) || O <- [named_table, public],
                                     L <- [Options1, Options2]]);
dependent_ets(Op1, Op2, false) ->
    dependent_ets(Op2, Op1, true);
dependent_ets(_Op1, _Op2, true) ->
    false.


add_all_backtracks(#dpor_state{preemption_bound = PreBound,
                               trace = Trace} = State) ->
    case State#dpor_state.dpor_flavor of
        fake ->
            %% add_some_next will take care of all the backtracks.
            State;
        Flavor ->
            [#trace_state{last = Transition}|_] = Trace,
            case may_have_dependencies(Transition) of
                true ->
                    NewTrace =
                        add_all_backtracks_trace(Transition, Trace,
                                                 PreBound, Flavor),
                    State#dpor_state{trace = NewTrace};
                false -> State
            end
    end.

add_all_backtracks_trace({Lid, _} = Transition, Trace, PreBound, Flavor) ->
    [#trace_state{i = I} = Top|
     [#trace_state{clock_map = ClockMap}|_] = PTrace] = Trace,
    ClockVector = dict:store(Lid, I, lookup_clock(Lid, ClockMap)),
    add_all_backtracks_trace(Transition, ClockVector, PreBound,
                             Flavor, PTrace, [Top]).

add_all_backtracks_trace(_Transition, _ClockVector, _PreBound,
                         _Flavor, [_] = Init, Acc) ->
    lists:reverse(Acc, Init);
add_all_backtracks_trace(Transition, ClockVector, PreBound, Flavor,
                         [#trace_state{preemptions = Preempt} = StateI|Trace],
                         Acc)
  when Preempt + 1 > PreBound ->
    add_all_backtracks_trace(Transition, ClockVector, PreBound, Flavor,
                             Trace, [StateI|Acc]);
add_all_backtracks_trace(Transition, ClockVector, PreBound, Flavor,
                         [StateI|Trace], Acc) ->
    #trace_state{i = I, last = {ProcSI, _} = SI} = StateI,
    Clock = lookup_clock_value(ProcSI, ClockVector),
    Action =
        case I > Clock andalso dependent(Transition, SI) of
            false -> {continue, ClockVector};
            true ->
                ?f_debug("~4w: ~p ~P Clock ~p\n",
                         [I, dependent(Transition, SI), SI, ?DEPTH, Clock]),
                [#trace_state{enabled = Enabled,
                              backtrack = Backtrack,
                              sleep_set = SleepSet} =
                     PreSI|Rest] = Trace,
                Candidates = ordsets:subtract(Enabled, SleepSet),
                Predecessors = predecessors(Candidates, I, ClockVector),
                case Flavor of
                    full ->
                        Initial =
                            ordsets:del_element(ProcSI, find_initial(I, Acc)),
                        ?f_debug("Initial: ~w\n",[Initial]),
                        case ordsets:intersection(Initial, Backtrack) of
                            [_|_] ->
                                ?f_debug("One initial already in backtrack.\n"),
                                {done, Trace};
                            [] ->
                                case Predecessors of
                                    [P|_] ->
                                        NewBacktrack =
                                            ordsets:add_element(P, Backtrack),
                                        ?f_debug("     Add: ~w\n", [P]),
                                        NewPreSI =
                                            PreSI#trace_state{backtrack =
                                                                  NewBacktrack},
                                        {done, [NewPreSI|Rest]};
                                    [] ->
                                        ?f_debug("     All sleeping...\n"),
                                        #trace_state{clock_map = ClockMap} =
                                            StateI,
                                        NewClockVector =
                                            lookup_clock(ProcSI, ClockMap),
                                        {continue, NewClockVector}
                                end
                        end;
                    flanagan ->
                        case ordsets:intersection(Predecessors, Backtrack) of
                            [_|_] ->
                                ?f_debug("One predecessor already"
                                         " in backtrack.\n"),
                                {done, Trace};
                            [] ->
                                case Predecessors of
                                    [P|_] ->
                                        NewBacktrack =
                                            ordsets:add_element(P, Backtrack),
                                        ?f_debug("     Add: ~w\n", [P]),
                                        NewPreSI =
                                            PreSI#trace_state{backtrack =
                                                                  NewBacktrack},
                                        {done, [NewPreSI|Rest]};
                                    [] ->
                                        NewBacktrack =
                                            ordsets:union(Candidates, Backtrack),
                                        ?f_debug("     Add: ~w\n", [Candidates]),
                                        NewPreSI =
                                            PreSI#trace_state{backtrack =
                                                                  NewBacktrack},
                                        {done, [NewPreSI|Rest]}
                                end
                        end
                end
        end,
    case Action of
        {continue, UpdClockVector} ->
            add_all_backtracks_trace(Transition, UpdClockVector, PreBound,
                                     Flavor, Trace, [StateI|Acc]);
        {done, FinalTrace} ->
            lists:reverse(Acc, [StateI|FinalTrace])
    end.

lookup_clock(P, ClockMap) ->
    case dict:find(P, ClockMap) of
        {ok, Clock} -> Clock;
        error -> dict:new()
    end.

lookup_clock_value(P, CV) ->
    case dict:find(P, CV) of
        {ok, Value} -> Value;
        error -> 0
    end.

find_initial(I, RevTrace) ->
    Empty = ordsets:new(),
    find_initial(I, RevTrace, Empty, Empty).

find_initial(_I, [], Initial, _NotInitial) ->
    Initial;
find_initial(I, [TraceTop|Rest], Initial, NotInitial) ->
    #trace_state{last = {P,_}, clock_map = ClockMap} = TraceTop,
    Add =
        case ordsets:is_element(P, Initial) orelse
            ordsets:is_element(P, NotInitial) of
            true -> false;
            false ->
                Clock = lookup_clock(P, ClockMap),
                case has_dependency_after(Clock, P, I) of
                    true -> not_initial;
                    false -> initial
                end
        end,
    case Add of
        false -> find_initial(I, Rest, Initial, NotInitial);
        initial ->
            NewInitial = ordsets:add_element(P, Initial),
            find_initial(I, Rest, NewInitial, NotInitial);
        not_initial ->
            NewNotInitial = ordsets:add_element(P, NotInitial),
            find_initial(I, Rest, Initial, NewNotInitial)            
    end.

has_dependency_after(Clock, P, I) ->
    Fold =
        fun(_Key, _Value, true) -> true;
           (Key, Value, false) -> P =/= Key andalso Value >= I
        end,
    dict:fold(Fold, false, Clock).                

predecessors(Candidates, I, ClockVector) ->
    Fold =
        fun(Lid, Acc) ->
                Clock = lookup_clock_value(Lid, ClockVector),
                ?f_debug("  ~p: ~p\n",[Lid, Clock]),
                case Clock > I of
                    false -> Acc;
                    true -> ordsets:add_element({Clock, Lid}, Acc)
                end
        end,
    [P || {_C, P} <- lists:foldl(Fold, ordsets:new(), Candidates)].

%% - add new entry with new entry
%% - wait any possible additional messages
%% - check for async
update_trace({Lid, _} = Selected, Next, State) ->
    #dpor_state{trace = [PrevTraceTop|Rest],
                dpor_flavor = Flavor} = State,
    #trace_state{i = I, enabled = Enabled, blocked = Blocked,
                 pollable = Pollable, done = Done,
                 nexts = Nexts, lid_trace = LidTrace,
                 clock_map = ClockMap, sleep_set = SleepSet,
                 preemptions = Preemptions, last = {LLid,_}} = PrevTraceTop,
    NewN = I+1,
    ClockVector = lookup_clock(Lid, ClockMap),
    BaseClockVector = dict:store(Lid, NewN, ClockVector),
    LidsClockVector = recent_dependency_cv(Selected, BaseClockVector, LidTrace),
    NewClockMap = dict:store(Lid, LidsClockVector, ClockMap),
    NewSleepSet =
        case Flavor of
            fake -> [];
            _Other ->
                NewSleepSetCandidates =
                    ordsets:union(ordsets:del_element(Lid, Done), SleepSet),
                filter_awaked(NewSleepSetCandidates, Nexts, Selected)
        end,
    NewNexts = dict:store(Lid, Next, Nexts),
    MaybeNotPollable = ordsets:del_element(Lid, Pollable),
    {NewPollable, NewEnabled, NewBlocked} =
        update_lid_enabled(Lid, Next, MaybeNotPollable, Enabled, Blocked),
    ErrorNext =
        case Next of
            {error, _} -> Lid;
            _Else -> none
        end,
    NewPreemptions =
        case ordsets:is_element(LLid, Enabled) of
            true ->
                case Lid =:= LLid of
                    false -> Preemptions + 1;
                    true -> Preemptions
                end;
            false -> Preemptions
        end,
    CommonNewTraceTop =
        #trace_state{i = NewN, last = Selected, nexts = NewNexts,
                     enabled = NewEnabled, blocked = NewBlocked,
                     clock_map = NewClockMap, sleep_set = NewSleepSet,
                     pollable = NewPollable, error_nxt = ErrorNext,
                     preemptions = NewPreemptions},
    InstrNewTraceTop = handle_instruction(Selected, CommonNewTraceTop),
    UpdatedClockVector =
        lookup_clock(Lid, InstrNewTraceTop#trace_state.clock_map),
    ?f_debug("Happened before: ~p\n", [dict:to_list(UpdatedClockVector)]),
    replace_messages(Lid, UpdatedClockVector),
    PossiblyRewrittenSelected = InstrNewTraceTop#trace_state.last,
    ?f_debug("Selected: ~P\n",[PossiblyRewrittenSelected, ?DEPTH]),
    NewLidTrace =
        queue:in({PossiblyRewrittenSelected, UpdatedClockVector}, LidTrace),
    {NewPrevTraceTop, NewTraceTop} =
        check_pollable(PrevTraceTop, InstrNewTraceTop),
    NewTrace =
        [NewTraceTop#trace_state{lid_trace = NewLidTrace},
         NewPrevTraceTop|Rest],
    State#dpor_state{trace = NewTrace}.

recent_dependency_cv({_Lid, {ets, _Info}} = Transition,
                     ClockVector, LidTrace) ->
    Fun =
        fun({Queue, CVAcc}) ->
            {Ret, NewQueue} = queue:out_r(Queue),
            case Ret of
                empty -> {done, CVAcc};
                {value, {Transition2, CV}} ->
                    case dependent(Transition, Transition2) of
                        true -> {cont, {NewQueue, max_cv(CVAcc, CV)}};
                        false -> {cont, {NewQueue, CVAcc}}
                    end
            end
        end,
    dynamic_loop_acc(Fun, {LidTrace, ClockVector});
recent_dependency_cv(_Transition, ClockVector, _Trace) ->
    ClockVector.

dynamic_loop_acc(Fun, Arg) ->
    case Fun(Arg) of
        {done, Ret} -> Ret;
        {cont, NewArg} -> dynamic_loop_acc(Fun, NewArg)
    end.
    
update_lid_enabled(Lid, Next, Pollable, Enabled, Blocked) ->
    {NewEnabled, NewBlocked} =
        case is_enabled(Next) of
            true -> {Enabled, Blocked};
            false ->
                ?f_debug("Blocking ~p\n",[Lid]),
                {ordsets:del_element(Lid, Enabled),
                 ordsets:add_element(Lid, Blocked)}
        end,
    NewPollable =
        case is_pollable(Next) of
            false -> Pollable;
            true -> ordsets:add_element(Lid, Pollable)
        end,
    {NewPollable, NewEnabled, NewBlocked}.

is_enabled({'receive', blocked}) -> false;
is_enabled(_Else) -> true.

is_pollable({'receive', blocked}) -> true;
is_pollable({'after', _Fun}) -> true;
is_pollable(_Else) -> false.

filter_awaked(SleepSet, Nexts, Selected) ->
    Filter =
        fun(Lid) ->
                Instr = dict:fetch(Lid, Nexts),
                not dependent({Lid, Instr}, Selected)
        end,
    [S || S <- SleepSet, Filter(S)].

%% Handle instruction is broken in two parts to reuse code in replay.
handle_instruction(Transition, TraceTop) ->
    Variables = handle_instruction_op(Transition),
    handle_instruction_al(Transition, TraceTop, Variables).

handle_instruction_op({Lid, {Spawn, _Info}})
  when Spawn =:= spawn; Spawn =:= spawn_link; Spawn =:= spawn_monitor ->
    ParentLid = Lid,
    Info =
        receive
            %% This is the replaced message
            #sched{msg = spawned, lid = ParentLid,
                   misc = Info0, type = prev} ->
                Info0
        end,
    ChildLid =
        case Spawn =:= spawn_monitor of
            true -> element(1, Info);
            false -> Info
        end,
    ChildNextInstr = wait_next(ChildLid, init),
    flush_mailbox(),
    {Info, ChildNextInstr};
handle_instruction_op({Lid, {ets, {new, _Info}}}) ->
    receive
        %% This is the replaced message
        #sched{msg = ets, lid = Lid, misc = {new, Info}, type = prev} ->
            Info
    end;
handle_instruction_op({Lid, {Updatable, _Info}})
  when Updatable =:= exit; Updatable =:= send; Updatable =:= whereis;
       Updatable =:= monitor; Updatable =:= 'receive';
       Updatable =:= process_flag ->
    receive
        #sched{msg = Updatable, lid = Lid, misc = Info, type = prev} ->
            Info
    end;
handle_instruction_op(_) ->
    flush_mailbox(),
    {}.

flush_mailbox() ->
    receive
        _Any ->
            ?f_debug("NONEMPTY!\n~p\n",[_Any]),
            flush_mailbox()
    after 0 ->
            ok
    end.

handle_instruction_al({Lid, {exit, _OldInfo}}, TraceTop, Info) ->
    #trace_state{enabled = Enabled, nexts = Nexts} = TraceTop,
    NewEnabled = ordsets:del_element(Lid, Enabled),
    NewNexts = dict:erase(Lid, Nexts),
    NewLast = {Lid, {exit, Info}},
    TraceTop#trace_state{enabled = NewEnabled, nexts = NewNexts,
                         last = NewLast};
handle_instruction_al({Lid, {Spawn, unknown}}, TraceTop,
                      {Info, ChildNextInstr})
  when Spawn =:= spawn; Spawn =:= spawn_link; Spawn =:= spawn_monitor ->
    ChildLid =
        case Spawn =:= spawn_monitor of
            true -> element(1, Info);
            false -> Info
        end,
    #trace_state{enabled = Enabled, blocked = Blocked,
                 nexts = Nexts, pollable = Pollable,
                 clock_map = ClockMap} = TraceTop,
    NewNexts = dict:store(ChildLid, ChildNextInstr, Nexts),
    ClockVector = lookup_clock(Lid, ClockMap),
    NewClockMap = dict:store(ChildLid, ClockVector, ClockMap),
    MaybeEnabled = ordsets:add_element(ChildLid, Enabled),
    {NewPollable, NewEnabled, NewBlocked} =
        update_lid_enabled(ChildLid, ChildNextInstr, Pollable,
                           MaybeEnabled, Blocked),
    NewLast = {Lid, {Spawn, Info}},
    TraceTop#trace_state{last = NewLast,
                         clock_map = NewClockMap,
                         enabled = NewEnabled,
                         blocked = NewBlocked,
                         pollable = NewPollable,
                         nexts = NewNexts};
handle_instruction_al({Lid, {'receive', Tag}}, TraceTop, {From, CV, Msg}) ->
    #trace_state{clock_map = ClockMap} = TraceTop,
    Vector = lookup_clock(Lid, ClockMap),
    Info =
        case Tag of
            unblocked -> {From, Msg};
            had_after ->
                When = lookup_clock_value(From, CV),
                {From, Msg, When}
        end,
    NewLast = {Lid, {'receive', Info}},
    NewVector = max_cv(Vector, CV),
    NewClockMap = dict:store(Lid, NewVector, ClockMap),
    TraceTop#trace_state{last = NewLast, clock_map = NewClockMap};
handle_instruction_al({Lid, {ets, {new, _Info}}}, TraceTop, Info) ->
    NewLast = {Lid, {ets, {new, Info}}},
    TraceTop#trace_state{last = NewLast};
handle_instruction_al({Lid, {Updatable, _Info}}, TraceTop, Info)
  when Updatable =:= send; Updatable =:= whereis; Updatable =:= monitor;
       Updatable =:= process_flag ->
    NewLast = {Lid, {Updatable, Info}},
    TraceTop#trace_state{last = NewLast};
handle_instruction_al({_Lid, {halt, _Status}}, TraceTop, {}) ->
    TraceTop#trace_state{enabled = [], blocked = [], error_nxt = none};
handle_instruction_al(_Transition, TraceTop, {}) ->
    TraceTop.

max_cv(D1, D2) ->
    Merger = fun(_Key, V1, V2) -> max(V1, V2) end,
    dict:merge(Merger, D1, D2).

check_pollable(OldTraceTop, TraceTop) ->
    #trace_state{pollable = Pollable} = TraceTop,
    PollableList = ordsets:to_list(Pollable),
    ?f_debug("Polling...\n"),
    lists:foldl(fun poll_all/2, {OldTraceTop, TraceTop}, PollableList).

poll_all(Lid, {OldTraceTop, TraceTop} = Original) ->
    case poll(Lid) of
        {'receive', Info} = Res when
              Info =:= unblocked;
              Info =:= had_after ->
            ?f_debug("  Poll ~p: ~p\n",[Lid, Res]),
            #trace_state{pollable = Pollable,
                         blocked = Blocked,
                         enabled = Enabled,
                         sleep_set = SleepSet,
                         nexts = Nexts} = TraceTop,
            #trace_state{backtrack = Backtrack} = OldTraceTop,
            {NewBacktrack, NewSleepSet} =
                case Info of
                    unblocked -> {Backtrack, SleepSet};
                    had_after ->
                        case ordsets:is_element(Lid, SleepSet) of
                            true ->
                                {Backtrack,
                                 ordsets:del_element(Lid, SleepSet)};
                            false ->
                                {ordsets:add_element(Lid, Backtrack),
                                 SleepSet}
                        end
                end,
            NewPollable = ordsets:del_element(Lid, Pollable),
            NewBlocked = ordsets:del_element(Lid, Blocked),
            NewEnabled = ordsets:add_element(Lid, Enabled),
            NewNexts = dict:store(Lid, Res, Nexts),
            {OldTraceTop#trace_state{backtrack = NewBacktrack},
             TraceTop#trace_state{pollable = NewPollable,
                                  blocked = NewBlocked,
                                  enabled = NewEnabled,
                                  sleep_set = NewSleepSet,
                                  nexts = NewNexts}};
        _Else ->
            Original
    end.

add_some_next_to_backtrack(State) ->
    #dpor_state{trace = [TraceTop|Rest], dpor_flavor = Flavor,
                preemption_bound = PreBound} = State,
    #trace_state{enabled = Enabled, sleep_set = SleepSet,
                 error_nxt = ErrorNext, last = {Lid, _},
                 preemptions = Preemptions} = TraceTop,
    ?f_debug("Pick random: Enabled: ~w Sleeping: ~w\n",
             [Enabled, SleepSet]),
    Backtrack =
        case ErrorNext of
            none ->
                case Flavor of
                    fake ->
                        case ordsets:is_element(Lid, Enabled) of
                            true when Preemptions =:= PreBound ->
                                [Lid];
                            _Else -> Enabled
                        end;
                    _Other ->
                        case ordsets:subtract(Enabled, SleepSet) of
                            [] -> [];
                            [H|_] = Candidates ->
                                case ordsets:is_element(Lid, Candidates) of
                                    true -> [Lid];
                                    false -> [H]
                                end
                        end
                end;
            Else -> [Else]
        end,
    ?f_debug("Picked: ~w\n",[Backtrack]),
    NewTraceTop = TraceTop#trace_state{backtrack = Backtrack},
    State#dpor_state{trace = [NewTraceTop|Rest]}.

report_error(Transition, State) ->
    #dpor_state{trace = [TraceTop|_], tickets = Tickets} = State,
    ?f_debug("ERROR!\n~P\n",[Transition, ?DEPTH]),
    Error = convert_error_info(Transition),
    LidTrace = queue:in({Transition, foo}, TraceTop#trace_state.lid_trace),
    Ticket = create_ticket(Error, LidTrace),
    State#dpor_state{must_replay = true, tickets = [Ticket|Tickets]}.

create_ticket(Error, LidTrace) ->
    InitTr = init_tr(),
    [{P1, init} = InitTr|Trace] = [S || {S,_V} <- queue:to_list(LidTrace)],
    InitSet = sets:add_element(P1, sets:new()),
    {ErrorState, _Procs} =
        lists:mapfoldl(fun convert_error_trace/2, InitSet, Trace),
    Ticket = concuerror_ticket:new(Error, ErrorState),
    concuerror_log:show_error(Ticket),
    Ticket.

convert_error_trace({Lid, {error, [ErrorOrThrow,Kind|_]}}, Procs)
  when ErrorOrThrow =:= error; ErrorOrThrow =:= throw ->
    Msg =
        concuerror_error:type(concuerror_error:new({Kind, foo})),    
    {{exit, Lid, Msg}, Procs};
convert_error_trace({Lid, {Instr, Extra}}, Procs) ->
    NewProcs =
        case Instr of
            Spawn when Spawn =:= spawn; Spawn =:= spawn_link;
                       Spawn =:= spawn_monitor ->
                NewLid =
                    case Spawn =:= spawn_monitor of
                        true -> element(1, Extra);
                        false -> Extra
                    end,
                sets:add_element(NewLid, Procs);
            exit   -> sets:del_element(Lid, Procs);
            _ -> Procs
        end,
    NewInstr =
        case Instr of
            send ->
                {Orig, Dest, Msg} = Extra,
                NewDest =
                    case is_atom(Orig) of
                        true -> {name, Orig};
                        false -> check_lid_liveness(Dest, NewProcs)
                    end,
                {send, Lid, NewDest, Msg};
            'receive' ->
                {Origin, Msg} =
                    case Extra of
                        {O, M, _W} -> {O, M};
                        _Else -> Extra
                    end,
                {'receive', Lid, Origin, Msg};
            'after' ->
                {'after', Lid};
            is_process_alive ->
                {is_process_alive, Lid, check_lid_liveness(Extra, NewProcs)};
            TwoArg when TwoArg =:= register;
                        TwoArg =:= whereis ->
                {Name, TLid} = Extra,
                {TwoArg, Lid, Name, check_lid_liveness(TLid, NewProcs)};
            process_flag ->
                {trap_exit, Value, _Links} = Extra,
                {process_flag, Lid, trap_exit, Value};
            exit ->
                {exit, Lid, normal};
            Monitor when Monitor =:= monitor;
                         Monitor =:= spawn_monitor ->
                {TLid, _RefLid} = Extra,
                {Monitor, Lid, check_lid_liveness(TLid, NewProcs)};
            ets ->
                case Extra of
                    {new, [_EtsLid, Name, Options]} ->
                        {ets_new, Lid, {Name, Options}};
                    {insert, [_EtsLid, Tid, Objects]} ->
                        {ets_insert, Lid, {Tid, Objects}};
                    {insert_new, [_EtsLid, Tid, Objects]} ->
                        {ets_insert_new, Lid, {Tid, Objects}};
                    {lookup, [_EtsLid, Tid, Key]} ->
                        {ets_lookup, Lid, {Tid, Key}};
                    {delete, [_EtsLid, Tid]} ->
                        {ets_delete, Lid, Tid}
                end;
            _ ->
                {Instr, Lid, Extra}
        end,
    {NewInstr, NewProcs}.


check_lid_liveness(not_found, _Live) ->
    not_found;
check_lid_liveness(Lid, Live) ->
    case sets:is_element(Lid, Live) of
        true -> Lid;
        false -> {dead, Lid}
    end.

convert_error_info({_Lid, {error, [Kind, Type, Stacktrace]}})->
    NewType =
        case Kind of
            error -> Type;
            throw -> {nocatch, Type};
            exit -> Type
        end,
    {Tag, Details} = concuerror_error:new({NewType, foo}),
    Info =
        case Tag of
            exception -> {NewType, Stacktrace};
            assertion_violation -> Details
        end,
    {Tag, Info}.

report_possible_deadlock(State) ->
    #dpor_state{trace = [TraceTop|Trace], tickets = Tickets} = State,
    NewTickets =
        case TraceTop#trace_state.enabled of
            [] ->
                case TraceTop#trace_state.blocked of
                    [] -> Tickets;
                    Blocked ->
                        ?f_debug("DEADLOCK!\n"),
                        Error = {deadlock, Blocked},
                        LidTrace = TraceTop#trace_state.lid_trace,
                        Ticket = create_ticket(Error, LidTrace),
                        [Ticket|Tickets]
                end;
            _Else ->
                Tickets
        end,
    ?f_debug("Stack frame dropped\n"),
    State#dpor_state{must_replay = true, trace = Trace, tickets = NewTickets}.

finished(#dpor_state{trace = Trace}) ->
    Trace =:= [].

dpor_return(State) ->
    RunCnt = State#dpor_state.run_count,
    case State#dpor_state.tickets of
        [] -> {ok, RunCnt};
        Tickets -> {error, RunCnt, Tickets}
    end.

%%------------------------------------------------------------------------------

%% Main loop for producing process interleavings.
%% The first process (FirstPid) is created linked to the scheduler,
%% so that the latter can receive the former's exit message when it
%% terminates. In the same way, every process that may be spawned in
%% the course of the program shall be linked to the scheduler process.
interleave_loop(Target, RunCnt, Tickets) ->
    %% Lookup state to replay.
    case state_load() of
        no_state -> {RunCnt - 1, Tickets, false};
        ReplayState ->
            ?debug_1("Running interleaving ~p~n", [RunCnt]),
            ?debug_1("----------------------~n"),
            concuerror_lid:start(),
            %% Save current process list (any process created after
            %% this will be cleaned up at the end of the run)
            ProcBefore = processes(),
            %% Spawn initial user process
            {Mod, Fun, Args} = Target,
            NewFun = fun() -> wait(), apply(Mod, Fun, Args) end,
            FirstPid = spawn_link(NewFun),
            %% Initialize scheduler context
            FirstLid = concuerror_lid:new(FirstPid, noparent),
            Active = ?SETS:add_element(FirstLid, ?SETS:new()),
            Blocked = ?SETS:new(),
            State = concuerror_state:empty(),
            Context = #context{active=Active, state=State,
                blocked=Blocked, actions=[]},
            %% Interleave using driver
            Ret = driver(Context, ReplayState),
            %% Cleanup
            proc_cleanup(processes() -- ProcBefore),
            concuerror_lid:stop(),
            NewTickets =
                case Ret of
                    {error, Error, ErrorState} ->
                        Ticket = concuerror_ticket:new(Error, ErrorState),
                        concuerror_log:show_error(Ticket),
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
                    interleave_loop(Target, NewRunCnt, NewTickets)
            end
    end.

%%%----------------------------------------------------------------------
%%% Core components
%%%----------------------------------------------------------------------

driver(Context, ReplayState) ->
    case concuerror_state:is_empty(ReplayState) of
        true -> driver_normal(Context);
        false -> driver_replay(Context, ReplayState)
    end.

driver_replay(Context, ReplayState) ->
    {Next, Rest} = concuerror_state:trim_head(ReplayState),
    NewContext = run(Context#context{current = Next, error = ?NO_ERROR}),
    #context{blocked = NewBlocked} = NewContext,
    case concuerror_state:is_empty(Rest) of
        true ->
            case ?SETS:is_element(Next, NewBlocked) of
                %% If the last action of the replayed state prefix is a block,
                %% we can safely abort.
                true -> abort;
                %% Replay has finished; proceed in normal mode, after checking
                %% for errors during the last replayed action.
                false -> check_for_errors(NewContext)
            end;
        false ->
            case ?SETS:is_element(Next, NewBlocked) of
                true -> concuerror_log:internal(
                        "Proc. ~p should be active.", [Next]);
                false -> driver_replay(NewContext, Rest)
            end
    end.

driver_normal(#context{active=Active, current=LastLid,
                       state = State} = Context) ->
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
    insert_states(State, Insert),
    check_for_errors(NewContext).

%% Handle four possible cases:
%% - An error occured during the execution of the last process =>
%%   Terminate the run and report the erroneous interleaving sequence.
%% - Only blocked processes exist =>
%%   Terminate the run and report a deadlock.
%% - No active or blocked processes exist =>
%%   Terminate the run without errors.
%% - There exists at least one active process =>
%%   Continue run.
check_for_errors(#context{error=NewError, actions=Actions, active=NewActive,
                          blocked=NewBlocked} = NewContext) ->
    case NewError of
        ?NO_ERROR ->
            case ?SETS:size(NewActive) of
                0 ->
                    case ?SETS:size(NewBlocked) of
                        0 -> ok;
                        _NonEmptyBlocked ->
                            Deadlock =
                                concuerror_error:new({deadlock, NewBlocked}),
                            ErrorState = lists:reverse(Actions),
                            {error, Deadlock, ErrorState}
                    end;
                _NonEmptyActive -> driver_normal(NewContext)
            end;
        _Other ->
            ErrorState = lists:reverse(Actions),
            {error, NewError, ErrorState}
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

insert_states(State, {Lids, current}) ->
    Extend = lists:map(fun(L) -> concuerror_state:extend(State, L) end, Lids),
    state_save(Extend);
insert_states(State, {Lids, next}) ->
    Extend = lists:map(fun(L) -> concuerror_state:extend(State, L) end, Lids),
    state_save_next(Extend).

%% Run process Lid in context Context until it encounters a preemption point.
run(#context{current = Lid, state = State} = Context) ->
    ?debug_2("Running process ~s.~n", [concuerror_lid:to_string(Lid)]),
    %% Create new state by adding this process.
    NewState = concuerror_state:extend(State, Lid),
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
            case concuerror_lid:from_pid(Pid) of
                not_found -> dispatch(Context);
                Lid -> handler(exit, Lid, Context, Reason)
            end
    end.

%%%----------------------------------------------------------------------
%%% Handlers
%%%----------------------------------------------------------------------

handler('after', Lid, #context{actions=Actions}=Context, _Misc) ->
    Action = {'after', Lid},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions};

%% Move the process to the blocked set.
handler(block, Lid,
        #context{active=Active, blocked=Blocked, actions=Actions} = Context,
        _Misc) ->
    Action = {'block', Lid},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    NewActive = ?SETS:del_element(Lid, Active),
    NewBlocked = ?SETS:add_element(Lid, Blocked),
    Context#context{active=NewActive, blocked=NewBlocked, actions=NewActions};

handler(demonitor, Lid, #context{actions=Actions}=Context, _Ref) ->
    %% TODO: Get LID from Ref?
    TargetLid = concuerror_lid:mock(0),
    Action = {demonitor, Lid, TargetLid},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions};

%% Remove the exited process from the active set.
%% NOTE: This is called after a process has exited, not when it calls
%%       exit/1 or exit/2.
handler(exit, Lid, #context{active = Active, actions = Actions} = Context,
        Reason) ->
    NewActive = ?SETS:del_element(Lid, Active),
    %% Cleanup LID stored info.
    concuerror_lid:cleanup(Lid),
    %% Handle and propagate errors.
    case Reason of
        normal ->
            Action1 = {exit, Lid, normal},
            NewActions1 = [Action1 | Actions],
            ?debug_1(concuerror_proc_action:to_string(Action1) ++ "~n"),
            Context#context{active = NewActive, actions = NewActions1};
        _Else ->
            Error = concuerror_error:new(Reason),
            Action2 = {exit, Lid, concuerror_error:type(Error)},
            NewActions2 = [Action2 | Actions],
            ?debug_1(concuerror_proc_action:to_string(Action2) ++ "~n"),
            Context#context{active=NewActive, error=Error, actions=NewActions2}
    end;

%% Return empty active and blocked queues to force run termination.
handler(halt, Lid, #context{actions = Actions}=Context, Misc) ->
    Action =
        case Misc of
            empty  -> {halt, Lid};
            Status -> {halt, Lid, Status}
        end,
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{active=?SETS:new(),blocked=?SETS:new(),actions=NewActions};

handler(is_process_alive, Lid, #context{actions=Actions}=Context, TargetPid) ->
    TargetLid = concuerror_lid:from_pid(TargetPid),
    Action = {is_process_alive, Lid, TargetLid},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions};

handler(link, Lid, #context{actions = Actions}=Context, TargetPid) ->
    TargetLid = concuerror_lid:from_pid(TargetPid),
    Action = {link, Lid, TargetLid},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions};

handler(monitor, Lid, #context{actions = Actions}=Context, {Item, _Ref}) ->
    TargetLid = concuerror_lid:from_pid(Item),
    Action = {monitor, Lid, TargetLid},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions};

handler(process_flag, Lid, #context{actions=Actions}=Context, {Flag, Value}) ->
    Action = {process_flag, Lid, Flag, Value},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions};

%% Normal receive message handler.
handler('receive', Lid, #context{actions = Actions}=Context, {From, Msg}) ->
    Action = {'receive', Lid, From, Msg},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions};

%% Receive message handler for special messages, like 'EXIT' and 'DOWN',
%% which don't have an associated sender process.
handler('receive_no_instr', Lid, #context{actions = Actions}=Context, Msg) ->
    Action = {'receive_no_instr', Lid, Msg},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions};

handler(register, Lid, #context{actions=Actions}=Context, {RegName, RegLid}) ->
    Action = {register, Lid, RegName, RegLid},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions};

handler(send, Lid, #context{actions = Actions}=Context, {DstPid, Msg}) ->
    DstLid = concuerror_lid:from_pid(DstPid),
    Action = {send, Lid, DstLid, Msg},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions};

%% Link the newly spawned process to the scheduler process and add it to the
%% active set.
handler(spawn, ParentLid,
        #context{active= Active, actions = Actions} = Context, ChildPid) ->
    link(ChildPid),
    ChildLid = concuerror_lid:new(ChildPid, ParentLid),
    Action = {spawn, ParentLid, ChildLid},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    NewActive = ?SETS:add_element(ChildLid, Active),
    Context#context{active = NewActive, actions = NewActions};

%% FIXME: Refactor this (it's exactly the same as 'spawn')
handler(spawn_link, ParentLid,
        #context{active = Active, actions = Actions} = Context, ChildPid) ->
    link(ChildPid),
    ChildLid = concuerror_lid:new(ChildPid, ParentLid),
    Action = {spawn_link, ParentLid, ChildLid},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    NewActive = ?SETS:add_element(ChildLid, Active),
    Context#context{active = NewActive, actions = NewActions};

%% FIXME: Refactor this (it's almost the same as 'spawn')
handler(spawn_monitor, ParentLid,
        #context{active=Active, actions=Actions}=Context, {ChildPid, _Ref}) ->
    link(ChildPid),
    ChildLid = concuerror_lid:new(ChildPid, ParentLid),
    Action = {spawn_monitor, ParentLid, ChildLid},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    NewActive = ?SETS:add_element(ChildLid, Active),
    Context#context{active = NewActive, actions = NewActions};

%% Similar to above depending on options.
handler(spawn_opt, ParentLid,
        #context{active=Active, actions=Actions}=Context, {Ret, Opt}) ->
    {ChildPid, _Ref} =
        case Ret of
            {_C, _R} = CR -> CR;
            C -> {C, noref}
        end,
    link(ChildPid),
    ChildLid = concuerror_lid:new(ChildPid, ParentLid),
    Opts = sets:to_list(sets:intersection(sets:from_list([link, monitor]),
                                          sets:from_list(Opt))),
    Action = {spawn_opt, ParentLid, ChildLid, Opts},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    NewActive = ?SETS:add_element(ChildLid, Active),
    Context#context{active = NewActive, actions = NewActions};

handler(unlink, Lid, #context{actions = Actions}=Context, TargetPid) ->
    TargetLid = concuerror_lid:from_pid(TargetPid),
    Action = {unlink, Lid, TargetLid},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions};

handler(unregister, Lid, #context{actions = Actions}=Context, RegName) ->
    Action = {unregister, Lid, RegName},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions};

handler(whereis, Lid, #context{actions = Actions}=Context, {RegName, Result}) ->
    ResultLid = concuerror_lid:from_pid(Result),
    Action = {whereis, Lid, RegName, ResultLid},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions};

%% Handler for anything "non-special". It just passes the arguments
%% for logging.
%% TODO: We may be able to delete some of the above that can be handled
%%       by this generic handler.
handler(CallMsg, Lid, #context{actions = Actions}=Context, Args) ->
    Action = {CallMsg, Lid, Args},
    NewActions = [Action | Actions],
    ?debug_1(concuerror_proc_action:to_string(Action) ++ "~n"),
    Context#context{actions = NewActions}.

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

%% Remove and return a state.
%% If no states available, return 'no_state'.
state_load() ->
    {Len1, Len2} = get(?NT_STATELEN),
    case get(?NT_STATE1) of
        [State | Rest] ->
            put(?NT_STATE1, Rest),
            concuerror_log:progress(log, Len1-1),
            put(?NT_STATELEN, {Len1-1, Len2}),
            concuerror_state:pack(State);
        [] -> no_state
    end.

%% Return a state without removing it.
%% If no states available, return 'no_state'.
state_peak() ->
    case get(?NT_STATE1) of
        [State|_] -> State;
        [] -> no_state
    end.

%% Add some states to the current `state` table.
state_save(State) ->
    Size = length(State),
    {Len1, Len2} = get(?NT_STATELEN),
    put(?NT_STATELEN, {Len1+Size, Len2}),
    put(?NT_STATE1, State ++ get(?NT_STATE1)).

%% Add some states to the next `state` table.
state_save_next(State) ->
    Size = length(State),
    {Len1, Len2} = get(?NT_STATELEN),
    put(?NT_STATELEN, {Len1, Len2+Size}),
    put(?NT_STATE2, State ++ get(?NT_STATE2)).

%% Initialize state tables.
state_start() ->
    put(?NT_STATE1, []),
    put(?NT_STATE2, []),
    put(?NT_STATELEN, {0, 0}),
    ok.

%% Clean up state table.
state_stop() ->
    ok.

%% Swap names of the two state tables and clear one of them.
state_swap() ->
    {_Len1, Len2} = get(?NT_STATELEN),
    concuerror_log:progress(swap, Len2),
    put(?NT_STATELEN, {Len2, 0}),
    put(?NT_STATE1, put(?NT_STATE2, [])).


%%%----------------------------------------------------------------------
%%% Instrumentation interface
%%%----------------------------------------------------------------------

%% Notify the scheduler of a blocked process.
-spec block() -> 'ok'.

block() ->
    notify(block, []).

%% Prompt process Pid to continue running.
continue(LidOrPid) ->
    send_message(LidOrPid, continue).

poll(Lid) ->
    send_message(Lid, poll),
    get_next(Lid).

send_message(Pid, Message) when is_pid(Pid) ->
    Pid ! #sched{msg = Message},
    ok;
send_message(Lid, Message) ->
    Pid = concuerror_lid:get_pid(Lid),
    Pid ! #sched{msg = Message},
    ok.

%% Notify the scheduler of an event.
%% If the calling user process has an associated LID, then send
%% a notification and yield. Otherwise, for an unknown process
%% running instrumented code completely ignore this call.
-spec notify(notification(), any()) -> 'ok' | 'continue' | 'poll'.

notify(Msg, Misc) ->
    notify(Msg, Misc, next).

-spec notify(notification(), any(), sched_msg_type()) ->
                    'ok' | 'continue' | 'poll'.

notify(Msg, Misc, Type) ->
    case lid_from_pid(self()) of
        not_found -> ok;
        Lid ->
            ?RP_SCHED_SEND ! #sched{msg = Msg, lid = Lid, misc = Misc, type = Type},
            case Type of
                next  ->
                    case Msg of
                        'receive' -> wait_poll_or_continue();
                        _Other -> wait()
                    end;
                _Else -> ok
            end
    end.

%% TODO: Maybe move into lid module.
-spec lid_from_pid(pid()) -> concuerror_lid:lid() | 'not_found'.

lid_from_pid(Pid) ->
    concuerror_lid:from_pid(Pid).

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
    wait_poll_or_continue(ok).

-spec wait_poll_or_continue() -> 'poll' | 'continue'.

wait_poll_or_continue() ->
    wait_poll_or_continue(continue).

-define(VECTOR_MSG(LID, VC),
        #sched{msg = vector, lid = LID, misc = VC, type = async}).

wait_poll_or_continue(Msg) ->
    receive
        #sched{msg = continue} -> Msg;
        #sched{msg = poll} -> poll;
        ?VECTOR_MSG(Lid, VC) ->
            instrument_my_messages(Lid, VC),
            notify(vector, ok, async),
            wait_poll_or_continue(Msg)
    end.

replace_messages(Lid, VC) ->
    %% Let "black" processes send any remaining messages.
    erlang:yield(),
    Unused = unused,
    Fun =
        fun(Pid, U) when U =:= Unused ->
            link(Pid),
            Pid ! ?VECTOR_MSG(Lid, VC),
            receive
                ?VECTOR_MSG(_PidsLid, ok) -> ok;
                {'EXIT', Pid, _Reason} ->
                    %% Process may have been asked to exit.
                    ok                                                        
            end,
            unlink(Pid),
            U
        end,
    concuerror_lid:fold_pids(Fun, Unused).

-define(IS_INSTR_MSG(Msg),
        (is_tuple(Msg) andalso
         size(Msg) =:= 4 andalso
         element(1, Msg) =:= ?INSTR_MSG)).

instrument_my_messages(Lid, VC) ->
    Self = self(),
    Check =
        fun() ->
                receive
                    Msg when not ?IS_INSTR_MSG(Msg) ->
                        Instr = {?INSTR_MSG, Lid, VC, Msg},
                        Self ! Instr,
                        cont
                after
                    0 -> done
                end
        end,
    dynamic_loop(Check).

dynamic_loop(Check) ->
    case Check() of
        done -> ok;
        cont -> dynamic_loop(Check)
    end.