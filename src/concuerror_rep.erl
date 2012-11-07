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
%%% Description : Replacement BIFs
%%%----------------------------------------------------------------------

-module(concuerror_rep).

-export([rep_after_notify/1, rep_receive/2, rep_receive_block/1, 
         rep_receive_notify/2, rep_receive_notify/3,
         rep_send/3, rep_spawn/3, rep_halt/3,
         rep_process_flag/3, rep_generic/3, rep_var/3]).

-include("gen.hrl").
-include("instr.hrl").

%%%----------------------------------------------------------------------
%%% Definitions and Types
%%%----------------------------------------------------------------------

%% Return the calling process' LID.
-define(LID_FROM_PID(Pid), concuerror_sched:lid_from_pid(Pid)).


%%%----------------------------------------------------------------------
%%% Callbacks
%%%----------------------------------------------------------------------

%% Handle Mod:Fun(Args) calls.
rep_var(Mod, Fun, Args) ->
    Key = {Mod, Fun, length(Args)},
    case lists:keyfind(Key, 1, ?INSTR_MOD_FUN) of
        {Key, Callback} -> apply(?MODULE, Callback, [Key, 0, Args]);
        false -> apply(Mod, Fun, Args)
    end.

rep_demonitor(Key, SrcLoc, [_Ref | _Rest]=Args) ->
    Result = apply(erlang, demonitor, Args),
    %% TODO: Get LID from Ref?
    concuerror_sched:notify(Key, {Args, Result}),
    Result.

rep_halt(Key, SrcLoc, Args) ->
    concuerror_sched:notify(Key, {Args, empty}).

rep_generic_lid({Mod, Fun, _Arity}=Key, SrcLoc, [Pid | Rest]=Args) ->
    Result = apply(Mod, Fun, Args),
    Lid = ?LID_FROM_PID(Pid),
    concuerror_sched:notify(Key, {[Lid | Rest], Result}),
    Result.

rep_monitor(Key, SrcLoc, [Type, Item]=Args) ->
    Result = apply(erlang, monitor, Args),
    Lid = ?LID_FROM_PID(find_pid(Item)),
    concuerror_sched:notify(Key, {[Type, Lid], Result}),
    Result.

rep_process_flag(Key, SrcLoc, [trap_exit,Value]=Args) ->
    Result = process_flag(trap_exit, Value),
    concuerror_sched:notify(Key, {Args, Result}),
    Result;
rep_process_flag(Key, SrcLoc, [Flag,Value]) ->
    process_flag(Flag, Value).

rep_register(Key, SrcLoc, [RegName, Pid]) ->
    Result = erlang:register(RegName, Pid),
    Lid = ?LID_FROM_PID(Pid),
    concuerror_sched:notify(Key, {[RegName, Lid], Result}),
    Result.

rep_spawn({_Mod, _Fun, Arity}=Key, SrcLoc, Args) ->
    case {?LID_FROM_PID(self()), Arity} of
        {not_found, _} -> apply(Mod, Fun, Args);
        {_Lid, 1} ->
            [SpawnFun] = Args,
            NewSpawnFun =
                fun() -> concuerror_sched:wait(), SpawnFun() end,
            rep_spawn_aux(Key, SrcLoc, NewSpawnFun, empty);
        {_Lid, 2} ->
            [SpawnFun, Opts] = Args,
            NewSpawnFun =
                fun() -> concuerror_sched:wait(), SpawnFun() end,
            rep_spawn_aux(Key, SrcLoc, NewSpawnFun, Opts);
        {_Lid, 3} ->
            [SpawnMod, SpawnFun, SpawnOpts] = Args,
            NewSpawnFun =
                fun() -> concuerror_sched:wait(),
                         apply(SpawnMod, SpawnFun, SpawnOpts)
                end,
            rep_spawn_aux(Key, SrcLoc, NewSpawnFun, empty);
        {_Lid, 4} ->
            [SpawnMod, SpawnFun, SpawnOpts, Opts] = Args,
            NewSpawnFun =
                fun() -> concuerror_sched:wait(),
                         apply(SpawnMod, SpawnFun, SpawnOpts)
                end,
            rep_spawn_aux(Key, SrcLoc, NewSpawnFun, Opts)
    end.

rep_spawn_aux({Mod, Fun, _Arity}=Key, SrcLoc, SpawnFun, Opts) ->
    Result = apply(Mod, Fun, NewSpawnFun),
    case Result of
        {ChildPid, _Ref} ->
            concuerror_sched:notify(Key, {Opts, ChildPid});
        ChildPid ->
            concuerror_sched:notify(Key, {Opts, ChildPid})
    end,
    Result.

rep_generic({Mod, Fun, _Arity}=Key, SrcLoc, Args) ->
    Result = apply(Mod, Fun, Args),
    concuerror_sched:notify(Key, {Args, Result}),
    Result.

%%%----------------------------------------------------------------------
%%% XXX: Instrument Send
%%%----------------------------------------------------------------------

%% If the target has a registered LID then instrument the message
%% and yield after sending. Otherwise, send the original message
%% and continue without yielding.
rep_send(Key, SrcLoc, [Dest, Msg | Opt] = Args) ->
    case ?LID_FROM_PID(self()) of
        not_found -> apply(erlang, send, Args);
        SelfLid ->
            NewDest = find_pid(Dest),
            case ?LID_FROM_PID(NewDest) of
                not_found -> apply(erlang, send, Args);
                _DestLid ->
                    InstrMsg = {?INSTR_MSG, SelfLid, Msg},
                    apply(erlang, send, [Dest, InstrMsg | Opt])
            end,
            concuerror_sched:notify(Key, {NewDest, Msg}),
            Msg
    end.


%%%----------------------------------------------------------------------
%%$ XXX: Instrument Receive
%%%----------------------------------------------------------------------

%% If a matching message is found in the process' message queue, continue
%% to actual receive statement, else block and when unblocked do the same.
rep_receive(SrcLoc, Fun) ->
    case ?LID_FROM_PID(self()) of
        not_found ->
            concuerror_log:internal(
                "Uninstrumented process enters instrumented receive");
        _Lid ->
            {messages, Mailbox} = process_info(self(), messages),
            case rep_receive_match(Fun, Mailbox) of
                block ->
                    concuerror_sched:block(),
                    rep_receive_loop(Fun);
                continue -> ok
            end
    end.

rep_receive_loop(Fun) ->
    {messages, Mailbox} = process_info(self(), messages),
    case rep_receive_match(Fun, Mailbox) of
        block ->
            concuerror_sched:no_wakeup(),
            rep_receive_loop(Fun);
        continue ->
            concuerror_sched:wakeup()
    end.

%% Blocks forever (used for 'receive after infinity -> ...' expressions).
rep_receive_block(SrcLoc) ->
    concuerror_sched:block(),
    rep_receive_block_loop().

rep_receive_block_loop() ->
    concuerror_sched:no_wakeup(),
    rep_receive_block_loop().

rep_receive_match(_Fun, []) ->
    block;
rep_receive_match(Fun, [H|T]) ->
    case Fun(H) of
        block -> rep_receive_match(Fun, T);
        continue -> continue
    end.

%% Called first thing after an `after' clause has been entered.
rep_after_notify(SrcLoc) ->
    concuerror_sched:notify('after', empty),
    ok.

%% Called first thing after a message has been received, to inform the scheduler
%% about the message received and the sender.
rep_receive_notify(SrcLoc, From, Msg) ->
    concuerror_sched:notify('receive', {From, Msg}),
    ok.

%% Similar to rep_receive/2, but used to handle 'EXIT' and 'DOWN' messages.
rep_receive_notify(SrcLoc, Msg) ->
    concuerror_sched:notify('receive_no_instr', Msg),
    ok.

%%%----------------------------------------------------------------------
%%% Helper functions
%%%----------------------------------------------------------------------

find_pid(Pid) when is_pid(Pid) ->
    Pid;
find_pid(Atom) when is_atom(Atom) ->
    whereis(Atom).
