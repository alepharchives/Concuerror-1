%%%----------------------------------------------------------------------
%%% Copyright (c) 2011, Alkis Gotovos <el3ctrologos@hotmail.com>,
%%%                     Maria Christakis <mchrista@softlab.ntua.gr>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>.
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%%----------------------------------------------------------------------
%%% Author      : Alkis Gotovos <el3ctrologos@hotmail.com>
%%% Description : Process action interface
%%%----------------------------------------------------------------------

-module(concuerror_proc_action).

-export([to_string/1]).

-export_type([proc_action/0]).

%%%----------------------------------------------------------------------
%%% Definitions
%%%----------------------------------------------------------------------

%% Printing depth of terms like messages or exit reasons.
-define(PRINT_DEPTH, 4).
-define(PRINT_DEPTH_EXIT, 10).


%%%----------------------------------------------------------------------
%%% User interface
%%%----------------------------------------------------------------------

-spec to_string() -> string().
to_string({'after', Proc}) ->
    io_lib:format("Process ~s receives no matching messages",
                  [concuerror_lid:to_string(Proc)]);
to_string({block, Proc}) ->
    io_lib:format("Process ~s blocks", [concuerror_lid:to_string(Proc)]);
to_string({{erlang, demonitor _Arity}, Proc, Args, not_found}) ->
    io_lib:format("Process ~s demonitors nonexisting process",
                  [concuerror_lid:to_string(Proc)]);
to_string({{erlang, demonitor, _Arity}, Proc1, Args, Proc2}) ->
    io_lib:format("Process ~s demonitors process ~s",
                  [concuerror_lid:to_string(Proc1),
                   concuerror_lid:to_string(Proc2)]);
to_string({exit, Proc, Reason}) ->
    io_lib:format("Process ~s exits (~P)",
                  [concuerror_lid:to_string(Proc),
                   Reason, ?PRINT_DEPTH_EXIT]);
to_string({fun_exit, Proc, not_found, Reason}) ->
    io_lib:format("Process ~s sends exit signal (~W) to nonexisting process",
                  [concuerror_lid:to_string(Proc),
                   Reason, ?PRINT_DEPTH]);
to_string({fun_exit, Proc, Target, Reason}) ->
    io_lib:format("Process ~s sends exit signal (~p) to process ~s",
                  [concuerror_lid:to_string(Proc),
                   Reason, concuerror_lid:to_string(Target)]);
to_string({halt, Proc}) ->
    io_lib:format("Process ~s halts the system",
                  [concuerror_lid:to_string(Proc)]);
to_string({halt, Proc, Status}) ->
    io_lib:format("Process ~s halts the system with status ~p",
                  [concuerror_lid:to_string(Proc), Status]);
to_string({is_process_alive, Proc, not_found}) ->
    io_lib:format("Process ~s checks if nonexisting process is alive",
                  [concuerror_lid:to_string(Proc)]);
to_string({is_process_alive, Proc1, Proc2}) ->
    io_lib:format("Process ~s checks if process ~s is alive",
                  [concuerror_lid:to_string(Proc1),
                   concuerror_lid:to_string(Proc2)]);
to_string({link, Proc, not_found}) ->
    io_lib:format("Process ~s links to nonexisting process",
                  [concuerror_lid:to_string(Proc)]);
to_string({link, Proc1, Proc2}) ->
    io_lib:format("Process ~s links to process ~s",
                  [concuerror_lid:to_string(Proc1),
                   concuerror_lid:to_string(Proc2)]);
to_string({monitor, Proc, not_found}) ->
    io_lib:format("Process ~s monitors nonexisting process",
                  [concuerror_lid:to_string(Proc)]);
to_string({monitor, Proc1, Proc2}) ->
    io_lib:format("Process ~s monitors process ~s",
                  [concuerror_lid:to_string(Proc1),
                   concuerror_lid:to_string(Proc2)]);
to_string({'process_flag', Proc, Flag, Value}) ->
    io_lib:format("Process ~s sets flag `~p` to `~p`",
                  [concuerror_lid:to_string(Proc), Flag, Value]);
to_string({'receive', Receiver, Sender, Msg}) ->
    io_lib:format("Process ~s receives message `~W` from process ~s",
                  [concuerror_lid:to_string(Receiver), Msg, ?PRINT_DEPTH,
                   concuerror_lid:to_string(Sender)]);
to_string({'receive_no_instr', Receiver, Msg}) ->
    io_lib:format("Process ~s receives message `~W` from unknown process",
                  [concuerror_lid:to_string(Receiver), Msg, ?PRINT_DEPTH]);
to_string({register, Proc, RegName, RegLid}) ->
    io_lib:format("Process ~s registers process ~s as `~p`",
                  [concuerror_lid:to_string(Proc),
                   concuerror_lid:to_string(RegLid), RegName]);
to_string({send, Sender, not_found, Msg}) ->
    io_lib:format("Process ~s sends message `~W` to nonexisting process",
                  [concuerror_lid:to_string(Sender), Msg, ?PRINT_DEPTH]);
to_string({send, Sender, Receiver, Msg}) ->
    io_lib:format("Process ~s sends message `~W` to process ~s",
                  [concuerror_lid:to_string(Sender), Msg, ?PRINT_DEPTH,
                   concuerror_lid:to_string(Receiver)]);
to_string({spawn, not_found, Child}) ->
    io_lib:format("Unknown process spawns process ~s",
                  [concuerror_lid:to_string(Child)]);
to_string({spawn, Parent, Child}) ->
    io_lib:format("Process ~s spawns process ~s",
                  [concuerror_lid:to_string(Parent),
                   concuerror_lid:to_string(Child)]);
to_string({spawn_link, not_found, Child}) ->
    io_lib:format("Unknown process spawns and links to process ~s",
                  [concuerror_lid:to_string(Child)]);
to_string({spawn_link, Parent, Child}) ->
    io_lib:format("Process ~s spawns and links to process ~s",
                  [concuerror_lid:to_string(Parent),
                   concuerror_lid:to_string(Child)]);
to_string({spawn_monitor, not_found, Child}) ->
    io_lib:format("Unknown process spawns and monitors process ~s",
                  [concuerror_lid:to_string(Child)]);
to_string({spawn_monitor, Parent, Child}) ->
    io_lib:format("Process ~s spawns and monitors process ~s",
                  [concuerror_lid:to_string(Parent),
                   concuerror_lid:to_string(Child)]);
to_string({spawn_opt, not_found, Child, [link]}) ->
    io_lib:format("Unknown process spawns and links to process ~s",
                  [concuerror_lid:to_string(Child)]);
to_string({spawn_opt, Parent, Child, [link]}) ->
    io_lib:format("Process ~s spawns and links to process ~s",
                  [concuerror_lid:to_string(Parent),
                   concuerror_lid:to_string(Child)]);
to_string({spawn_opt, not_found, Child, [monitor]}) ->
    io_lib:format("Unknown process spawns and monitors process ~s",
                  [concuerror_lid:to_string(Child)]);
to_string({spawn_opt, Parent, Child, [monitor]}) ->
    io_lib:format("Process ~s spawns and monitors process ~s",
                  [concuerror_lid:to_string(Parent),
                   concuerror_lid:to_string(Child)]);
to_string({spawn_opt, not_found, Child, _Opts}) ->
    io_lib:format("Unknown process spawns, monitors and links to process ~s",
                  [concuerror_lid:to_string(Child)]);
to_string({spawn_opt, Parent, Child, _Opts}) ->
    io_lib:format("Process ~s spawns, monitors and links to process ~s",
                  [concuerror_lid:to_string(Parent),
                   concuerror_lid:to_string(Child)]);
to_string({unlink, Proc, not_found}) ->
    io_lib:format("Process ~s unlinks from nonexisting process",
                  [concuerror_lid:to_string(Proc)]);
to_string({unlink, Proc1, Proc2}) ->
    io_lib:format("Process ~s unlinks from process ~s",
                  [concuerror_lid:to_string(Proc1),
                   concuerror_lid:to_string(Proc2)]);
to_string({unregister, Proc, RegName}) ->
    io_lib:format("Process ~s unregisters process `~p`",
                  [concuerror_lid:to_string(Proc), RegName]);
to_string({whereis, Proc, RegName, not_found}) ->
    io_lib:format("Process ~s requests the pid of unregistered "
                  "process `~p` (undefined)",
                  [concuerror_lid:to_string(Proc), RegName]);
to_string({whereis, Proc, RegName, RegLid}) ->
    io_lib:format("Process ~s requests the pid of process `~p` (~s)",
                  [concuerror_lid:to_string(Proc), RegName,
                   concuerror_lid:to_string(RegLid)]);
%to_string({CallMsg, Proc, _Args}) ->
%    io_lib:format("Process ~s: ~p", [concuerror_lid:to_string(Proc), CallMsg]);
to_string(What) ->
    io_lib:format("~p", [What]).
