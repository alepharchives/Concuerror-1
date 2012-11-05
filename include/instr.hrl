%%%----------------------------------------------------------------------
%%% Copyright (c) 2011, Alkis Gotovos <el3ctrologos@hotmail.com>,
%%%                     Maria Christakis <mchrista@softlab.ntua.gr>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>.
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%%----------------------------------------------------------------------
%%% Authors     : Ilias Tsitsimpis <iliastsi@hotmail.com>
%%% Description : Instrumentation header file
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Definitions
%%%----------------------------------------------------------------------

%% Callback function mapping.
%% The callback functions should be in `concuerror_rep' module.
%% Each callback function should take as arguments:
%%   1: A tupple representing the module, function and arity
%%   2: A tupple representing the file and the source line
%%   3: A list with the arguments
%% For most of the cases rep_generic should be sufficient
-define(INSTR_MOD_FUN,
    %% Auto-imported functions of 'erlang' module.
    [{{erlang, demonitor, 1},           rep_generic},
     {{demonitor, 1},                   rep_generic},
     {{erlang, demonitor, 2},           rep_generic},
     {{demonitor, 2},                   rep_generic},
     {{erlang, halt, 0},                rep_halt},
     {{halt, 0},                        rep_halt},
     {{erlang, halt, 1},                rep_halt},
     {{halt, 1},                        rep_halt},
     {{erlang, is_process_alive, 1},    rep_generic},
     {{is_process_alive, 1},            rep_generic},
     {{erlang, link, 1},                rep_generic},
     {{link, 1},                        rep_generic},
     {{erlang, monitor, 2},             rep_generic},
     {{monitor, 2},                     rep_generic},
     {{erlang, process_flag, 2},        rep_process_flag},
     {{process_flag, 2},                rep_process_flag},
     {{erlang, register, 2},            rep_generic},
     {{register, 2},                    rep_generic},
     {{erlang, spawn, 1},               rep_spawn},
     {{spawn, 1},                       rep_spawn},
     {{erlang, spawn, 3},               rep_spawn},
     {{spawn, 3},                       rep_spawn},
     {{erlang, spawn_link, 1},          rep_spawn},
     {{spawn_link, 1},                  rep_spawn},
     {{erlang, spawn_link, 3},          rep_spawn},
     {{spawn_link, 3},                  rep_spawn},
     {{erlang, spawn_monitor, 1},       rep_spawn},
     {{spawn_monitor, 1},               rep_spawn},
     {{erlang, spawn_monitor, 3},       rep_spawn},
     {{spawn_monitor, 3},               rep_spawn},
     {{erlang, spawn_opt, 2},           rep_spawn},
     {{spawn_opt, 2},                   rep_spawn},
     {{erlang, spawn_opt, 4},           rep_spawn},
     {{spawn_opt, 4},                   rep_spawn},
     {{erlang, unlink, 1},              rep_generic},
     {{unlink, 1},                      rep_generic},
     {{erlang, unregister, 1},          rep_generic},
     {{unregister, 1},                  rep_generic},
     {{erlang, whereis, 1},             rep_generic},
     {{whereis, 1},                     rep_generic},
     {{erlang, send, 2},                rep_send},
     {{erlang, send, 3},                rep_send},
    %% Functions from ets module.
     {{ets, new, 2},                    rep_generic},
     {{ets, insert_new, 2},             rep_generic},
     {{ets, lookup, 2},                 rep_generic},
     {{ets, select_delete, 2},          rep_generic},
     {{ets, insert, 2},                 rep_generic},
     {{ets, delete, 1},                 rep_generic},
     {{ets, delete, 2},                 rep_generic},
     {{ets, match_object, 1},           rep_generic},
     {{ets, match_object, 2},           rep_generic},
     {{ets, match_object, 3},           rep_generic},
     {{ets, foldl, 3},                  rep_generic}
    ]).
