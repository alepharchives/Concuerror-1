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
%%% Description : Error ticket interface
%%%----------------------------------------------------------------------

-module(ticket).

-export([new/3, get_target/1, get_error/1, get_state/1, sort/1]).

-export_type([ticket/0]).

-include("gen.hrl").

%% An error ticket containing all necessary information needed to replay the
%% interleaving that caused it.
-type ticket() :: {sched:analysis_target(), error:error(), state:state()}.

%% @doc: Create a new error ticket.
-spec new(sched:analysis_target(), error:error(), state:state()) ->
                 ticket().

new(Target, Error, ErrorState) ->
    {Target, Error, ErrorState}.

-spec get_target(ticket()) -> sched:analysis_target().

get_target({Target, _Error, _ErrorState}) ->
    Target.

-spec get_error(ticket()) -> error:error().

get_error({_Target, Error, _ErrorState}) ->
    Error.

-spec get_state(ticket()) -> state:state().

get_state({_Target, _Error, ErrorState}) ->
    ErrorState.

%% Sort a list of tickets according to state.
-spec sort([ticket()]) -> [ticket()].

sort(Tickets) ->
    Compare = fun(T1, T2) -> get_state(T1) =< get_state(T2) end,
    lists:sort(Compare, Tickets).
