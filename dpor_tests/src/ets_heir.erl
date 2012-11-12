-module(ets_heir).

-export([ets_heir/0]).

ets_heir() ->
    Heir = spawn(fun() -> ok end),
    ets:new(table, [named_table, {heir, Heir, heir}]),
    spawn(fun() ->
                  ets:lookup(table, x),
                  throw(too_bad)
          end).
