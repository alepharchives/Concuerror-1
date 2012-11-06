-module(ets_dependencies_3).

-export([ets_dependencies_3/0]).

ets_dependencies_3() ->
    Parent = self(),
    ets:new(table, [public, named_table]),
    spawn(fun() ->
                  ets:lookup(table, x),
                  Parent ! ok
          end),
    spawn(fun() ->
                  ets:insert(table, {y, 2}),
                  ets:insert(table, {x, 1}),
                  Parent ! ok
          end),
    spawn(fun() ->
                  ets:insert(table, {z, 3}),
                  ets:lookup(table, x),
                  Parent ! ok
          end),
    receive
        ok ->
            receive
                ok ->
                    receive
                        ok ->
                            receive
                                dead -> ok
                            end
                    end
            end
    end.
