-module(send_receive_dependencies).

-export([send_receive_dependencies/0]).

send_receive_dependencies() ->
    P = self(),
    P101 = spawn(fun() ->
                       receive
                           p ->
                               receive
                                   P1 ->
                                       receive
                                           P2 -> [P1, P2]
                                       end
                               end
                       end
               end),
    P102 = spawn(fun() ->
                       receive
                           L -> L ! 1
                       end
               end),
    P103 = spawn(fun() ->
                       receive
                           p -> P101 ! 2
                       end,
                       receive
                           1 -> P101 ! 1
                       end
               end),
    P103 ! p,
    P102 ! P103,
    P101 ! p.
