-module(spawned_senders).

-compile(export_all).

spawned_senders() ->
    Receiver = spawn(fun receive_two/0),
    spawn(sender(Receiver, one)),
    spawn(sender(Receiver, two)).

    

receive_two() ->
    receive
        Pat1 ->
            receive
                Pat2 ->
                    [Pat1, Pat2]
            end
    end.

sender(Receiver, Msg) ->
    fun() -> Receiver ! Msg end.
