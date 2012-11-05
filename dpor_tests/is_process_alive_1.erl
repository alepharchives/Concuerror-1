-module(is_process_alive_1).

-compile(export_all).

is_process_alive_1() ->
    Pid = spawn(fun() -> ok end),
    case is_process_alive(Pid) of
        true -> register(child, Pid);
        false -> ok
    end.
                
