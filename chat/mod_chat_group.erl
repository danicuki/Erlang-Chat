-module(mod_chat_group).
-export([start/3]).
-import(lib_chan_mm, [send/2]).

start(MM, _, [Group]) ->
    process_flag(trap_exit, true),
    io:format("mod_chat_group off we go ...~p(~p)~n", [Group, MM]),
    loop(Group, MM).

loop(Group, MM) ->
     receive
	 {chan, MM, Msg} ->
	     Group ! {mm, MM, Msg},
	     loop(Group, MM);
	 {chan_closed, MM} ->
		 Group ! {mm_closed, MM},
		 loop(Group, MM);
	 {'EXIT', MM, _Why} ->
		 exit(normal);
	 Other ->
	     io:format("mod_chat_group unexpected message =~p (MM=~p)~n",
		       [Other, MM]),
	     loop(Group, MM)
    end.

