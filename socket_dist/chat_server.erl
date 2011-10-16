%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(chat_server).
-import(lib_chan_mm, [send/2, controller/2]).
-import(lists, [delete/2, foreach/2, map/2, member/2,reverse/2]).

-compile(export_all).


start() ->
    start_server(),
    lib_chan:start_server("chat.conf").

start_server() ->
    register(chat_server,
	     spawn(fun() ->
			   process_flag(trap_exit, true),
			   Val= (catch server_loop([])),
			   io:format("Server terminated with:~p~n",[Val])
		   end)).

groups(L) -> lists:map(fun({Group, _, _, _}) -> Group end, L).

server_loop(L) ->
  receive
  	{mm, Channel, {login, Group, Nick}} ->
  	    case lookup(Group, L) of
  		    {ok, Host, Port, _} ->
  		      send(Channel, {connect_to_group, Group, Nick, Host, Port}),
  		      server_loop(L);
  		    error ->
  		      io:format("Nao tem o grupo ~n", []),
  		      send(Channel, {create_group, Group, Nick, groups(L)}),
            io:format("waiting ~p (~p) to create the group ~p~n", [Nick, Channel, Group]),
      			receive
      			    {mm, Channel, {ack, Group, Host, Port}} ->
           			    io:format("group ~p (~p) has been created on ~p:~p ~n", [Group, Channel, Host, Port]),
      		            server_loop([{Group, Host, Port, Channel}|L])
            after 10000 ->
                  server_loop(L)
      		  end
      	  end,
      	server_loop(L);
    {mm, Channel, {give_me_the_members, Group, Client}} ->
  	    case lookup(Group, L) of
  		    {ok, _, _, GroupChannel} -> send(GroupChannel, {send_members_to_client, Channel, Client});
    		  error -> send(Channel, {sys, update_members, Client, ['Group does not exists']})
    	  end,
        server_loop(L);
    {mm, Channel, {sys,update_members, Members, GroupChannel, ClientChannel}} ->
        send(GroupChannel, {sys, update_members, Members, ClientChannel}),
        server_loop(L);
  	{mm_closed, _} ->
  	    server_loop(L);
  	{mm, Channel, {sys, update_groups}} ->
  	    send(Channel, {sys, update_groups, groups(L)}),
   	    server_loop(L);
  	{mm, _From, {groupDied,Group}} ->
      	L1 = remove_group(Group, L),
        foreach(fun({_, _, _, Channel}) -> self() ! {mm, Channel, {sys, update_groups}}  end, L1),
      	server_loop(L1);
  	Msg ->
  	    io:format("Server received Msg=~p~n",
  		      [Msg]),
  	    server_loop(L)
  end.



lookup(Group, [{Group, Host, Port, Channel}|_]) -> {ok, Host, Port, Channel};
lookup(Group, [_|T])       -> lookup(Group, T);
lookup(_,[])           -> error.

remove_group(Pid, [{G,Pid}|T]) -> io:format("~p removed~n",[G]), T;
remove_group(Pid, [H|T])       -> [H|remove_group(Pid, T)];
remove_group(_, [])            -> [].

