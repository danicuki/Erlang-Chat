%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(chat_group).
-import(lib_chan_mm, [send/2, controller/2]).
-import(lists, [foreach/2, reverse/2]).

-export([start_group/6]).

start_group(ServerMM, Parent, Nick, Group, Host, Port) ->
    process_flag(trap_exit, true),
    controller(ServerMM, self()),
    register(list_to_atom(Group), self()),
    io:format("Criando grupo ~p~n", [Group]),
    lib_chan:start_server(Nick++".conf"),
  	io:format("Grupo ~p criado em ~p:~p~n", [Group, Host, Port]),
    send(ServerMM, {ack, Group, Host, Port}),
    send(ServerMM, {sys, update_groups}),
  	group_controller(ServerMM, Parent, Group, []).



delete(Pid, [{Pid,Nick}|T], L) -> {Nick, reverse(T, L)};
delete(Pid, [H|T], L)          -> delete(Pid, T, [H|L]);
delete(_, [], L)               -> {"????", L}.

find_nick_pid(To, [{Pid, To}|T]) -> Pid;
find_nick_pid(To, [H|T]) -> find_nick_pid(To, T);
find_nick_pid(To, []) -> self().

group_controller(ServerMM, Parent, Group, L) ->
    receive
	{chan, C, {give_me_the_members, G}} ->
      send(ServerMM, {give_me_the_members, G, C}),
	    group_controller(ServerMM, Parent, Group, L);
	{chan, C, {relay, Nick, Str}} ->
	    foreach(fun({Pid,_}) -> send(Pid, {msg,Nick,C,Str}) end, L),
	    group_controller(ServerMM, Parent, Group, L);
  {chan,C, {priv_message, From, To, Str}} ->
      Dest = find_nick_pid(To, L),
      send(Dest, {msg,From,C,"<private> "++Str}),
      group_controller(ServerMM, Parent, Group, L);
	{chan, C, update_users} ->
	    Nicks = lists:map(fun({_,Nick}) -> Nick end, L),
      foreach(fun({Pid,_}) -> send(Pid, {sys,update_users,Nicks}) end, L),
      group_controller(ServerMM, Parent, Group, L);
  {chan, _, {sys, update_groups, Groups}} ->
      foreach(fun({Pid,_}) -> send(Pid, {sys,update_groups,Groups}) end, L),
      group_controller(ServerMM, Parent, Group, L);
  {chan, C, {send_members_to_client, GroupChannel, ClientChannel}} ->
      Members = lists:map(fun({_,Nick}) -> Nick end, L),
	    send(C, {sys, update_members, Members, GroupChannel, ClientChannel}),
	    group_controller(ServerMM, Parent, Group, L);
	{chan, C, {sys,update_members, Members, ClientChannel}} ->
	    send(ClientChannel, {sys, update_members, Members}),
	    group_controller(ServerMM, Parent, Group, L);
	{mm, C, {login, Nick}} ->
	    controller(C, self()),
	    send(C, ack),
	    self() ! {chan, C, {relay, Nick, "I'm joining the groups!"}},
	    self() ! {chan, C, update_users},
      send(ServerMM, {sys, update_groups}),
	    group_controller(ServerMM, Parent, Group, [{C,Nick}|L]);
	{chan_closed, C} ->
	    {Nick, L1} = delete(C, L, []),
	    self() ! {chan, C, {relay, Nick, "I'm leaving the groups"}},
	    self() ! {chan, C, update_users},
	    group_controller(ServerMM, Parent, Group, L1);
  {'EXIT', Parent, _Reason} ->
  	foreach(fun({Pid,_}) -> send(Pid, group_died) end, L), % envia morte do grupo para todos os membros
  	send(ServerMM, {groupDied,Group});
	Any ->
	    io:format("group controller received Msg=~p~n", [Any]),
	    group_controller(ServerMM, Parent, Group, L)
    end.

