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

-export([start/3]).

start(C, Nick, Groups) ->
    process_flag(trap_exit, true),
    controller(C, self()),
    send(C, ack),
    self() ! {chan, C, {relay, Nick, "I'm starting the group"}},
    self() ! {chan, C, update_users},
    send(C, {sys,update_groups,Groups}),
    group_controller([{C,Nick}]).



delete(Pid, [{Pid,Nick}|T], L) -> {Nick, reverse(T, L)};
delete(Pid, [H|T], L)          -> delete(Pid, T, [H|L]);
delete(_, [], L)               -> {"????", L}.



group_controller([]) ->
    exit(allGone);
group_controller(L) ->
    receive
	{chan, C, {give_me_the_members, Group}} ->
      chat_server ! {give_me_the_members, Group, C},
	    group_controller(L);
	{chan, C, {relay, Nick, Str}} ->
	    foreach(fun({Pid,_}) -> send(Pid, {msg,Nick,C,Str}) end, L),
	    group_controller(L);
	{chan, C, update_users} ->
	    Nicks = lists:map(fun({Pid,Nick}) -> Nick end, L),
      foreach(fun({Pid,_}) -> send(Pid, {sys,update_users,Nicks}) end, L),
      group_controller(L);
  {send, {sys, update_groups, Groups}} ->
      foreach(fun({Pid,_}) -> send(Pid, {sys,update_groups,Groups}) end, L),
      group_controller(L);
  {send_members_to_client, C} ->
      Members = lists:map(fun({Pid,Nick}) -> Nick end, L),
	    send(C, {sys, update_members, Members}),
	    group_controller(L);
	{login, C, Nick, Groups} ->
	    controller(C, self()),
	    send(C, ack),
	    self() ! {chan, C, {relay, Nick, "I'm joining the groupos!"}},
	    self() ! {chan, C, update_users},
	    send(C, {sys,update_groups,Groups}),
	    group_controller([{C,Nick}|L]);
	{chan_closed, C} ->
	    {Nick, L1} = delete(C, L, []),
	    self() ! {chan, C, {relay, Nick, "I'm leaving the groupos"}},
	    self() ! {chan, C, update_users},
	    group_controller(L1);
	Any ->
	    io:format("group controller received Msg=~p~n", [Any]),
	    group_controller(L)
    end.

