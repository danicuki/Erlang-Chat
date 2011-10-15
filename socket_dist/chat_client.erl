%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(chat_client).

-import(io_widget,
	[get_state/1, insert_str/2, set_prompt/2, set_state/2,
	 set_title/2, set_handler/2, update_state/3, update_groups/2, update_users/2, update_members/2]).

-export([start/0, test/0, connect/5, start/1, start/2]).

start(G, U) ->
  connect("localhost", 2223, "AsDT67aQ", G, U).

start(G) ->
  connect("localhost", 2223, "AsDT67aQ", G, "joe").

start() ->
    connect("localhost", 2223, "AsDT67aQ", "general", "daniel").


test() ->
    connect("localhost", 2223, "AsDT67aQ", "general", "daniel").
    % connect("localhost", 2223, "AsDT67aQ", "general", "daniella"),
    % connect("localhost", 2223, "AsDT67aQ", "general", "reverbel"),
    % connect("localhost", 2223, "AsDT67aQ", "general", "steve").


connect(Host, Port, HostPsw, Group, Nick) ->
    spawn(fun() -> handler(Host, Port, HostPsw, Group, Nick) end).

handler(Host, Port, HostPsw, Group, Nick) ->
    process_flag(trap_exit, true),
    Widget = io_widget:start(self()),
    set_title(Widget, Nick),
    set_state(Widget, Nick),
    set_prompt(Widget, [Nick, " > "]),
    set_handler(Widget, fun parse_command/1),
    start_connector(Host, Port, HostPsw),
    disconnected(Widget, Group, Nick).



disconnected(Widget, Group, Nick) ->
    receive
	{connected, MM} ->
	    insert_str(Widget, "connected to server\nsending data\n"),
	    lib_chan_mm:send(MM, {login, Group, Nick}),
	    wait_login_response(Widget, MM);
	{Widget, destroyed} ->
	    exit(died);
	{status, S} ->
	    insert_str(Widget, to_str(S)),
	    disconnected(Widget, Group, Nick);
	Other ->
	    io:format("chat_client disconnected unexpected:~p~n",[Other]),
	    disconnected(Widget, Group, Nick)
    end.



wait_login_response(Widget, MM) ->
    receive
    {chan, MM, {create_group, Group, Nick, Groups}} ->
    	  io:format("Group novo. Usuario ~p vai criar o grupo ~p~n", [Nick, Group]),
    	  {Host, Port} = get_host_and_port(Nick),
    		Parent = self(),
    	  spawn_link(fun() -> chat_group:start_group(MM, Parent, Nick, Group, Groups, Host, Port) end),
    	  GroupMM = connect_in_group(Group, Nick, Host, Port),
    	  insert_str(Widget, [Nick,"@", pid_to_list(self()),"I'm starting the group\n"]),
        % Espera se conectar no grupo.
        wait_login_response(Widget, GroupMM);
    {chan, MM, {connect_to_group, Group, Nick, Host, Port}} ->
    	  GroupMM = connect_in_group(Group, Nick, Host, Port),
    		% Fecha a conexao que foi aberta com o servidor
    		MM ! close,
        % Espera se conectar no grupo.
        wait_login_response(Widget, GroupMM);
	  {chan, MM, ack} ->
	    active(Widget, MM);
	Other ->
	    io:format("chat_client login unexpected:~p~n",[Other]),
	    wait_login_response(Widget, MM)
    end.

get_host_and_port(Nick) ->
    {ok, [{port, Port}, _]} = file:consult(Nick++".conf"),
    {localhost, Port}.


connect_in_group(Group, Nick, Host, Port) ->
    io:format("Usuario ~p conectando no grupo ~p (~p:~p) ~n", [Nick, Group, Host, Port]),
	  GroupMM = try_to_connect_in_group(Host, Port),
    lib_chan_mm:send(GroupMM, {login, Nick}),
	  GroupMM.

active(Widget, MM) ->
     receive
	 {Widget, Nick, Str} ->
	     lib_chan_mm:send(MM, {relay, Nick, Str}),
	     active(Widget, MM);
	 {Widget, {give_me_the_members, Group}} ->
	     lib_chan_mm:send(MM, {give_me_the_members, Group}),
	     active(Widget, MM);
	 {chan, MM, {sys, update_users, Users}} ->
	     update_users(Widget, Users),
	     active(Widget, MM);
	 {chan, MM, {sys, update_groups, Groups}} ->
       update_groups(Widget, Groups),
       active(Widget, MM);
   {chan, MM, {sys, update_members, Members}} ->
       update_members(Widget, Members),
       active(Widget, MM);
	 {chan, MM, {msg, From, Pid, Str}} ->
	     insert_str(Widget, [From,"@",pid_to_list(Pid)," ", Str, "\n"]),
	     active(Widget, MM);
	 {'EXIT',Widget,windowDestroyed} ->
	     lib_chan_mm:close(MM);
	 {close, MM} ->
	     exit(serverDied);
	 Other ->
	     io:format("chat_client active unexpected:~p~n",[Other]),
	     active(Widget, MM)
     end.



start_connector(Host, Port, Pwd) ->
    S = self(),
    spawn_link(fun() -> try_to_connect(S, Host, Port, Pwd) end).

try_to_connect(Parent, Host, Port, Pwd) ->
    %% Parent is the Pid of the process that spawned this process
    case lib_chan:connect(Host, Port, chat, Pwd, []) of
	{error, _Why} ->
	    Parent ! {status, {cannot, connect, Host, Port}},
	    sleep(2000),
	    try_to_connect(Parent, Host, Port, Pwd);
	{ok, MM} ->
	    lib_chan_mm:controller(MM, Parent),
	    Parent ! {connected, MM},
	    exit(connectorFinished)
    end.

try_to_connect_in_group(Host, Port) ->
	case lib_chan:connect(Host, Port, chat_group, "AsDT67aQ", []) of
		{error, _Why} ->
			sleep(2000),
			try_to_connect_in_group(Host, Port);
		{ok, MM} -> MM
	end.


sleep(T) ->
    receive
    after T -> true
    end.

to_str(Term) ->
    io_lib:format("~p~n",[Term]).

parse_command(Str) -> skip_to_gt(Str).

skip_to_gt(">" ++ T) -> T;
skip_to_gt([_|T])    -> skip_to_gt(T);
skip_to_gt([])       -> exit("no >").
