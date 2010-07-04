-module(gui).

-export([start/0, init/0]).

-define(WIDTH, 800).
-define(HEIGHT, 600).

-define(REGNAME, cleric_listener).


start() ->
    spawn(fun () -> init() end).

init() ->
    Layout =
	[{window, the_window, [{title, io_lib:format("CLERIC tester - ~p",
						     [node()])},
			       {configure, true},
			       {width, ?WIDTH},
			       {height, ?HEIGHT}],
	  [
	   {menubar, the_menubar, [],
	    [
	     {menubutton, [{label, {text, "File"}}],
	      [{menu, [],
		[{menuitem, connect_menuitem,
		  [{label, {text, "Connect to node"}}]},
		 {menuitem, exit_menuitem, [{label, {text, "Exit"}}]}
		]}
	      ]},
	     {menubutton, [{label, {text, "Help"}}],
	      [{menu, [],
		[{menuitem, about_menuitem, [{label, {text, "About..."}}]}
		]}
	      ]}
	    ]},

	   {frame, the_frame,
	    [{bw, 2},
	     {bg, white},
	     {packer_x, [{stretch, 1, 120},
			 {stretch, 2, 120},
			 {stretch, 2, 120}]},
	     {packer_y, [{fixed, 25},
			 {stretch, 3, 300},
			 {stretch, 2, 200}]}],
	    [
	     {label, [{label, {text, "Connected nodes"}}, {pack_xy, {1, 1}}]},
	     {label, [{label, {text, "Sent messages"}}, {pack_xy, {2, 1}}]},
	     {label, [{label, {text, "Received messages"}}, {pack_xy, {3, 1}}]},

	     {listbox, node_list, [{selectmode, single},
				   {hscroll, false},
				   {vscroll, false},
				   {bg, white},
				   {pack_x, 1},
				   {pack_y, {2, 3}}]},
	     {editor, sent_messages, [{enable, false},
				      {vscroll, right},
				      {pack_xy, {2, 2}}]},
	     {editor, received_messages, [{enable, false},
					  {vscroll, right},
					  {pack_x, 3},
					  {pack_y, {2, 3}}]},

	     {frame, [{pack_xy, {2, 3}},
	  	      {packer_x, [{stretch, 1}]},
	  	      {packer_y, [{fixed, 25}, {stretch, 1}, {fixed, 25}]}],
	      [
	       {label, [{label, {text, "Send message"}}, {pack_xy, {1, 1}}]},
	       {editor, message_editor, [{bg, white},
					 {vscroll, right},
					 {setfocus, true},
					 {pack_xy, {1, 2}}]},
	       {button, send_message_button, [{label, {text, "Send"}},
					      {pack_xy, {1, 3}}]}
	      ]}
	    ]}
	  ]}],
    gs:create_tree(gs:start(), Layout),
    receive
	{gs, the_window, configure, _Data, [1, 1, _X, _Y]} -> ok;
	Other -> Other ! self()
    after 1000 -> ok
    end,
    MenuHeight = gs:read(the_menubar, height),
    gs:config(the_frame, [{y, MenuHeight},
			  {width, ?WIDTH},
			  {height, ?HEIGHT - MenuHeight}]),
    update_node_list(node_list),
    gs:config(message_editor,
	      {insert, {insert, "{greeting, \"Hello world!\"}"}}),
    gs:config(the_window, {map, true}),
    Listener = listener:start(self()),
    register(?REGNAME, Listener),
    link(Listener),
    loop(Listener).


loop(Listener) ->
    receive
	{Listener, received, Msg} ->
	    Timestamp = io_lib:format("~2..0B:~2..0B:~2..0B",
				      tuple_to_list(time())),
	    gs:config(received_messages, {enable, true}),
	    gs:config(received_messages,
		      {insert, {{0, 0}, [Timestamp, " Received message\n",
					 io_lib:format("~p\n\n", [Msg])]}}),
	    gs:config(received_messages, {fg, {{{0, 0}, {0, lineend}}, green}}),
	    gs:config(received_messages, {enable, false});
	{gs, the_window, configure, _Data, [W, H, _X, _Y]} ->
	    gs:config(the_frame, [{width, W},
				  {height, H - gs:read(the_menubar, height)}]);
	{gs, send_message_button, click, _Data, _Args} ->
	    Message = gs:read(message_editor, {get, {{0, 0}, 'end'}}),
	    Timestamp = io_lib:format("~2..0B:~2..0B:~2..0B",
				      tuple_to_list(time())),
	    gs:config(sent_messages, {enable, true}),
	    case gs:read(node_list, selection) of
		[] ->
		    gs:config(sent_messages,
			      {insert,
			       {{0, 0},	[Timestamp, " No node selected\n"]}}),
		    gs:config(sent_messages,
			      {fg, {{{0, 0}, {0, lineend}}, red}});
		[Pos] ->
		    Node = gs:read(node_list, {get, Pos}),
		    case parse_written_message(Message) of
			{ok, Term} ->
			    Listener ! {self(), send, Term,
					{?REGNAME, list_to_atom(Node)}},
			    gs:config(sent_messages,
				      {insert,
				       {{0, 0},
					[Timestamp, " Sent message to ", Node,
					 io_lib:format("\n~p\n\n", [Term])]}}),
			    gs:config(sent_messages,
				      {fg, {{{0, 0}, {0, lineend}}, blue}});
			{error, _Fun, _Error} ->
			    gs:config(sent_messages,
				      {insert,
				       {{0, 0},	[Timestamp,
						 " Invalid Erlang term\n"]}}),
			    gs:config(sent_messages,
				      {fg, {{{0, 0}, {0, lineend}}, red}})
		    end
	    end,
	    gs:config(sent_messages, {enable, false}),
	    ok;
	{gs, connect_menuitem, click, _Data, _Args} ->
	    connect_window(the_window);
	{gs, connect_button, click, _Data, _Args} ->
	    net_adm:ping(list_to_atom(gs:read(connect_entry, text))),
	    update_node_list(node_list),
	    gs:destroy(connect_window);
	{gs, connect_cancel_button, click, _Data, _Args} ->
	    gs:destroy(connect_window);
	{gs, connect_entry, keypress, _Data, ['Return' | _]} ->
	    net_adm:ping(list_to_atom(gs:read(connect_entry, text))),
	    update_node_list(node_list),
	    gs:destroy(connect_window);
	{gs, connect_entry, keypress, _Data, _Args} ->
	    ok;
	{gs, about_menuitem, click, _Data, _Args} ->
	    about_window(the_window);
	{gs, about_close_button, click, _Data, _Args} ->
	    gs:destroy(about_window);
	{gs, about_window, destroy, _Data, _Args} ->
	    ok;
	{gs, exit_menuitem, click, _Data, _Args} ->
	    gs:stop(),
	    exit(normal);
	{gs, the_window, destroy, _Data, _Args} ->
	    gs:stop(),
	    exit(normal);
	{gs, _ID, _EventType, _Data, _Args} = Msg ->
	    io:format("Got unrecognized gs message: ~p\n", [Msg]);
	_Other ->
	    io:format("Got unrecognized message: ~p\n", [_Other])
    after 2000 ->
	    update_node_list(node_list)
    end,
    loop(Listener).


parse_written_message(String) ->
    case erl_scan:string(String ++ ".") of
	{error, ScanError, _} ->
	    {error, {erl_scan, string, 1}, ScanError};
	{ok, Tokens, _} ->
	    case erl_parse:parse_term(Tokens) of
		{error, ParseError} ->
		    {error, {erl_parse, parse_term, 1}, ParseError};
		{ok, Term} ->
		    {ok, Term}
	    end
    end.


update_node_list(NodeList) ->
    NewItems = nodes(known),
    Selection = gs:read(NodeList, selection),
    gs:config(NodeList, {items, NewItems}),
    case Selection of
	[]         -> ok;
	[Selected] -> gs:config(NodeList, {selection, Selected});
	_          -> ignore
    end.


about_window(Parent) ->
    Layout =
	[{window, about_window, [{title, "About CLERIC tester"},
				 {width, 450},
				 {height, 150},
				 {map, true}],
	 [
	  {frame, about_frame, [{bw, 2},
				{packer_x, [{stretch, 2}, {stretch, 1}]},
				{packer_y, [{stretch, 1}, {fixed, 25}]}],
	   [
	    {label, [{pack_x, {1, 2}},
		     {pack_y, 1},
		     {label, {text, ["A program for testing CLERIC ",
				     "(Common Lisp Erlang Interface).\n",
				     "\n",
				     "Created by Markus Flambard ",
				     "<markus@flambard.se>"]}}]},
	    {button, about_close_button, [{label, {text, "Close"}},
					  {pack_xy, {2, 2}}]}
	   ]}
	 ]}],
    gs:create_tree(Parent, Layout),
    gs:config(about_frame, [{x, 10}, {y, 10}, {width, 430}, {height, 130}]).

connect_window(Parent) ->
    Layout =
	[{window, connect_window, [{title, "Connect to node"},
				   {width, 300},
				   {height, 50},
				   {map, true}],
	 [
	  {frame, connect_frame, [{bw, 0},
				  {packer_x, [{stretch, 1},
					      {stretch, 1},
					      {stretch, 1}]},
				  {packer_y, [{fixed, 25}, {fixed, 25}]}],
	   [
	    {entry, connect_entry, [{bg, white},
				    {keypress, true},
				    {setfocus, true},
				    {pack_x, {1, 3}},
				    {pack_y, 1}]},
	    {button, connect_button, [{label, {text, "Connect"}},
				      {pack_xy, {2, 2}}]},
	    {button, connect_cancel_button, [{label, {text, "Cancel"}},
					     {pack_xy, {3, 2}}]}
	   ]}
	 ]}],
    gs:create_tree(Parent, Layout),
    gs:config(connect_frame, [{width, 300}, {height, 50}]).
