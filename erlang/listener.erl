-module(listener).

-export([start/1]).


start(Master) ->
    spawn(fun () -> init(Master) end).

init(Master) ->
    loop(Master).

loop(Master) ->
    receive
	{Master, send, Msg, To} ->
	    To ! Msg;
	Msg ->
	    Master ! {self(), received, Msg}
    end,
    loop(Master).
