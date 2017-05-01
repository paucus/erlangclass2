%%% assignment 1.50 palindrome checker process

-module(b180).
-export([start/0,receiver/0,seqreceiver/0]).
%-include_lib("eunit/include/eunit.hrl").
-created_by("Marcelo Ruiz Camauër").


start()->
    Pid = spawn(?MODULE,receiver,[]),
    Pid ! {self(),{check,"first"}},
    Pid ! {self(),{check,"second"}},
    Pid ! {self(),{check,"third"}},

    Pid ! stop.

receiver() ->                                 
    receive
        {From,{check,T}} ->
            io:format("Received '~p' from ~w~n",[T,From]),  
            receiver();                       
        stop -> true;
        _ -> true
    end.


seqreceiver() ->
    receive
        {first, FirstString} ->
            io:format("Received: ~w~n", [{first, FirstString}]),
            seqreceiver();
        {second, SecondString} ->
            receive
                {first, FirstString} ->
                    io:format("Received: ~w~n", [{first, FirstString}])
            end,
            io:format("Received: ~w~n", [{second, SecondString}]),
            seqreceiver()
    end.
