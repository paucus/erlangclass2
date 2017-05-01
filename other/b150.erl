%%% assignment 1.50 palindrome checker process

-module(b150).
-export([start/0,server/0,palindrome/1]).
-include_lib("eunit/include/eunit.hrl").
-created_by("Marcelo Ruiz Camauër").

% Define a function server/1 that accepts messages of the form
% 
% {check,"Madam I\'m Adam"}
% and returns results like
% 
% {result,"\"Madam I\'m Adam\" is a palindrome"}
% If it is sent any other format of message, such as stop,
% the server should stop, by terminating its operation. 
% The argument to server/1 should be the Pid of the process to which results are to be returned.

start()->
    Text = "T. Eliot, top bard, notes putrid tang emanating, is sad; I'd assign it a name: gnat dirt upset on drab pot toilet.",
    Pid = spawn(b150,server,[]),
    Pid ! {self(),{check,Text}},
    receive
        {Pid, Result} -> io:format("Result of evaluating '~p' is ~w~n",[Text,Result])
    end,
    Pid ! stop.

server() ->                                 % process palindromes
    receive
        {From,{check,T}} ->
            From ! {self(),palindrome(T)},  % return results to our caller
            server();                       % stay in loop waiting for next invocation
        stop -> true;
        _ -> true
    end.





% common code, serial:
palindrome(Xs) ->
    palin(nocaps(nopunct(Xs))).

nopunct([]) ->
    [];
nopunct([X|Xs]) ->
    case lists:member(X,".,\ ;:!?¿¡\t\n\'\"") of
	true ->
	    nopunct(Xs);
	false ->
	    [ X | nopunct(Xs) ]
    end.

nocaps([]) ->
    [];
nocaps([X|Xs]) ->
    [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
    case $A =< X andalso X =< $Z of
	true ->
	    X+32;
	false ->
	    X
    end.

% literal palindrome

palin(Xs) ->
    Xs == reverse(Xs).

reverse(Xs) ->
    shunt(Xs,[]).

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).

 
	




%%%%%%%%%%%%%%%
% invoke with 'b150:test().' or "eunit:test(b150)."
palindrome_test_()->
    [   ?_assert(palindrome("A man, a plan, a canal: Panama!")==true),
        ?_assert(palindrome("Able was I ere I saw Elba")==true),
        ?_assert(palindrome("Madam, I'm Adam")==true),
        ?_assert(palindrome("Never odd or even")==true),
        ?_assert(palindrome("Doc, note: I dissent. A fast never prevents a fatness. I diet on cod")==true),
        ?_assert(palindrome("T. Eliot, top bard, notes putrid tang emanating, is sad; I'd assign it a name: gnat dirt upset on drab pot toilet.")==true),
        ?_assert(palindrome([])==true),
        ?_assert(palindrome("A man, no plan, bad canal: Panama")==false)
    ].

