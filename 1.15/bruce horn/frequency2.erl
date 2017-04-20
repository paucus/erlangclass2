%% Based on code from 
%% Erlang Programming 
%% Francecso Cesarini and Simon Thompson 
%% O'Reilly, 2008 
%% http://oreilly.com/catalog/9780596518189/ 
%% http://www.erlangprogramming.org/ 
%% (c) Francesco Cesarini and Simon Thompson

-module(frequency2). 
-export([start/0, allocate/0, deallocate/1, stop/0, clear/0]). 
-export([init/0]).

%% These are the start functions used to create and 
%% initialize the server.

start() -> 
register(frequency, spawn(frequency2, init, [])).

init() -> 
Frequencies = {get_frequencies(), []}, 
loop(Frequencies).

% Hard Coded 
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop 
% add fake sleep in the allocate request handler to simulate workload

loop(Frequencies) -> 
    receive 
        {request, Pid, allocate} -> 
            {NewFrequencies, Reply} = allocate(Frequencies, Pid), 
            timer:sleep(750), % the fake pause 
            Pid ! {reply, Reply}, 
            loop(NewFrequencies); 
        {request, Pid , {deallocate, Freq}} -> 
            NewFrequencies = deallocate(Frequencies, Freq), 
            Pid ! {reply, ok}, 
            loop(NewFrequencies); 
        {request, Pid, stop} -> 
            Pid ! {reply, stopped} 
    end.

% remove all messages from the calling processes's mailbox, printing 
% them as they're extracted. Once there is nothing left, return ok. 
clear() -> 
    receive 
        Msg -> 
        io:format("clearing: ~w~n", [Msg]), 
        clear() 
    after 0 -> 
        ok 
    end.

%% Functional interface 
% i.e. the "client code" 
% clear mailbox before we make a new request 
% hard coded 500ms timeout limit

allocate() -> 
    clear(), % remove any messages which are pending 
    frequency ! {request, self(), allocate}, 
    receive 
        {reply, Reply} -> Reply 
    after 500 -> 
        {reply, timeout} 
    end.

deallocate(Freq) -> 
    clear(), % remove any messages which are pending 
    frequency ! {request, self(), {deallocate, Freq}}, 
    receive 
        {reply, Reply} -> Reply 
    after 500 -> 
        {reply, timeout} 
    end.

% no timeout for stop: just wait until it's done 
stop() -> 
    clear(), % remove any messages which are pending 
    frequency ! {request, self(), stop}, 
    receive 
        {reply, Reply} -> Reply 
    end.

%% The Internal Help Functions used to allocate and 
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) -> 
    {{[], Allocated}, {error, no_frequency}}; 
allocate({[Freq|Free], Allocated}, Pid) -> 
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) -> 
    NewAllocated=lists:keydelete(Freq, 1, Allocated), 
    {[Freq|Free], NewAllocated}.