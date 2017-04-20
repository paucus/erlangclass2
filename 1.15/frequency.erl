%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%% Step 1.15 ASSIGNMENT: Enhancing the frequency server

%% Added flushing of mailboxes, timeouts, delays to simulate server load, a show() client method to 
%% see the current frequencies stack, a check to not start the server if it is already running,
%% trace messages, a testing function. In the additional file, some sample executions.

-module(frequency).

-define(CLIENTTIMEOUT,1000).        % timeout period in ms for client calls
-define(SIMLOAD,2000).              % to simulate server load, add 0..SIMLOAD ms delay before responding

-export([start/0,allocate/0,deallocate/1,stop/0,show/0,clear/0]).
-export([init/0]).
-export([testit/0]).

%% The Server code:

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

get_frequencies() -> [10,11,12,13,14,15].       % hardcoded, for now


loop(Frequencies) ->                            % The Main Loop
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      timer:sleep(rand:uniform(?SIMLOAD)),          % simulate server load, introduce random delay up to 1.5s
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    
    {request, Pid , {deallocate, Freq}} ->
      {NewFrequencies,Reply} = deallocate(Frequencies, {Freq,Pid}),
      timer:sleep(rand:uniform(?SIMLOAD)),          % simulate server load, introduce random delay up to 1.5s
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    
    {request, Pid, stop} ->
        io:format("Frequency server stopping (server)... ~n",[]),
        Pid ! {reply, stopped};

    {request, Pid, show} -> % show current frequency allocations 
        Pid ! {reply, Frequencies},
        loop(Frequencies)
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies:

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};                   % no free freqs, return error msg
allocate({[Freq|Free], Allocated}=Frequencies, Pid) ->        
    case  lists:keymember(Pid,2,Allocated) of                 % see if this Pid already has a frecuency (key in 2nd pos)
      true ->  {Frequencies, {error, already_allocated}};     % will not allocate second freq to same pid
      false -> {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}  % first allocation to this Pid, return rest of frequencies
    end.


deallocate({Free, Allocated}, {Freq,Pid}) ->
  case lists:member({Freq,Pid}, Allocated) of 
    true -> 
        Allocated2 = lists:delete({Freq,Pid}, Allocated),     % this Pid did have this freq, so release it
        {{[Freq|Free], Allocated2}, ok};
    false -> 
        {{Free, Allocated}, {error, not_allocated}}           % whatcha talkin 'bout bro? u didn't have this...
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Functional interface (the Client):


start()->
    case whereis(?MODULE) of
        undefined ->
            Pid=spawn(?MODULE,init,[]),
            register(frequency,Pid),
            io:format("Created and registered server 'frequency' with Pid ~w ~n",[Pid]),
            clear();          % flush any messages in Client mailbox, JIC...
        _Pid -> server_already_running
    end.
        

stop() -> 
    ?MODULE ! {request, self(), stop},
    receive 
	    {reply, Reply} -> 
            io:format("Frequency server stopping...(client) ~n ~n",[]),
            Reply
    end.

% requirement: add timeouts to the client code that asks to allocate or deallocate a frequency:
allocate() -> 
    clear(),  % just in case
    frequency ! {request, self(), allocate},
    receive 
        {reply, Reply} -> Reply
    after ?CLIENTTIMEOUT ->        % Don't wait forever, the server may be overloaded
        io:format("Client: server took too long to allocate a frequency. ~n",[])
    end.

deallocate(Freq) -> 
    clear(),  % just in case
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    after ?CLIENTTIMEOUT ->               % Don't wait forever,  the server must be overloaded
        io:format("Client: server took too long to deallocate my frequency. ~n",[]),
        error
    end.


clear() ->            % clear the mailbox of any leftover msgs
    receive
        Msg -> 
            io:format("Client: Clearing a message from mailbox: ~w ~n",[Msg]),
            clear() 
    after 0 -> 
        %io:format("Client: done clearing messages. ~n",[]),
        ok           % just like in Erlang Programming p. 106... 
    end.             % AFTER ensures it will continue if no msgs are present

show()->
    clear(),  % just in case
    frequency ! {request, self(), show},
    receive 
	    {reply, Reply} -> Reply
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
testit()->
    % launch a server, init it.
    ?MODULE:start(),          

    %% Allocate a freq:
    ?MODULE:allocate(),
    receive 
        {error, already_allocated} ->
            io:format("already allocated! ~n",[]),
            F1=error;
        {reply,{ok,F1}} ->  
            io:format("allocated: ok ~w~n",[F1]);
        F1 ->  io:format("allocated: ?? ~w~n",[F1])
    end,

    ?MODULE:show(),

    %% Deallocate the freq.
    ok = ?MODULE:deallocate(F1),
    receive 
        {reply,Msg2} -> Msg2,
        io:format("deallocated:  ~w~n",[Msg2]);
        F2 ->  io:format("deallocated: ?? ~w~n",[F2])
    end,

    ?MODULE:show(),

    ?MODULE:stop(),

    done.

