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

-define(CLIENTTIMEOUT,1000).       % timeout period in ms for client calls
-define(SIMLOAD,50).              % to simulate server load, add 0..SIMLOAD ms delay before responding
-define(INDENT,"             ").       % how many spaces to indent per column
-define(NUMCLIENTRUNS,5).           % how many times to run client loop in each client instance

-export([start/0,allocate/0,deallocate/1,stop/0,show/0,clear/0]).
-export([init/0]).
-export([print_client_msg/2,print_client_msg/3,start_clients/1,client/1]).

%% The Server code:

init() ->
    trap(true), % server will trap if a client dies
    Frequencies = {get_frequencies(), []},
    io:format("Server up at ~w with ~p~n",[self(),Frequencies]),
    loop(Frequencies).

get_frequencies() -> [10,11,12,13,14].       % hardcoded, for now


loop(Frequencies) ->                            % The Main Loop
  receive
    {request, Pid, allocate} ->
        {NewFrequencies, Reply} = allocate(Frequencies, Pid),
        simulate_load(),
        Pid ! {reply, Reply},     % Reply can be {error, already_allocated}, {ok, Freq}, {error, no_frequency}
        loop(NewFrequencies);
    
    {request, Pid , {deallocate, Freq}} ->
        {NewFrequencies,Reply} = deallocate(Frequencies, {Freq,Pid}),
        simulate_load(),
        Pid ! {reply, Reply},   % Reply can be {error, not_allocated}, ok
        loop(NewFrequencies);
    
    {request, Pid, show} ->     % show current frequency allocations 
        Pid ! {reply, Frequencies},
        loop(Frequencies);

    {'EXIT', Pid, Reason} ->     %% a client linked to the server has exited!
        NewFrequencies = exited(Frequencies, Pid), % {Free,Allocated}
        io:format("Client died: ~p~n",[Reason]),
        %io:format("Freqs: ~w~n",[NewFrequencies]),
        clear(),
        loop(NewFrequencies);

     {request, Pid, stop} ->
        Pid ! {reply, stopped}     % will exit server loop
 end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies:

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};               % no free freqs, return error msg
allocate({[Freq|Free], Allocated}=Frequencies, Pid) ->   
    link(Pid),                                              % while a client has a frequency, get notified if it dies.     
    case  lists:keymember(Pid,2,Allocated) of               % see if this Pid already has a frecuency (key in 2nd pos)
      true ->  
          {Frequencies, {error, already_allocated}};        % will not allocate second freq to same pid
      false -> {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}  % first allocation to this Pid, return rest of frequencies
    end.


deallocate({Free, Allocated}, {Freq,Pid}) ->
  case lists:member({Freq,Pid}, Allocated) of 
    true -> 
        unlink(Pid),    % client deallocated frequencies, so ignore client if it dies now
        Allocated2 = lists:delete({Freq,Pid}, Allocated),     % this freq did belong to this caller, so release it
        {{[Freq|Free], Allocated2}, ok};
    false -> 
        {{Free, Allocated}, {error, not_allocated}}           % whatcha talkin 'bout bro? u didn't have this freq...
  end.


exited({Free, Allocated}, Pid) ->  
    % a client linked to server has exited, so return their assigned 
    % frequency to list of available frequencies.
    case lists:keysearch(Pid,2,Allocated) of
        {value,{Freq,Pid}} ->
            NewAllocated = lists:keydelete(Freq,1,Allocated),
            io:format("(D~w)~n",[Freq]),
            {[Freq|Free],NewAllocated};
        false ->
            {Free,Allocated}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Functional interface (the Client):

start()->
    case whereis(?MODULE) of        % verify if server is already running
        undefined ->
            Pid=spawn(?MODULE,init,[]),
            register(frequency,Pid),
            io:format("Launched 'frequency' (~w)~n",[Pid]),
            clear();          % flush any messages in Client mailbox, JIC...
        _Pid -> clear(), server_already_running
    end.
        
stop()->
    stop(0).
stop(Id) -> 
    %start(),
    ?MODULE ! {request, self(), stop},
    receive 
	    {reply, Reply} -> 
            print_client_msg(Id,Reply)
    end.

allocate() -> 
    clear(),  
    trap(true), % while client holds a frequency, get notified if server dies
    
    frequency ! {request, self(), allocate},
    receive 
        {reply, Reply} -> Reply    % Reply: {ok, Freq} {error,no_frequency} {error,already_allocated}  {error,atimeout}
    after ?CLIENTTIMEOUT ->        % Don't wait forever, the server may be overloaded
        {error,atimeout}
    end.

deallocate(Freq) -> 
    clear(),  
    
    frequency ! {request, self(), {deallocate, Freq}},
    trap(false), % while client does not have a frequency, don't care if server dies
    
    receive 
	    {reply, Reply} -> Reply
    after ?CLIENTTIMEOUT ->         % the server must be overloaded
        trap(true),              % since we did not release freq, maintain link to server.        
        {error,dtimeout}
        % now client still holds a frequency, in next loop will try to acquire another one...
    end.


clear() ->            
    % clear the client mailbox of any leftover msgs
    receive
        _Msg -> 
            clear() 
    after 0 -> 
        ok           % just like in Erlang Programming p. 106... 
    end.             % AFTER ensures it will continue if no msgs are present

show()->        
    % show the current state of allocated frequencies in the server
    clear(),  
    frequency ! {request, self(), show},
    receive 
	    %{reply, Reply} -> Reply
        {reply, Reply} -> 
            io:format("~w ~n",[Reply]),
            Reply
    end.

% parameterise the client so that it can behave in different ways when called with different parameters

start_clients(Num_clients) ->      % launch a bunch of clients numbered 1..n
    start(),                       % launch the server first... duh!
 
    % print headings:
    for(1,Num_clients+1,1),
    io:format("~n--------------------------------------------------------------------------~n",[]),

    % launch N clients:
    [spawn(?MODULE,client,[Id]) || Id <- lists:seq(1,Num_clients)],
    clients_launched.


% make a simulated Client perform a sequence of steps:
    % launch a server, init it, register it as "frequency":
    % launch several clients: allocate and deallocate frequencies
    % kill a client or two, at random
    % kill the server and have the system recover gracefully, if possible.
client(Id) ->
    % function called on spawn(), it initializes a Client process

    % client will NOT be notified if server dies until it holds a frequency.
    % we wish for the client to be notified ONLY while it's holding a frequency,
    % so it can re-register it with the server once it has been restarted.
    % while server is down client can continue operating (i.e. continue phone call)
    %trap(Id,false),                    
    process_flag(trap_exit, false),
    link(whereis(frequency)),    % link to server, bidirectionally

    register(list_to_atom("c"++integer_to_list(Id)),self()),    % register client with a name c1, c2...
    client_loop(Id,?NUMCLIENTRUNS). % do set of actions a few times per client
    %unregister(self())
    



% a Client will perform Actions in a loop a certain number of times:
client_loop(Id,0) ->
    print_client_msg(Id,"done");
client_loop(Id,I) ->
    client_actions(Id),     % alloc, dealloc
    timer:sleep(rand:uniform(5000)),
    client_loop(Id,I-1).

% Actions that a Client will perform:
client_actions(Id)->
    print_client_msg(Id,"start loop"),
    link(whereis(frequency)),
    trap(Id,true),  % not yet holding a freq. Allocate() will turn this on.
    
    % request a freq:
    case allocate() of 
        % values for Reply: {ok, Freq}, {error,no_frequency},{error,already_allocated},{error,atimeout}
        {error,Reply} -> 
            print_client_msg(Id,Reply),
            clear(); % will exit...
        
        {ok,Frequency} -> 
            print_client_msg(Id,"A~w~n",[Frequency]),         
            timer:sleep(1000+rand:uniform(500)),     % hold it for 1-1.5s...

            % return the freq to the server:
            case deallocate(Frequency) of
                ok ->
                    print_client_msg(Id,"D~w~n",[Frequency]);  
                {error,ErrMsg} ->
                    print_client_msg(Id,ErrMsg),
                    % need to deallocate! commit sepuku, let server clear my holdings:
                    exit(dtimeout) 
             end;

        {'EXIT', _Pid, Reason} ->    %% server has died!
            print_client_msg(Id,"~p: Server died: ~p~n",[Id,Reason]);

        Other -> 
            print_client_msg(Id,"Unk msg: ~p~n",[Other])
    end,
    trap(Id,false),
    unlink(whereis(frequency)),
    print_client_msg(Id,"end loop").

% auxiliary functions -------------------------------------------------------------------
% for printing msgs, indented under client column:
print_client_msg(Num,Format,Params) ->              
    % function to indent depending on client id and then work like normal io:format...
    io:format("~s",[string:copies(?INDENT,Num-1)]),
    io:format(Format,Params).

print_client_msg(Num,Msg) when is_list(Msg)->
    % print a message indented depending on the client id
    io:format("~s~n",[string:copies(?INDENT,Num-1)++Msg]);
print_client_msg(Num,Msg) when is_atom(Msg)->
    io:format("~s~n",[string:copies(?INDENT,Num-1)++atom_to_list(Msg)]);
print_client_msg(Num,Msg) when is_integer(Msg) -> %frequency
    io:format("~p~n",[string:copies(?INDENT,Num-1)++Msg]);
print_client_msg(Num,Msg) ->
    io:format("~p~n",[string:copies(?INDENT,Num-1)++Msg]).

% for artificial delays:
simulate_load()->timer:sleep(rand:uniform(?SIMLOAD)).

% for printing headings:
for(I, N, _) when I == N -> 1; 
for(I, N, D) when I < N -> 
    io:format("C~w~s", [I,lists:sublist(?INDENT,length(?INDENT)-2)]), 
    for(I+D, N, D).

trap(_Id,true)->
    %print_client_msg(Id,"trap ON"),
    process_flag(trap_exit, true);
trap(_Id,false)->
    %print_client_msg(Id,"trap OFF"),
    process_flag(trap_exit, false).

trap(true)->
    %print_client_msg(1,"s:trap ON"),
    process_flag(trap_exit, true);
trap(false)->
    %print_client_msg(1,"s:trap OFF"),
    process_flag(trap_exit, false).