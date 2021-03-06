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

%% make a simulation to perform a sequence of steps:
    % launch a frequency server, init it, register it as "frequency":
    % launch several clients: allocate and deallocate frequencies
    % kill a client or two, at random, during operation. Server should release that frequency.
    % kill the server and have the system recover gracefully, if possible.


%% to run the program: start_clients(num_clients).
%% A10 means allocated freq 10, D10 means deallocated freq 10, (D10) means the server
%% caught a dying client and released the frequency for him.
%% atimeout is a timeout in Allocate(), dtimeout ditto in Deallocate().
%% Output for a client run goes between "start" and "end".
%% When a client finishes it launches another client with the same Id, for demo purposes.

-module(frequency).

-define(CLIENTTIMEOUT,1000).        % timeout period in ms for client calls
-define(SIMLOAD,500).               % to simulate server load, add 0..SIMLOAD ms delay before responding
-define(INDENT,"             ").    % how many spaces to indent per column

-export([start/0,allocate/0,deallocate/1,stop/0,show/0,clear/0]).
-export([init/0]).
-export([start_clients/1,client/1]).

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

    {'EXIT', Pid, _Reason} ->     %% a client linked to the server has exited!
        NewFrequencies = exited(Frequencies, Pid), % {Free,Allocated}
        %io:format("Client died: ~p~n",[Reason]),
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
        Allocated2 = lists:delete({Freq,Pid}, Allocated),     % this freq did belong to this caller, so release it
        unlink(Pid),    % client deallocated frequencies, so ignore client if it dies now
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
            %io:format("(D~w)~n",[Freq]),
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
    trap(true), % while client holds a frequency, get notified if server dies
    
    catch frequency ! {request, self(), allocate},
    receive 
        {'EXIT', _Pid, _Reason} -> 
            io:format("server died",[]),
            {error,atimeout};
        {reply, Reply} -> Reply    % Reply: {ok, Freq} {error,no_frequency} {error,already_allocated}  {error,atimeout}
    after ?CLIENTTIMEOUT ->        % Don't wait forever, the server may be overloaded
        trap(false),               % no freq, don't bother server if we die
        {error,atimeout}
    end.

deallocate(Freq) -> 
    trap(true),
    catch frequency ! {request, self(), {deallocate, Freq}},
    receive 
        {'EXIT', _Pid, _Reason} -> 
            %io:format("server died",[]),
            {error,atimeout};
	    {reply, Reply} -> 
            trap(false), % while client does not have a frequency, don't care if server dies
            Reply
    after ?CLIENTTIMEOUT ->         % the server must be overloaded
        trap(true),              % since we did not release our freq, maintain link to server.        
        {error,dtimeout}
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
        {reply, Reply} ->     Reply
    end.

% parameterise the client so that it can behave in different ways when called with different parameters

start_clients(Num_clients) ->      % launch a bunch of clients numbered 1..n, as processes
    start(),                        % launch the server first... duh!
    label(1,Num_clients+1,1),          % print labels
    io:format("~n--------------------------------------------------------------------------~n",[]),
    
    client_loop(Num_clients),

    timer:kill_after(15000,whereis(frequency)). % will kill server after 15s.

client_loop(0) ->
    clients_launched;
client_loop(Num_clients) ->
    spawn(?MODULE,client,[Num_clients]),
    client_loop(Num_clients-1).

client(Id) ->
    trap(false),  % not yet holding a freq. Allocate() will turn this on.
    case whereis(frequency) of
        undefined -> exit(no_server);
        _Other -> ok
    end,
    catch link(whereis(frequency)),           % link to server, bidirectionally
    register(list_to_atom("c"++integer_to_list(Id)),self()),    % register client with a name c1, c2...
    % client will NOT be notified if server dies until it holds a frequency.
    % we wish for the client to be notified ONLY while it's holding a frequency:

    print_client_msg(Id,"start"),
    
    % request a freq:
    case allocate() of 
        % values for Reply: {ok, Freq}, {error,no_frequency},{error,already_allocated},{error,atimeout}
        {error,Reply} -> 
            print_client_msg(Id,Reply),
            timer:sleep(timer:seconds(1)+rand:uniform(timer:seconds(4)));     
        
        {ok,Frequency} -> 
            print_client_msg(Id,"A~w~n",[Frequency]),         
            timer:sleep(1000+rand:uniform(4000)),     % hold it for a little random while...

            % return the freq to the server:
            case deallocate(Frequency) of
                {'EXIT', _Pid, _Reason} ->    %% server has died!
                    print_client_msg(Id,"Server died.~n");
                ok ->
                    print_client_msg(Id,"D~w~n",[Frequency]),
                    timer:sleep(timer:seconds(1)+rand:uniform(timer:seconds(4)));  
                {error,ErrMsg} ->
                    print_client_msg(Id,ErrMsg),
                    print_client_msg(Id,"(D~w)~n",[Frequency]),
                    % need to deallocate! commit sepuku, let the server clear my freq:
                    exit(dtimeout)
            end;

        {'EXIT', _Pid, _Reason} ->    %% server has died!
            print_client_msg(Id,"Server died~n"); 

        Other -> 
            print_client_msg(Id,"Unk result for alloc(): ~p~n",[Other])
    end,
    unregister(list_to_atom("c"++integer_to_list(Id))),
    print_client_msg(Id,"end"),

spawn(?MODULE,client,[Id]).

% auxiliary functions -------------------------------------------------------------------
% for printing msgs, indented under client column:
print_client_msg(Col,Format,Params) ->              
    io:format("~s",[string:copies(?INDENT,Col-1)]),
    io:format(Format,Params).

print_client_msg(Col,Msg) when is_list(Msg)->
    % print a message indented depending on the client id
    io:format("~s~n",[string:copies(?INDENT,Col-1)++Msg]);
print_client_msg(Col,Msg) when is_atom(Msg)->
    io:format("~s~n",[string:copies(?INDENT,Col-1)++atom_to_list(Msg)]);
print_client_msg(Col,Msg) when is_integer(Msg) -> %frequency
    io:format("~p~n",[string:copies(?INDENT,Col-1)++Msg]);
print_client_msg(Col,Msg) ->
    io:format("~p~n",[string:copies(?INDENT,Col-1)++Msg]).

% for artificial delays:
simulate_load()->timer:sleep(rand:uniform(?SIMLOAD)).

% for printing headings:
label(I, N, _) when I == N -> 1; 
label(I, N, D) when I < N -> 
    io:format("C~w~s", [I,lists:sublist(?INDENT,length(?INDENT)-2)]), 
    label(I+D, N, D).


trap(true)->
    process_flag(trap_exit, true);
trap(false)->
    process_flag(trap_exit, false).
