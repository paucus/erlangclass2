%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%% assignment 3.4
%% Marcelo Ruiz Camauër

%% implement replicated servers, using round-robin scheduling.


-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init_balancer/0,init_freq_server/2]).          % Added
-export([inject/1,whois/1,whoami/0,test/0,test_many_clients/1,client/0]).            % Added


start() ->
    spawn(?MODULE, init_balancer, []).
 
init_balancer()->                               % Added
    catch register(balancer,self()),
    case whereis(fs1) of
      undefined ->  
        spawn_link(?MODULE, init_freq_server, [fs1,get_frequencies1()]),  % init a FS with name, frequencies to use
        io:format("launched fs1~n",[]);    
      _ -> true
    end,
    case whereis(fs2) of
      undefined ->  
          spawn_link(?MODULE, init_freq_server, [fs2,get_frequencies2()]),
          io:format("launched fs2~n",[]);    
      _ -> true
    end,
    process_flag(trap_exit, true),              % if a FS dies, get notified, don't just die.
    balancer_loop(1).


init_freq_server(MyName,MyFrequencies) ->        % Added
    register(MyName,self()),  
    Frequencies = {MyFrequencies, []},          % create {free, allocated} tuple
    fs_loop(Frequencies).

% Hard Coded, could have been a DETS table.
get_frequencies1() -> [10,11,12,13,14,15].      % must correspond with server_handling_frequency()
get_frequencies2() -> [20,21,22,23,24,25].

balancer_loop(Count) ->
  receive
      {request, Pid, allocate} ->
          % implement a distribution policy, like round-robin, least_assigned, random, etc.
          case Count rem 2 == 0 of  % round-robin
            true ->   fs1 ! {request, Pid , allocate}, Alternate_server = whereis(fs2);    
            false ->  fs2 ! {request, Pid , allocate}, Alternate_server = whereis(fs1)
          end,
          % what if no frequencies were left in that server? call the other one!
          receive
                {error,_} -> Alternate_server ! {request, Pid , allocate}
          after 0 -> ok
          end,

          balancer_loop(Count + 1);

      {request, Pid , {deallocate, Freq}} ->
          server_handling_frequency(Freq) ! {request, Pid , {deallocate, Freq}},
          balancer_loop(Count);

      {'EXIT', Pid, _Reason} ->     % a frequency server has died!
          io:format("Balancer: FS '~p' has died.~n",[Pid]),
          % now what? kill the whole system? restart that server? 
          % (it won't have clients)
          init_balancer();

      {request, Pid, stop} ->      % stop all processes
          fs1 ! {request,self(),stop},   
          fs2 ! {request,self(),stop}, 
          Pid ! {reply, balancer_stopped}
  end.

%% The Main Loop for Frequency Servers:
fs_loop(Frequencies) ->
  receive
      {request, Pid, allocate} ->
          link(Pid),    % if FS dies, take this client with it!
          {NewFrequencies, Reply} = allocate(Frequencies, Pid),
          %io:format("FS: '~p' allocated ~p (~w).~n",[whoami(),Reply,NewFrequencies]),
          io:format("FS: '~p' allocated ~p.~n",[whoami(),Reply]),

          Pid ! {reply, Reply},     
          fs_loop(NewFrequencies);     

      {request, Pid , {deallocate, Freq}} ->
          NewFrequencies = deallocate(Frequencies, Freq),
          Pid ! {reply, {ok,whoami()}},
          %io:format("FS: '~p' deallocated ~p (~w).~n",[whoami(),Freq,NewFrequencies]),
          io:format("FS: '~p' deallocated ~p .~n",[whoami(),Freq]),
          unlink(Pid), % don't kill the client if I die, it does not have one of my frequencies
          fs_loop(NewFrequencies);
      
      {request, Pid, stop} ->
          io:format("FS: '~p' stopping.~n",[whoami()]),
          Pid ! {reply, fs_stopped};
      
      {request, Pid, {inject, Freqs}} ->
          NewFrequencies = inject(Frequencies, Freqs),
          Pid ! {reply, injected},
          ?MODULE:fs_loop(NewFrequencies)
    end.

%% Functional interface:

allocate() ->
    balancer ! {request, self(), allocate},
    receive
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) ->
    balancer ! {request, self(), {deallocate, Freq}},
    receive
	    {reply, Reply} -> Reply
    end.

stop() ->
    balancer ! {request, self(), stop},
    receive
	    {reply, Reply} -> Reply
    end.

inject(Freqs) ->
  	balancer ! {request, self(), {inject, Freqs}},
  	receive
  		{reply, Reply} -> Reply
  	end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies, aka the Frequency Server code:

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

inject({Free, Allocated}, Freqs) ->
	{Free ++ Freqs, Allocated}.

whois(Pid)->
  {registered_name, Name} = process_info(Pid,registered_name),
  Name.
whoami()->
  {registered_name, Name} = process_info(self(),registered_name),
  Name.

server_handling_frequency(Freq) when Freq >= 10, Freq =< 15 -> 
    whereis(fs1) ;
server_handling_frequency(Freq) when Freq >= 20, Freq =< 25 -> 
    whereis(fs2).

test()->
    frequency:allocate(), % should be 20
    frequency:allocate(), % 10
    frequency:allocate(), % 21
    frequency:deallocate(20),
    frequency:deallocate(10),
    frequency:allocate(), 
    frequency:allocate(), 
    frequency:allocate(), 
    frequency:allocate(), 
    frequency:allocate(), 
    frequency:allocate(), 
    frequency:allocate(), 
    frequency:allocate(), 
    frequency:allocate(), 
    frequency:allocate(), 

    %frequency:stop()
    done. 

test_many_clients(100)->
    inited_clients;
test_many_clients(N)->
    spawn(?MODULE,client,[]),
   %timer:sleep(rand:uniform(100)),
    test_many_clients(N+1).

client()->
    balancer ! {request,self(),allocate},
    
    receive
        {reply,{ok,Freq}}               -> 
            %timer:sleep(rand:uniform(100)),
            balancer ! {request,self(),{deallocate,Freq}};
            %timer:sleep(rand:uniform(100));
        {reply,{error,no_frequency}}    -> 
            timer:sleep(rand:uniform(100)),
            client();
        Other  ->      
            io:format("got other msg: ~w~n",[Other]),
            Other
    end.


