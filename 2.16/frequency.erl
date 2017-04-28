%% Based on code from
%%   Erlang Programming
%%   Francesco Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%% Step 2.16 ASSIGNMENT: Enhancing the frequency server


%% to run the program: start_clients(num_clients).
%% A10 means allocated freq 10, D10 means deallocated freq 10, (D10) means the server
%% caught a dying client and released the frequency for him.
%% atimeout is a timeout in Allocate(), dtimeout ditto in Deallocate().
%% Output for a client run goes between "start" and "end".
%% When a client finishes it launches another client with the same Id, for demo purposes.

-module(frequency).

-define(CLIENTTIMEOUT,100000).        % timeout period in ms for client calls
-define(SIMLOAD,10).               % to simulate server load, add 0..SIMLOAD ms delay before responding
-define(INDENT,"             ").    % how many spaces to indent per column

-export([start/0,allocate/0,deallocate/1,stop/0,show/0,clear/0]).
-export([init_fs_monitor/0,init_fs/0]).
-export([start_clients/1,client/1]).

%% The Server code:

start()->
  % start the system
  % launch a supervisor.
  %   supervisor launches frequency server
  %   supervisor monitors frequency server
  % launch clients, which link to frequency server
    case whereis(frequency_monitor) of  % verify if monitor is already running
        undefined ->
            MonitorPid=spawn(?MODULE,init_fs_monitor,[]),
            register(frequency_monitor,MonitorPid),
            io:format("~w Launched 'frequency_monitor' (~w)~n",[linenum(),MonitorPid]);

        _Pid -> monitor_already_running
    end.


init_fs_monitor()->
  % initialization of MONITOR
  case whereis(frequency) of        % verify if frequency server is already running
      undefined ->
          launch_frequency_server(),
          trap(true),   % catch EXITS in case FS dies
          monitor_loop(1);
      _Pid -> fserver_already_running
  end.

launch_frequency_server()->
    FServerPid=spawn_link(?MODULE,init_fs,[]),
    register(frequency,FServerPid),
    io:format("~w Launched 'frequency' (~w)~n",[linenum(),FServerPid]),
    ok.

  init_fs() ->
      % initialization of FREQUENCY SERVER
      trap(false),       % trap if a client dies, but not while they don't hold a frequency.
      Frequencies = {get_frequencies(), []},  % allocated[] and free[] frequencies
      io:format("~w FServer up with ~w~n",[linenum(),Frequencies]),
      loop(Frequencies).

  get_frequencies() -> [10,11,12,13].       % hardcoded, for now

monitor_loop(Count) ->
  %io:format("~w Times FS server started: ~w~n",[linenum(),Count]),
  receive
    {'EXIT', _Pid, _Reason} ->     % the frequency server has died!
        print_client_msg(1,"Monitor: FS died.----------------------------------------------~n",[]),
        launch_frequency_server(),
        monitor_loop(Count + 1);

     {request, Pid, stop} ->
        frequency ! {request,self(),stop},   % stop the frequency server
        Pid ! {reply, stopped}
  end.

loop(Frequencies) ->                            % The Main Loop
  receive
    {request, Pid, allocate} ->
        {NewFrequencies, Reply} = allocate(Frequencies, Pid),
        simulate_load(),
        Pid ! {reply, Reply},     % Reply can be {error, already_allocated}, {ok, Freq}, {error, no_frequency}
        loop(NewFrequencies);

    {request, Pid , {deallocate, Freq}} ->    %% Added try-catch
        try deallocate(Frequencies, {Freq,Pid}) of
            {NewFrequencies,Reply} ->
                simulate_load(),
                Pid ! {reply, Reply},
                loop(NewFrequencies)
        catch
            throw:unallocated_frequency_deallocation ->   % thrown in deallocate()
                io:format("~w FS: catch unallocated_frequency_deallocation.~n",[linenum()]),
                Pid ! {error,deallocation_error}, % tell the client the deallocation failed.
                loop(Frequencies)
        end;

    {request, Pid, show} ->     % show current frequency allocations
        Pid ! {reply, Frequencies},
        loop(Frequencies);

    {'EXIT', Pid, _Reason} ->     %% a client linked to the server has exited!
        NewFrequencies = exited(Frequencies, Pid), % {Free,Allocated}
        io:format("~w FS:  Exit.~n",[linenum()]),
        loop(NewFrequencies);

     {request, Pid, stop} ->
        io:format("~w FS: stop.~n",[linenum()]),
        Pid ! {reply, stopped};     % will exit server loop

    _ ->  % received unknown message
        io:format("~w FS: will throw unknown message.~n",[linenum()]),

        throw(unknown_message)
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
        %{{Free, Allocated}, {error, not_allocated}}           % whatcha talkin 'bout bro? u didn't have this freq...
        throw(unallocated_frequency_deallocation)
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
    catch frequency ! {request, self(), allocate},
    receive
%        {'EXIT', Pid, _Reason} ->
%            print_client_msg(1,"a:Server ~p died.~n",[Pid]),
%            {error,atimeout};
        {reply, Reply} ->
            trap(true), % while client holds a frequency, get notified if server dies
            Reply    % Reply: {ok, Freq} {error,no_frequency} {error,already_allocated}  {error,atimeout}
    after ?CLIENTTIMEOUT ->        % Don't wait forever, the server may be overloaded
        trap(false),               % no freq, don't bother server if we die
        {error,atimeout}
    end.

deallocate(Freq) ->
    trap(true),
    catch frequency ! {request, self(), {deallocate, Freq}},
    receive
        {error,deallocation_error} ->
          %io:format("~w Client: deallocation_error.~n",[linenum()]),
          {error,deallocation_error};
        {'EXIT', _Pid, _Reason} ->
           io:format("~w dealloc: got EXIT.~n",[linenum()]),
           {error,atimeout};
	      {reply, Reply} ->
            trap(false), % while client does not have a frequency, we don't care if server dies
            Reply;
      Other ->
        io:format("~w Client: Other msg: ~w.~n",[linenum(),Other]),
        Other

    after ?CLIENTTIMEOUT ->      % the server must be overloaded or missing
        trap(false),             % die without killing the server too
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
    start(),                       % launch the server first... duh!
    label(1,Num_clients+1,1),      % print labels identifying clients
    io:format("~n"++string:copies("_",80)++"~n",[]),

    % print_client_msg(1,"will kill freq server in 15s, monitor will restart it"),
    % _TimerRef=timer:kill_after(15000,whereis(frequency)),
    % print_client_msg(1,"will kill client c1 in 3s, FS will restart it"),
    % _TimerRef2=timer:kill_after(3000,whereis(c1)),
    % print_client_msg(1,"will kill Monitor in 20s, what will happen?"),
    % _TimerRef3=timer:kill_after(20000,whereis(frequency_monitor)),
    client_loop(Num_clients).

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
            timer:sleep(timer:seconds(1+rand:uniform(3)));

        {ok,Frequency} ->
            print_client_msg(Id,"A~w~n",[Frequency]),
            timer:sleep(timer:seconds(1+rand:uniform(3))),     % hold it for a little random while...

            % return the freq to the server:
            Frequency1=Frequency,
            case deallocate(Frequency1) of
                %{'EXIT', Pid, _Reason} ->    %% server has died!
                %    print_client_msg(Id,"dd:Server ~p died.~n",[Pid]);
                ok ->
                    print_client_msg(Id,"D~w~n",[Frequency1]),
                    timer:sleep(timer:seconds(1)+rand:uniform(timer:seconds(4)));

                % {error,deallocation_error} ->
                %     trap(false),
                %     %print_client_msg(Id,"deallocation_error~n",[]),
                %     {error,deallocation_error};

                {error,ErrMsg} ->
                    print_client_msg(Id,ErrMsg),
                    print_client_msg(Id,"error with (D~w)~n",[Frequency1])
            end;

  %      {'EXIT', Pid, _Reason} ->    %% server has died!
  %          print_client_msg(Id,"FS: Server ~p died.~n",[Pid]);

        Other ->
            print_client_msg(Id,"Unk result for alloc(): ~p~n",[Other])
    end,

    receive
      {'EXIT', Pid, _Reason} ->    %% server has died!
          print_client_msg(Id,"X:Server ~p died.~n",[Pid])
    after 0 ->
      ok
    end,
    unregister(list_to_atom("c"++integer_to_list(Id))),
    print_client_msg(Id,"end"),

spawn(?MODULE,client,[Id]).

% auxiliary functions -------------------------------------------------------------------
% for printing msgs, indented under client column:
print_client_msg(Col,Format,Params) ->
    io:format("~w ~s",[linenum(),string:copies(?INDENT,Col-1)]),
    io:format(Format,Params).

% print a message indented depending on the client id and with monotonic line counter
print_client_msg(Col,Msg) when is_list(Msg)->
    io:format("~w ~s~n",[linenum(),string:copies(?INDENT,Col-1)++Msg]);
print_client_msg(Col,Msg) when is_atom(Msg)->
    io:format("~w ~s~n",[linenum(),string:copies(?INDENT,Col-1)++atom_to_list(Msg)]);
%print_client_msg(Col,Msg) when is_integer(Msg) -> %frequency
%    io:format("~w ~p~n",[linenum(),string:copies(?INDENT,Col-1)++Msg]);
print_client_msg(Col,Msg) ->
    io:format("~w ~p~n",[linenum(),string:copies(?INDENT,Col-1)++Msg]).

% for artificial delays:
simulate_load()->timer:sleep(rand:uniform(?SIMLOAD)).

% for printing headings:
label(I, N, _) when I == N -> 1;
label(I, N, D) when I < N ->
    io:format("~w C~w~s", [linenum(),I,lists:sublist(?INDENT,length(?INDENT)-2)]),
    label(I+D, N, D).

linenum()->
    erlang:unique_integer([monotonic,positive]).

trap(true)->
    process_flag(trap_exit, true);
trap(false)->
    process_flag(trap_exit, false).

%pid_to_name(Pid) ->
%  {registered_name} = process_info(Pid).
