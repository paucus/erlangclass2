%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).
-export([loop/1,inject/1]). % Added

%% This is the "old" version of code, modified so that it can force updates
%% on message "upgrade_code".

start() ->
    register(frequency,
	     spawn(frequency, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  io:format("This is the NEW code~n",[]),
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);     %NOTE: not qualified, will run existing code
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
% Added:
    {request, Pid, {inject, Freqs}} ->
        NewFrequencies = inject(Frequencies, Freqs),
        Pid ! {reply, injected},
        ?MODULE:loop(NewFrequencies);

% Added:
    {request, Pid, upgrade_code}->
            compile:file("new/frequency"),
            code:soft_purge(?MODULE),
            code:load_file(?MODULE),
            Pid ! {reply,upgraded_code},
            % note fully-qualified module:function(), to force code reload:
            ?MODULE:loop(Frequencies)
    end.

%% Functional interface

allocate() ->
    frequency ! {request, self(), allocate},
    receive
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) ->
    frequency ! {request, self(), {deallocate, Freq}},
    receive
	    {reply, Reply} -> Reply
    end.

stop() ->
    frequency ! {request, self(), stop},
    receive
	    {reply, Reply} -> Reply
    end.

% Added:
  inject(Freqs) ->
  	frequency ! {request, self(), {inject, Freqs}},
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
  {[Freq|Free],  NewAllocated}.

inject({Free, Allocated}, Freqs) ->
	{Free ++ Freqs, Allocated}.
