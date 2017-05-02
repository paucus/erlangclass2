%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(gf).
-behaviour(gen_server).

% API
-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-export([handle_info/2, terminate/2, code_change/3]).

% API
-export([allocate/0,deallocate/1,stop/0, report/0]).
% inject


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API

start_link() ->
    gen_server:start_link(
		    {local, ?MODULE},
		    ?MODULE, [], []).

%%%===================================================================
%%% Functional interface
%%%===================================================================

allocate() ->
    gen_server:call(?MODULE,allocate).

deallocate(Freq) ->
    gen_server:call(?MODULE,{deallocate,Freq}).

report()->
    gen_server:call(?MODULE,report).

% inject()->

stop() ->
    gen_server:stop(?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Frequencies = {get_frequencies(), []},
    {ok, Frequencies}.

handle_call(allocate, From, State) ->
    {NewFrequencies, Reply} = allocate(State,From),
    {reply, Reply, NewFrequencies};

handle_call({deallocate, Freq}, From,State) ->
    {NewFrequencies, Reply} = deallocate(State,Freq),
    {reply, Reply, NewFrequencies};

handle_call(report, _From, State) ->
    {{Free,Allocated}, State} = report(),
    {reply, {Free,Allocated}, State}.

% handle_cast(inject, State) ->
%   'for you to do';

handle_cast(stop, State) ->
    {stop,normal,State}.

% default:
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

get_frequencies() -> [10,11,12,13,14,15].

% inject()->
