%%%-------------------------------------------------------------------
%%% @author <vjache@gmail.com>
%%% @copyright (C) 2011, Vyacheslav Vorobyov.  All Rights Reserved.
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @doc
%%%    TODO: Document it.
%%% @end
%%% Created : Jan 5, 2012
%%%-------------------------------------------------------------------------------
-module(logmachine_sub_srv).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

-record(state, {instance,zlist,subscriber,subscriber_mref,last_from,mode :: recover | normal, marker}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(InstanceName, FromTimestamp, SubPid, Marker) ->
    gen_server:start_link(?MODULE, {InstanceName, FromTimestamp, SubPid, Marker}, []).
    

%% ====================================================================
%% Server functions
%% ====================================================================

init({InstanceName, FromTimestamp, SubPid, Marker}) ->
    link(SubPid),
    MRef=monitor(process, SubPid),
    ZList=logmachine:get_zlist(InstanceName, FromTimestamp),
    {ok, #state{instance=InstanceName,
                zlist=ZList,
                subscriber=SubPid,
                subscriber_mref=MRef,
                last_from=FromTimestamp,
                mode=recover,
                marker=Marker}, 1}.

handle_call(_Request, _From, #state{}=State) ->
    {reply, unsupported, State}.

handle_cast({Ts, _ }=HistoryEntry, 
            #state{subscriber=SubPid,
                   last_from=LastFrom,
                   mode=normal,
                   marker=Marker}=State) when Ts > LastFrom ->
    LastFrom1=send_entries(SubPid, [HistoryEntry], LastFrom, Marker),
    {noreply, State#state{last_from=LastFrom1}};
handle_cast(_Msg, #state{}=State) ->
    {noreply, State}.

% History pump done, hence enter normal mode (retranslation)
handle_info(timeout, 
            #state{instance=InstanceName,
                   zlist=[],
                   subscriber=SubPid,
                   last_from={MgS,S,McS},
                   mode=recover,
                   marker=Marker}=State) ->
    % Subscribe for receiver events
    logmachine_receiver_srv:subscribe(InstanceName, self(), {gen_server,cast}),
    % Await buffered messages arrive to RAM cache 
    timer:sleep(200),
    % Check messages in cache
    LastFrom={MgS,S,McS+1},
    ZList=logmachine:get_zlist(InstanceName, LastFrom),
    % Send if any
    LastFrom1=send_entries(SubPid, ZList, LastFrom, Marker),
    % Switch mode to normal
    {noreply, State#state{last_from=LastFrom1,mode=normal}};
% Do pump another one chunk to the subscriber (recovery)
handle_info(timeout, 
            #state{zlist=ZList,
                   subscriber=SubPid,
                   last_from=LastFrom,
                   mode=recover,
                   marker=Marker}=State) ->
    % Take a chunk of 100 entries
    {L, ZList1}=zlists:scroll(100, ZList),
    % Send it to subscriber
    LastFrom1=send_entries(SubPid, L, LastFrom, Marker),
    % Update state with tail zlist and new timestamp
    {noreply, State#state{zlist=ZList1,last_from=LastFrom1}, 1};
% Subscriber process terminated normaly, hence subscription session stops also (normaly)
handle_info({'DOWN', MRef, process, _Object, _Info}, 
            #state{subscriber_mref=MRef}=State) ->
    {stop, normal, State};
handle_info(_Info, #state{}=State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

send_entries(SubPid, [{T,_}=E], _LastFrom, Marker) ->
    SubPid ! {Marker, E}, T;
send_entries(SubPid, EntryList, LastFrom, Marker) ->
    zlists:foldl(fun({T,_}=E, _) -> SubPid ! {Marker, E}, T end, LastFrom, EntryList).

