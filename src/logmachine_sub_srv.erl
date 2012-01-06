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
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

-record(state, {instance,zlist,subscriber,last_from,mode :: recover | normal}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(InstanceName, FromTimestamp, SubPid) ->
    gen_server:start_link(?MODULE, {InstanceName, FromTimestamp, SubPid}, []).
    

%% ====================================================================
%% Server functions
%% ====================================================================

init({InstanceName, FromTimestamp, SubPid}) ->
    link(SubPid),
    ZList=logmachine:get_zlist(InstanceName, FromTimestamp),
    {ok, #state{instance=InstanceName,
                zlist=ZList,
                subscriber=SubPid,
                last_from=FromTimestamp,mode=recover}, 1}.

handle_call(_Request, _From, State) ->
    {reply, unsupported, State}.

handle_cast({Ts, _ }=HistoryEntry, 
            #state{subscriber=SubPid,
                   last_from=LastFrom,
                   mode=normal}=State) when Ts > LastFrom ->
    LastFrom1=send_entries(SubPid, [HistoryEntry], LastFrom),
    {noreply, State#state{last_from=LastFrom1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, 
            #state{instance=InstanceName,
                   zlist=[],
                   subscriber=SubPid,
                   last_from={MgS,S,McS},
                   mode=recover}=State) ->
    % Subscribe for receiver events
    logmachine_receiver_srv:subscribe(InstanceName, self(), {gen_server,cast}),
    % Await buffered messages arrive to RAM cache 
    timer:sleep(200),
    % Check messages in cache
    LastFrom={MgS,S,McS+1},
    ZList=logmachine:get_zlist(InstanceName, LastFrom),
    % Send if any
    LastFrom1=send_entries(SubPid, ZList, LastFrom),
    % Switch mode to normal
    {noreply, State#state{last_from=LastFrom1,mode=normal}};
handle_info(timeout, 
            #state{zlist=ZList,
                   subscriber=SubPid,
                   last_from=LastFrom,
                   mode=recover}=State) ->
    % Take a chunk of 100 entries
    {L, ZList1}=zlists:scroll(100, ZList),
    % Send it to subscriber
    LastFrom1=send_entries(SubPid, L, LastFrom),
    % Update state with tail zlist and new timestamp
    {noreply, State#state{zlist=ZList1,last_from=LastFrom1}, 1};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

send_entries(SubPid, [{T,_}=E], _LastFrom) ->
    SubPid ! E, T;
send_entries(SubPid, EntryList, LastFrom) ->
    zlists:foldl(fun({T,_}=E) -> SubPid ! E, T end, LastFrom, EntryList).

