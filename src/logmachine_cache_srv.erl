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
%%% Created : Nov 05, 2011
%%%-------------------------------------------------------------------------------
-module(logmachine_cache_srv).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-import(logmachine_util,[make_name/1,send_after/2,to_millis/1,now_to_millis/1,millis_to_now/1]).

-define(ALARM_EVICT, {alarm,evict}).

%% --------------------------------------------------------------------
%% External exports
-export([start_link_cache/1, start_link_evictor/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state_cache, {ets :: ets:tab()}).
-record(state_evictor, {ets :: ets:tab(),evict_period,evict_after}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link_cache(InstanceName) ->
    SrvName=make_name([InstanceName, cache, srv]),
    gen_server:start_link({local, SrvName}, ?MODULE, {cache,InstanceName}, []).

start_link_evictor(InstanceName) ->
    SrvName=make_name([InstanceName, evictor, srv]),
    gen_server:start_link({local, SrvName}, ?MODULE, {evictor,InstanceName}, []).

get_evict_period(InstanceName) ->
    logmachine_app:get_instance_env(
      InstanceName, 
      evict_period,
      {5, sec}).

get_evict_after(InstanceName) ->
    logmachine_app:get_instance_env(
      InstanceName, 
      evict_after,
      {4, hour}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init({cache,InstanceName}) ->
	ok=logmachine_receiver_srv:subscribe(
	  InstanceName, self(), {gen_server,cast}),
    % 1. Open ETS ordered table
    InstanceEts=ets:new(make_name(InstanceName), [ordered_set, named_table, public, {keypos, 1}]),
    {ok, #state_cache{ets=InstanceEts}};
init({evictor,InstanceName}) ->
    EvictPeriod=get_evict_period(InstanceName),
    send_after(EvictPeriod, ?ALARM_EVICT),
    {ok, #state_evictor{ets=make_name(InstanceName), 
                        evict_period=EvictPeriod,
                        evict_after=get_evict_after(InstanceName)}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    handle_info(Msg, State).

% Cache clauses
handle_info({_,_}=Info, #state_cache{ets=Ets}=State) ->
    ets:insert(Ets, Info),
    {noreply, State};
% Evictor clauses
handle_info(?ALARM_EVICT, #state_evictor{ets=Ets,evict_after=EvictAfter,evict_period=EvictPeriod}=State) ->
    Infimum=millis_to_now(now_to_millis(now()) - to_millis(EvictAfter)),
    do_eviction(Ets,Infimum),
    send_after(EvictPeriod, ?ALARM_EVICT),
    {noreply, State};
% Default clauses
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

do_eviction(Ets, Infimum) ->
    case ets:first(Ets) of
        '$end_of_table' ->
            ok;
        K when K<Infimum ->
            ets:delete(Ets, K),
            do_eviction(Ets, Infimum);
        _ ->
            ok
    end.