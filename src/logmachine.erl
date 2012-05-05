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
-module(logmachine).

%%===============================================================================
%% Include files
%%===============================================================================
-include("log.hrl").
-include("types.hrl").

%%===============================================================================
%% Exported Functions
%%===============================================================================
-export([get_zlist/2,
         subscribe/2,
         subscribe/3,
         subscribe/4,
		 subscribe/5,
         info/1,
         info/2]).

-export([start_err_sim/0]).

%%===============================================================================
%% API Functions
%%===============================================================================

info(InstanceName) ->
    [{data_dir, logmachine_recorder_srv:get_data_dir(InstanceName)},
     {arch_data_dir, logmachine_recorder_srv:get_arch_data_dir(InstanceName)}].

info(InstanceName, data_dir) ->
    logmachine_recorder_srv:get_data_dir(InstanceName);
info(InstanceName, arch_data_dir) ->
     logmachine_recorder_srv:get_arch_data_dir(InstanceName);
info(_InstanceName, Prop) ->
    throw({noprop, Prop}).

%%-------------------------------------------------------------------------------
%% @doc
%%  Subscribes a caller process on events starting from specified time instant.
%% @end
%%-------------------------------------------------------------------------------
-spec subscribe(InstanceName :: atom(), 
                FromTimestamp :: timestamp()) -> SubscriptionSession :: pid().
subscribe(InstanceName, FromTimestamp) ->
    subscribe(InstanceName, FromTimestamp, self()).

%%-------------------------------------------------------------------------------
%% @doc
%%  Subscribes a specified process on events starting from specified time instant.
%% @end
%%-------------------------------------------------------------------------------
-spec subscribe(InstanceName :: atom(), 
                FromTimestamp :: timestamp(),
                SubscriberPid :: pid()) -> SubscriptionSession :: pid().
subscribe(InstanceName, FromTimestamp, SubPid) ->
    subscribe(InstanceName, FromTimestamp, SubPid, InstanceName).

subscribe(InstanceName, FromTimestamp, SubPid, Marker) ->
    subscribe(InstanceName, FromTimestamp, SubPid, Marker, [{'$1',[],['$1']}]).

-spec subscribe(InstanceName :: atom(), 
                FromTimestamp :: timestamp(),
                SubscriberPid :: pid(),
                Marker :: term(), 
                MatchSpec :: ets:match_spec()) -> SubscriptionSession :: pid().
subscribe(InstanceName, FromTimestamp, SubPid, Marker, MatchSpec) -> 
    logmachine_sup:start_sub_child(InstanceName, FromTimestamp, SubPid, Marker, MatchSpec).

%%-------------------------------------------------------------------------------
%% @doc
%%  Returns a Z-List of history entries starting from specified time instant.
%% @end
%%-------------------------------------------------------------------------------
-spec get_zlist(InstanceName :: atom(), 
                FromTimestamp :: timestamp()) -> zlists:zlist(history_entry()).
get_zlist(InstanceName, FromTimestamp) ->
    case logmachine_cache_srv:get_interval(InstanceName) of
        % If fits into RAM
        {From, _To} when From =< FromTimestamp ->
            logmachine_cache_srv:get_history(InstanceName, FromTimestamp);
        % Otherwise read from disk log
        _ -> 
            zlists:entail(
              logmachine_recorder_srv:get_history(InstanceName, FromTimestamp),
              fun({T,_}=E) ->
                      % Get zlist (probably form RAM cache)
                      ZList=get_zlist(InstanceName, T),
                      % Exclude last element
                      zlists:dropwhile(fun(E1) -> E==E1 end, ZList)
              end)
    end.

%%-------------------------------------------------------------------------------
%% @doc
%%  Starts a process that periodically (once a sec) reports errors.
%% @end
%%-------------------------------------------------------------------------------
start_err_sim() ->
    spawn(
      fun()->
              err_sim_loop(0)
      end).

%%===============================================================================
%% Local Functions
%%===============================================================================
err_sim_loop(N) ->
    error_logger:error_report({test, N}),
    timer:sleep(1000),
    err_sim_loop(N+1).

