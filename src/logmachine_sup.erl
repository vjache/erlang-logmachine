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
-module(logmachine_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link_main/0, start_link_instance/1,start_link_component/2]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

start_link_main() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, root).

start_link_instance(InstanceName) ->
    SupName=logmachine_util:make_name([InstanceName,sup]),
    supervisor:start_link({local, SupName}, ?MODULE, {instance, InstanceName}).

start_link_component(ComponentName, InstanceName) ->
    SupName=logmachine_util:make_name([InstanceName,ComponentName,sup]),
    supervisor:start_link({local, SupName}, ?MODULE, {component, ComponentName, InstanceName}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
% Root supervisor
init(root) ->
    Instances=logmachine_app:get_env(instances,[]),
    InstancesSups= 
        [{InstanceName,
          {?MODULE,start_link_instance,[InstanceName]},
          permanent, infinity, supervisor,[?MODULE]} || InstanceName <- proplists:get_keys(Instances)],
    {ok,{{one_for_one,0,1}, InstancesSups}};
% Instance supervisor
init({instance, InstanceName}) ->
    Components=[recorder, cache], % logmachine_receiver_srv
    ComponentsSups=
        [{Component,
          {?MODULE,start_link_component,[Component,InstanceName]},
          permanent, infinity, supervisor, [?MODULE]}|| Component <- Components],
    Receiver={logmachine_receiver_srv,
              {logmachine_receiver_srv,start_link,[InstanceName]},
              permanent, 10000, worker, [logmachine_receiver_srv]},
    Locator={logmachine_emlocator_srv,
             {logmachine_emlocator_srv,start_link,[InstanceName]},
             permanent, 10000, worker, [logmachine_emlocator_srv]},
    {ok,{{one_for_one,0,1}, [Receiver | ComponentsSups] ++ [Locator]}};
% Recorder supervisor
init({component, recorder, InstanceName}) ->
    Mod=logmachine_recorder_srv,
    Recorder={logmachine_recorder,
              {Mod,start_link_recorder,[InstanceName]},
              permanent, 10000, worker, [Mod]},
    Archiver={'logmachine_recorder/archiver',
              {Mod,start_link_archiver,[InstanceName]},
              permanent, 10000, worker, [Mod]},
    {ok,{{one_for_one,0,1}, [Recorder, Archiver]}};
% Cache supervisor
init({component, cache, InstanceName}) ->
    Mod=logmachine_cache_srv,
    Cache={logmachine_cache,
              {Mod,start_link_cache,[InstanceName]},
              permanent, 10000, worker, [Mod]},
    Evictor={'logmachine_cache/evictor',
              {Mod,start_link_evictor,[InstanceName]},
              permanent, 10000, worker, [Mod]},
    {ok,{{one_for_one,0,1}, [Cache, Evictor]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

