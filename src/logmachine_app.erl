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
-module(logmachine_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([get_env/1, get_env/2, get_instance_env/2, get_instance_env/3]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------


%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
    case logmachine_sup:start_link_main() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

get_env(Env) ->
    get_env(Env, fun()-> throw({noconf, Env}) end).
get_env(Env, Default) ->
    case application:get_env(logmachine, Env) of
        undefined ->
            if is_function(Default,0) -> Default();
               true -> Default
            end;
        {ok,Val} ->
            Val
    end.

get_instance_env(InstanceName, Env) ->
    InstanceProps=get_instance_props(InstanceName),
    case proplists:get_value(Env,InstanceProps) of
        undefined -> get_env(Env);
        Value -> Value
    end.

get_instance_env(InstanceName, Env, Default) ->
    InstanceProps=get_instance_props(InstanceName),
    case proplists:get_value(Env,InstanceProps) of
        undefined -> get_env(Env, Default);
        Value -> Value
    end.

get_instance_props(InstanceName) ->
    case proplists:get_value(InstanceName, get_env(instances)) of
        undefined -> throw({noconf_instance, InstanceName});
        [{_,_}|_]=Props ->
            Props;
        BadProps ->
            throw({badconf_instance, InstanceName, BadProps})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

