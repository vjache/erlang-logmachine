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
-module(logmachine_receiver_srv).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, get_global_alias/1, subscribe/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-import(logmachine_util,[make_name/1]).

-type send_method() :: 
		  {Module :: atom(), Function :: atom()} | 
		  {gen_server, cast} |
		  {gen_event, notify} |
		  send |
		  fun((Msg :: term())-> ok ).

-record(state, {global_alias,
				registry = [] :: [{Pid :: pid(),
								   SendMethod :: send_method() }]}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(InstanceName) ->
    SrvName=make_name([InstanceName, receiver, srv]),
    gen_server:start_link(
      {local, SrvName},
      ?MODULE, [InstanceName], []).

-spec subscribe(InstanceName :: atom(), 
				SubscriberPid :: pid(), 
				SendMethod :: send_method() ) -> ok.
subscribe(InstanceName, SubscriberPid, SendMethod) 
  when is_pid(SubscriberPid) ->
	Alias=get_global_alias(InstanceName),
	case gen_server:call({global, Alias}, 
						 {subscribe, SubscriberPid, SendMethod}) of
        {error, Reason} -> throw(Reason);
        R -> R
    end.

get_global_alias(InstanceName) ->
    SrvName=make_name([InstanceName, receiver, srv]),
    case gen_server:call(SrvName, get_global_alias) of
        {error, Reason} -> throw(Reason);
        R -> R
    end.

allocate_global_name(BaseName, MaxAttempts) ->
    allocate_global_name(BaseName, 0, MaxAttempts).
allocate_global_name(BaseName, MaxAttempts, MaxAttempts) ->
    throw({global_aliases_exhausted, BaseName, MaxAttempts});
allocate_global_name(BaseName, N, MaxAttempts) ->
    GlobalAlias={BaseName, N},
    case global:register_name(
           GlobalAlias, self(), fun global:random_notify_name/3) of
        yes -> GlobalAlias;
        no -> allocate_global_name(BaseName, N+1, MaxAttempts)
    end.

%% ====================================================================
%% Server functions
%% ====================================================================

init([InstanceName]) ->
    {ok, #state{global_alias=allocate_global_name(InstanceName, 10)}}.

handle_call({subscribe, SubscriberPid, SendMethod}, 
			_From, 
			#state{registry=Reg}=State) ->
	case lists:keymember(SubscriberPid, 1, Reg) of
		false -> 
			SendMethodR=resolve_send_method(SendMethod),
			{reply, ok, State#state{registry=[{SubscriberPid,SendMethodR}|Reg]}};
		true -> {reply, {error, already_subscribed}, State}
	end;
handle_call(get_global_alias, _From, #state{global_alias=GlbalAlias}=State) ->
    {reply, GlbalAlias, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Info, #state{registry=Reg}=State) ->
    Timestamp=now(),
    Event={Timestamp, Info},
	notify_subs(Reg, Event),
    {noreply, State}.

notify_subs(Reg, Event) ->
	[SendMethod(Pid, Event) || {Pid, SendMethod} <- Reg].

resolve_send_method(send) ->
	fun erlang:send/2;
resolve_send_method({gen_server, cast}) ->
	fun gen_server:cast/2;
resolve_send_method({gen_event, notify}) ->
	fun gen_event:notify/2;
resolve_send_method({Module, Function}) ->
	fun(Pid, Event) ->
			apply(Module, Function, [Pid, Event])
	end.

handle_info({global_name_conflict, _Name}, 
            #state{global_alias={InstanceName,_}}=State) ->
    {noreply, 
     State#state{global_alias=allocate_global_name(InstanceName, 10)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

