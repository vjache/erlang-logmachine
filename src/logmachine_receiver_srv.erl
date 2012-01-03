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
				SubscriberRef :: 
					pid() | 
					port() | 
					atom() | % LocalRegName
					{LocalRegName :: atom(), Node :: node()}|
					{global, GlobalRegName :: term()}, 
				SendMethod :: send_method() ) -> ok.
subscribe(InstanceName, SubscriberRef, SendMethod) ->
	SrvName=make_name([InstanceName, receiver, srv]),
	case gen_server:call(SrvName, {subscribe, SubscriberRef, SendMethod}) of
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
	erlang:process_flag(trap_exit, true),
    {ok, #state{global_alias=allocate_global_name(InstanceName, 10)}}.

handle_call({subscribe, SubscriberRef, SendMethod}, 
			_From, 
			#state{registry=Reg}=State) ->
	case lists:keymember(SubscriberRef, 1, Reg) of
		false -> 
			try
				SendMethodR=resolve_send_method(SendMethod),
				MonRef=monitor(SubscriberRef),
				{reply, ok, 
				 State#state{registry=[{SubscriberRef, SendMethodR, MonRef} | Reg]}}
			catch
				_:ErrReason ->
					{reply, {error, ErrReason}, State}
			end;
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
    {noreply, State#state{registry=notify_subs(Reg, Event)}}.

handle_info({'DOWN', MonitorRef, process, _Object, _Info}, 
			#state{registry=Reg}=State) ->
	Reg1=lists:keydelete(MonitorRef, 3, Reg),
    {noreply, State#state{registry=Reg1}};
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

monitor(Pid) when is_pid(Pid) ->
	link(Pid),
	erlang:monitor(process, Pid);
monitor(RegName) when is_atom(RegName) ->
	Pid=whereis(RegName),
	Pid=/=undefined orelse throw(badarg),
	link(Pid),
	erlang:monitor(process, Pid);
monitor({RegName, Node}) when is_atom(RegName), is_atom(Node) ->
	case rpc:call(Node, erlang, whereis, [RegName]) of
		Pid when is_pid(Pid) ->
			link(Pid),
			erlang:monitor(process, Pid);
		Err -> throw(Err)
	end;
monitor({global, RegName}) ->
	Pid=global:whereis_name(RegName),
	Pid=/=undefined orelse throw(badarg),
	link(Pid),
	erlang:monitor(process, Pid).

notify_subs(Reg, Event) ->
	lists:foldl(
	  fun({Pid, SendMethod, MonRef}, Reg1) ->
			  try SendMethod(Pid, Event),Reg1
			  catch 
				  _:undef ->
					  lists:keydelete(MonRef, 3, Reg1)
			  end
	  end, Reg, Reg).

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
