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
-module(logmachine_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([make_name/1, to_millis/1, now_to_millis/1,millis_to_now/1, send_after/2, ensure_dir/1]).

%%
%% API Functions
%%

ensure_dir(Dir) ->
    case filelib:ensure_dir(filename:join(Dir, "fake")) of
        ok -> Dir;
        {error, Reason} -> throw(Reason)
    end.

make_name(NameComponent) when is_atom(NameComponent) ->
    NameComponent;
make_name([H|_]=NameComponents) when is_atom(H) ->
    list_to_atom(lists:flatten(([io_lib:format(".~p", [Comp]) || Comp <- NameComponents])));
make_name(String) when is_list(String) ->
    list_to_atom(String).

now_to_millis({MegaS,S,MicroS}) ->
    MegaS*1000*1000*1000 + S*1000 + MicroS div 1000.

millis_to_now(Millis) ->
    {Millis div 1000000000,
     (Millis rem 1000000000) div 1000,
     (Millis rem 1000)*1000}.

to_millis({N,sec}) ->
   timer:seconds(N);
to_millis({N,min}) ->
   timer:minutes(N);
to_millis({N,hour}) ->
   timer:hours(N);
to_millis({N,day}) ->
   N*timer:hours(24);
to_millis({N,hms}) ->
   timer:hms(N).

send_after(Period, Message) ->
    {ok,_}=timer:send_after(to_millis(Period), Message).

%%
%% Local Functions
%%

