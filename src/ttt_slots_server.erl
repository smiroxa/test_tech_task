%%%-------------------------------------------------------------------
%%% @author smiroshnik
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Май 2017 15:39
%%%-------------------------------------------------------------------
-module(ttt_slots_server).
-author("smiroshnik").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([request_slot/1, take_slot/1, release_slot/1, list_slots/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("slots_helper.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

request_slot(UserId) ->
  gen_server:call(?MODULE, {get_slot, UserId}).

take_slot(Slot) ->
  gen_server:call(?MODULE, {take_slot, Slot}).

release_slot(Slot) ->
  gen_server:call(?MODULE, {release_slot, Slot}).

list_slots() ->
  {ok, [Slot || {slot, _UserId, Slot, _TTL, ?STATUS_TAKEN} <- ets:tab2list(?MODULE)]}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  ets:new(?MODULE, [set, public, named_table, {keypos, 2}]),
  {ok, HashFunction} = application:get_env(test_tech_task, hash_function),
  {ok, #state{hash_function = HashFunction}}.

handle_call({get_slot, UserId}, _From, State = #state{hash_function = Hash}) ->
  Result = do_get_slot(ets:lookup(?MODULE, UserId), Hash, UserId),
  {reply, Result, State};
handle_call({take_slot, Slot}, _From, State) ->
  Result = do_take_slot(ets:lookup(?MODULE, Slot)),
  {reply, Result, State};
handle_call({release_slot, Slot}, _From, State) ->
  Result = do_release_slot(ets:lookup(?MODULE, Slot)),
  {reply, Result, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
do_get_slot([], Hash, UserId) ->
  Slot = create_slot(Hash, UserId),
  NewSlot = #slot{user_id = UserId, slot = Slot, ttl = now_sec()+?TTL_S, status = ?STATUS_CREATED},
  SlotLinkUser = #slot_link_user{slot = Slot, user_id = UserId},
  true = ets:insert(?MODULE, [NewSlot, SlotLinkUser]),
  {ok, NewSlot#slot.slot};
do_get_slot(_Slot, _Hash, _UserId) -> {error, user_already_have_slot}.

%% @private
do_take_slot([#slot_link_user{user_id = UserId}]) ->
  [RecSlot] = ets:lookup(?MODULE, UserId),
  take_slot_if_possible(RecSlot, now_sec());
do_take_slot(_) -> {error, slot_not_found}.

%% @private
do_release_slot([#slot_link_user{user_id = UserId}]) ->
  [RecSlot] = ets:lookup(?MODULE, UserId),
  release_slot_if_possible(RecSlot);
do_release_slot(_) -> {error, slot_not_found}.

%% @private
take_slot_if_possible(Slot, Now) when Slot#slot.ttl > Now, Slot#slot.status =:= ?STATUS_CREATED ->
  true = ets:update_element(?MODULE, Slot#slot.user_id, {#slot.status, ?STATUS_TAKEN}), ok;
take_slot_if_possible(Slot, Now) when Slot#slot.ttl =< Now -> {error, slot_ttl_expired};
take_slot_if_possible(#slot{status = Status}, _Now) when Status =:= ?STATUS_TAKEN -> {error, slot_taken};
take_slot_if_possible(#slot{status = Status}, _Now) when Status =:= ?STATUS_RELEASED -> {error, slot_released};
take_slot_if_possible(_Slot, _Now) -> {error, unknow_error}.

release_slot_if_possible(Slot) when Slot#slot.status =:= ?STATUS_TAKEN ->
  true = ets:update_element(?MODULE, Slot#slot.user_id, {#slot.status, ?STATUS_RELEASED}), ok;
release_slot_if_possible(Slot) when Slot#slot.status =:= ?STATUS_CREATED -> {error, slot_was_not_taken};
release_slot_if_possible(Slot) when Slot#slot.status =:= ?STATUS_RELEASED -> {error, slot_already_released};
release_slot_if_possible(_Slot) -> {error, unknow_error}.

%% @private
create_slot(Hash, UserId) ->
  StrRandomInt = integer_to_list(crypto:rand_uniform(1, 1000000)),
  binary_to_list(crypto:hash(Hash, UserId)) ++ "/" ++ binary_to_list(crypto:hash(Hash, StrRandomInt)).

%% @private
now_sec() ->
  {Mega, Sec, _Micro} = os:timestamp(),
  Mega*1000000+Sec.