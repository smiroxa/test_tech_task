-module(ttt_slots_server_test_SUITE).
-author("smiroshnik").

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(TEST_USER_CREATE_SOME_SLOTS,  "user_1").
-define(TEST_USER_TAKE_SOME_SLOTS,    "user_2").
-define(TEST_USER_TAKE_EXPIRED_SLOT,  "user_3").
-define(TEST_USER_TAKE_RELEASED_SLOT, "user_4").
-define(TEST_USER_LIST_SLOTS,         "user_5").

-export([
  suite/0,
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  groups/0,
  init_per_group/2,
  end_per_group/2,
  init_per_testcase/2,
  end_per_testcase/2
]).
-export([
  create_some_slots_test/1,
  take_some_slots_test/1,
  take_expired_slot_test/1,
  take_released_port_test/1,
  get_list_taken_slots/1
]).


suite() -> [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  application:ensure_all_started(test_tech_task),
  timer:sleep(1000),
  Config.

end_per_suite(Config) -> Config.

init_per_group(_GroupName, Config) -> Config.

end_per_group(_GroupName, _Config) -> ok.

init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

groups() ->
  [
    {
      common_group,
      [sequence],
      [
        create_some_slots_test,
        take_some_slots_test,
        take_expired_slot_test,
        take_released_port_test,
        get_list_taken_slots
      ]
    }].

all() -> [{group, common_group}].



%% пользователь может иметь только один незанятый слот. Повторный запрос слота – ошибка
create_some_slots_test(Config) ->
  {ok, _Slot} = ttt_slots_server:request_slot(?TEST_USER_CREATE_SOME_SLOTS),
  ?assertEqual({error, user_already_have_slot}, ttt_slots_server:request_slot(?TEST_USER_CREATE_SOME_SLOTS)),
  Config.

%% пользователь может занять только незанятый слот. Повторное занятие слота – ошибка
take_some_slots_test(Config) ->
  {ok, Slot} = ttt_slots_server:request_slot(?TEST_USER_TAKE_SOME_SLOTS),
  ?assertEqual(ok, ttt_slots_server:take_slot(Slot)),
  ?assertEqual({error, slot_taken}, ttt_slots_server:take_slot(Slot)),
  Config.

%% незанятый слот должен иметь время жизни. По истечению времени жизни незанятый слот недоступен для занятия
take_expired_slot_test(Config) ->
  {ok, Slot} = ttt_slots_server:request_slot(?TEST_USER_TAKE_EXPIRED_SLOT),
  timer:sleep(6000),
  ?assertEqual({error, slot_ttl_expired}, ttt_slots_server:take_slot(Slot)),
  Config.

%% после освобождения слот не может быть занят повторно
take_released_port_test(Config) ->
  {ok, Slot} = ttt_slots_server:request_slot(?TEST_USER_TAKE_RELEASED_SLOT),
  ?assertEqual(ok, ttt_slots_server:take_slot(Slot)),
  ?assertEqual(ok, ttt_slots_server:release_slot(Slot)),
  ?assertEqual({error, slot_released}, ttt_slots_server:take_slot(Slot)),
  Config.

get_list_taken_slots(Config) ->
  ets:delete_all_objects(ttt_slots_server),
  {ok, Slot} = ttt_slots_server:request_slot(?TEST_USER_LIST_SLOTS),
  ?assertEqual(ok, ttt_slots_server:take_slot(Slot)),
  ?assertEqual({ok, [Slot]}, ttt_slots_server:list_slots()),
  Config.