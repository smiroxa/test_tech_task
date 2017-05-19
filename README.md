# test_tech_task
Для компиляции и запуска common тестов:

make && ct_run -suite ./test/ttt_slots_server_test_SUITE.erl -pa ./ebin/ -pa ./deps/*/ebin/ -logdir ./logs/ -erl_args -config путь к файлу /rel/sys.config