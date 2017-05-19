%%%-------------------------------------------------------------------
%%% @author smiroshnik
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Май 2017 10:24
%%%-------------------------------------------------------------------
-author("smiroshnik").


-record(state, {
    hash_function :: atom()
}).
-record(slot, {
  user_id :: binary(),
  slot    :: string(),
  ttl     :: integer(),
  status  :: binary()
}).
-record(slot_link_user, {
  slot    :: string(),
  user_id :: binary()
}).

-define(STATUS_CREATED,  <<"created">>).
-define(STATUS_TAKEN,    <<"taken">>).
-define(STATUS_RELEASED, <<"realesed">>).

-define(TTL_S, 5).