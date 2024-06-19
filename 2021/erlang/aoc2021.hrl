-define(IN, "priv/"++?MODULE_STRING++".in").
-define(TEST_IN, "priv/"++?MODULE_STRING++"_test.in").
-define(BIGBOI_IN, "priv/"++?MODULE_STRING++"_bigboi.in").
-type uint() :: non_neg_integer().

-spec ints_sum(Goal :: integer()) -> integer().
ints_sum(Goal) ->
    Goal*(Goal+1) div 2.
