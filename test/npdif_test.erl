-module(npdif_test).
-include_lib("eunit/include/eunit.hrl").

count_off_test_() ->
	[
	?_assertMatch([], npdif:count_off([])),
	?_assertMatch([#{seek := 0, hash := $1}], npdif:count_off("1")),
	?_assertMatch([#{seek := 0, hash := $1},#{seek := 1, hash := $2}], npdif:count_off("12")),
	?_assertMatch([#{seek := 0, hash := $A},#{seek := 1, hash := $B},#{seek := 2, hash := $C}], npdif:count_off("ABC"))
	].

invert_script_test_() ->
	[
	?_assertMatch([], npdif:invert_script([])),
	?_assertMatch([{true, b, a}], npdif:invert_script([{false, a, b}])),
	?_assertMatch([{false, d, c},{true, b, a}], npdif:invert_script([{false, a, b}, {true, c, d}]))
	].

compute_test_() ->
	[
	?_assertMatch({0, _}, npdif:compute("1", "1")),
	?_assertMatch({2, _}, npdif:compute("1", "A")),
	?_assertMatch({8, _}, npdif:compute("123", "ABCDE")),
	?_assertMatch({8, _}, npdif:compute("ABCDE", "123")),
	?_assertMatch({1, _}, npdif:compute("ABD", "ABCD")),
	?_assertMatch({1, _}, npdif:compute("ABCD", "ABD")),
	?_assertMatch({4, _}, npdif:compute("ABCD", "ACDBECFD")),
	?_assertMatch({6, _}, npdif:compute("ABCDEF", "ABXYEFCD")),
	?_assertMatch({6, _}, npdif:compute("ABCDEFGHIJK", "ABCEFGIJKDEFGHIJK")),
	?_assertMatch({5, _}, npdif:compute("ABCABBA", "CBABAC")),
	?_assertMatch({6, _}, npdif:compute("ACEBDABBABED", "ACBDEACBED"))
	].
