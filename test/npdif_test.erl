-module(npdif_test).
-include_lib("eunit/include/eunit.hrl").

npdiff_test_() ->
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
