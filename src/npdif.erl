%%%
%%% npdif.erl
%%%
%%% Copyright 2017 by Anthony Howe.  All rights reserved.
%%%
%%% MIT License.
%%%
%%% Wu, Manber, Myers, and Miller; August 1989;
%%% "An O(NP) Sequence Comparison Algorithm";
%%% <http://myerslab.mpi-cbg.de/wp-content/uploads/2014/06/np_diff.pdf>
%%%

-module(npdif).
-export([compute/2]).

-ifdef(EUNIT).
-compile(export_all).
-endif.

% Convert list of #{seek, hash} to array.
compute(A, B) when is_list(A) andalso is_map(hd(A)) ->
	compute(array:from_list(A), B);
compute(A, B) when is_list(B) andalso is_map(hd(B)) ->
	% Assume tuple is {Position, Integer}.
	compute(A, array:from_list(B));

% Convert simple list into 1-based #{seek, hash} list.
compute(A, B) when is_list(A) ->
	compute(count_off([" "|A]), B);
compute(A, B) when is_list(B) ->
	compute(A, count_off([" "|B]));

compute(A, B) ->
	case {array:get(0, A), array:get(0, B)} of
	{#{seek := 0}, #{seek := 0}} ->
		ok;
	{_, _} ->
		error({badarg, "expect 1-based arrays"})
	end,

	case array:size(A) > array:size(B) of
	true ->
		% Swap A and B if N < M.  The edit distance will be the
		% same, but edit script operations will be reversed.
		{Dist, Script} = compute(B, A, true),
		{Dist, invert_script(Script)};
	false ->
		{Dist, Script} = compute(A, B, false),
		{Dist, lists:reverse(Script)}
	end.

compute(A, B, _Invert) ->
	% Delta = N - M where A[1..M], B[1..N], and N >= M.
	Delta = array:size(B) - array:size(A),

	% From -(M+1).. 0 .. (N+1); up & lower sentinels and zero.
	Diag = arr:new(array:size(A) + array:size(B) + 1, array:size(A), {-1, []}),

%io:format("delta=~w M=~w N=~w fp.len=~w~n", [Delta, array:size(A), array:size(B), arr:size(Diag)]),
	compute(A, B, Diag, Delta, 0).

compute(A, B, Diag, Delta, P) ->
	% Snake diagonals of the p-band edges towards delta middle.
	Diag1 = pband(A, B, Diag,  Delta, -P),
	Diag2 = pband(A, B, Diag1, Delta, Delta+P),
	% Snake delta after the lo and hi bands worked in from edges.
	Diag3 = snake(A, B, Diag2, Delta),

	{Y, Script} = arr:get(Delta, Diag3),
	case  Y == array:size(B)-1 of
	false ->
		compute(A, B, Diag3, Delta, P+1);
	true ->
		{Delta + 2 * P, Script}
	end.

count_off(List) ->
	count_off(List, 0, []).
count_off([], _, Acc) ->
	lists:reverse(Acc);
count_off([H | T], Index, Acc) ->
	count_off(T, Index+1, [#{seek => Index, hash => H} | Acc]).

invert_script(Script) ->
	invert_script(Script, []).
invert_script([], Acc) ->
	Acc;
invert_script([{Op, Aline, Bline} | Rest], Acc) ->
	invert_script(Rest, [{not Op, Bline, Aline} | Acc]).

% P-band Middle
pband(_A, _B, Diag, Delta, Delta) ->
	Diag;

% P-band Lo
%
% for (k = -p; k < delta; k++)
%	diag[k] = snake(k);
pband(A, B, Diag, Delta, K) when K < Delta ->
	Diag1 = snake(A, B, Diag, K),
%io:format("1st fp[~w]=~w~n", [K, {Y, _} = arr:get(K, Diag1)]),
	pband(A, B, Diag1, Delta, K+1);

% P-band Hi
%
% for (k = delta + p; delta < k; k--)
%	diag[k] = snake(k);
pband(A, B, Diag, Delta, K) when Delta < K ->
	Diag1 = snake(A, B, Diag, K),
%io:format("2nd fp[~w]=~w~n", [K, {Y, _} = arr:get(K, Diag1)]),
	pband(A, B, Diag1, Delta, K-1).

snake(A, B, Diag, K) ->
	{H, Hedit} = arr:get(K-1, Diag),
	{V, Vedit} = arr:get(K+1, Diag),

	% Y = max(H+1, V),
	case V < H+1 of
	true ->
		% Insert
		Op = true,
		Y = H+1,
		Prev = Hedit;
	false ->
		% Delete
		Op = false,
		Y = V,
		Prev = Vedit
	end,

	% Diagonal k = y - x of matrix [-(M+1), (N-1)], where
	% row 0 and column 0 are initialised with sentenial -1.
	%
	% k-diagonal	= A and B match
	% y-horizontal	= insert from B
	% x-vertical	= delete from A
	%
	X = Y - K,
%io:format("snake in K=~w Y=~w X=~w~n", [K, Y, X]),

	Edit = case 0 < Y orelse 0 < X of
	true ->
		Aline = {X, maps:get(seek, array:get(X, A))},
		Bline = {Y, maps:get(seek, array:get(Y, B))},
		[{Op, Aline, Bline} | Prev];
	false ->
		[]
	end,

	Vertex = {slide(X, Y, A, B), Edit},
%io:format("snake out K=~w Y=~w X=~w~n", [K, Vertex, X]),
	arr:set(K, Vertex, Diag).

slide(X, Y, A, B) ->
	% Algorithm assumes 1-based indexing, A[1..M] and B[1..N].
	% There is no hash collision checking.
	case
		X+1 < array:size(A) andalso Y+1 < array:size(B)
		andalso maps:get(hash, array:get(X+1, A)) == maps:get(hash, array:get(Y+1, B))	of
	true ->
		% Slide along diagonal.
		slide(X+1, Y+1, A, B);
	false ->
		Y
	end.

