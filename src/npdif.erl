%%%
%%% npdif.erl
%%%
%%% Copyright 2017 by Anthony Howe.  All rights reserved.
%%%
%%% MIT License.
%%%
%%% Ported from http://www.ioccc.org/2015/howe/spoiler/prog.c
%%%
%%% Wu, Manber, Myers, and Miller; August 1989;
%%% "An O(NP) Sequence Comparison Algorithm";
%%% <http://myerslab.mpi-cbg.de/wp-content/uploads/2014/06/np_diff.pdf>
%%%

-module(npdif).
-export([compute/2]).

compute(A, B) when is_list(A) ->
	compute(array:from_list(A), B);
compute(A, B) when is_list(B) ->
	compute(A, array:from_list(B));
compute(A, B) ->
	case array:size(A) > array:size(B) of
	true ->
		% Swap A and B if N < M.  The edit distance will be the
		% same, but edit script operations will be reversed.
%io:format("swap A & B~n"),
		compute(B, A, true);
	false ->
		compute(A, B, false)
	end.

compute(A, B, Invert) ->
	% Delta = N - M where A[1..M], B[1..N], and N >= M.
	Delta = array:size(B) - array:size(A),

	% From -(M+1).. 0 .. (N+1); up & lower sentinels and zero.
	Diag = arr:new(array:size(A)+array:size(B)+3, array:size(A)+1, {-1, []}),

%io:format("delta=~w M=~w N=~w fp.len=~w~n", [Delta, array:size(A), array:size(B), arr:size(Diag)]),
	compute(A, B, Diag, Delta, 0).

compute(A, B, Diag, Delta, P) ->
	% Snake diagonals of the p-band edges towards delta middle.
	Diag1 = pband(A, B, Diag,  Delta, -P),
	Diag2 = pband(A, B, Diag1, Delta, Delta+P),
	% Snake delta after the lo and hi bands worked in from edges.
	Diag3 = snake(A, B, Diag2, Delta),

	{Y, Script} = arr:get(Delta, Diag3),
	case  Y == array:size(B) of
	false ->
		compute(A, B, Diag3, Delta, P+1);
	true ->
		{Delta + 2 * P, lists:reverse(Script)}
	end.


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
%io:format("snake ~w ~w~n", [K, Diag]),
	{H, _} = arr:get(K-1, Diag),
	{V, _} = arr:get(K+1, Diag),
	Y = max(H+1, V),

	% Diagonal k = y - x of matrix [-(M+1), (N-1)], where
	% row 0 and column 0 are initialised with sentenial -1.
	%
	% k-diagonal	= A and B match
	% y-horizontal	= insert from B
	% x-vertical	= delete from A
	%
	X = Y - K,
%io:format("snake in K=~w Y=~w X=~w~n", [K, Y, X]),

	Vertex = {slide(X, Y, A, B), []},
%io:format("snake out K=~w Y=~w X=~w~n", [K, Vertex, X]),
	arr:set(K, Vertex, Diag).

slide(X, Y, A, B) ->
	% Algorithm assumes 1-based indexing, A[1..M] and B[1..N], but
	% we have 0-based arrays. NOTE no hash collision checking.
	%
	case
		X < array:size(A) andalso Y < array:size(B)
		andalso array:get(X, A) == array:get(Y, B)
	of
	true ->
		% Slide along diagonal.
		slide(X+1, Y+1, A, B);
	false ->
		Y
	end.

