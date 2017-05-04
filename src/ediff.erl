%%#!/usr/bin/env escript

%%%
%%% Erlang diff(1)
%%%
%%% Copyright 2017 by Anthony Howe.  All rights reserved.
%%%
%%% MIT style license.
%%%
%%% Ported from http://www.ioccc.org/2015/howe/spoiler/prog.c
%%%
%%% Wu, Manber, Myers, and Miller; August 1989;
%%% "An O(NP) Sequence Comparison Algorithm";
%%% <http://myerslab.mpi-cbg.de/wp-content/uploads/2014/06/np_diff.pdf>
%%%

-module(ediff).
-export([main/1, diff/2, diff/3]).

-ifdef(EUNIT).
-compile(export_all).
-endif.

-define(BUFSIZ, (8*1024)).
-define(INIT_FILE_SIZE, 5).

usage() ->
	io:format("usage: ediff [-d] file1 file2~n"),
	io:format("-d\t\twrite edit distance~n"),
	halt(2).

main(Args) ->
	case egetopt:parse(Args, [
		{ $d, flag, write_distance }
	]) of
	{ok, Options, ArgsN} ->
		process(Options, ArgsN);
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt]),
		usage()
	end.

process(_Opts, Files) when length(Files) /= 2 ->
	usage();
process(Opts, [File1, File2]) ->
	try
		process(Opts, File1, File2)
	catch
		throw:{error, File, Reason} ->
			io:format(standard_error, "ediff: ~s: ~s~n", [File, str:error(Reason)]),
			halt(1)
	end.

process(Opts, File1, File2) ->
	Fp1 = open_file(File1),
	Fp2 = open_file(File2),
	case {Fp1, Fp2} of
	{standard_io, standard_io} ->
		throw({error, "-", 'only one file argument can refer to standard input'});
	_ ->
		diff(Fp1, Fp2, Opts)
	end,
	file:close(Fp1),
	file:close(Fp2).

open_file("-") ->
        io:setopts(standard_io, [binary]),
	standard_io;
open_file(File) ->
	case file:open(File, [read, binary, {read_ahead, ?BUFSIZ}]) of
	{error, Reason} ->
		throw({error, File, Reason});
	{ok, Fp} ->
		Fp
	end.

diff(Fp1, Fp2) ->
	diff(Fp1, Fp2, []).
diff(Fp1, Fp2, Opts) ->
	{Distance, Script} = edit_distance(hash_file(Fp1), hash_file(Fp2)),
	case proplists:get_value(write_distance, Opts, false) of
	true ->
		io:format("~B~n", [Distance]);
	false ->
		dump_script(Script)
	end.

hash_file(Fp) ->
	% Building list faster than an array when reading a file.  Add
	% dummy index-0 element for 1-based array indexing later.
	Hashes = hash_file(Fp, [{0, 0}]),
	file:position(Fp, 0),
	% Once built, convert list to array for faster lookups.
	array:from_list(lists:reverse(Hashes)).
hash_file(Fp, Lines) ->
	case file:read_line(Fp) of
	{ok, Line} ->
		{ok, Position} = file:postion(Fp, cur),
		hash_file(Fp, [{fnv:hash56(Line), Position} | Lines]);
	eof ->
		Lines;
	Error ->
		throw(Error)
	end.

edit_distance(A, B) when is_list(A) ->
	edit_distance(array:from_list(A), B);
edit_distance(A, B) when is_list(B) ->
	edit_distance(A, array:from_list(B));
edit_distance(A, B) ->
	case array:size(A) > array:size(B) of
	true ->
		% Swap A and B if N < M.  The edit distance will be the
		% same, but edit script operations will be reversed.
		edit_distance(B, A, true);
	false ->
		edit_distance(A, B, false)
	end.

edit_distance(A, B, Invert) ->
	% Delta = N - M where A[1..M], B[1..N], and N >= M.
	Delta = array:size(B) - array:size(A),

	% From -(M+1).. 0 .. (N+1); up & lower sentinels and zero.
	Diag = arr:new(array:size(A) + array:size(B) + 3, array:size(A), {-1, []}),

	edit_distance(A, B, Diag, Delta, 0).

edit_distance(A, B, Diag, Delta, P) ->
	{Y, _} = arr:get(Delta, Diag),
	case  Y == array:size(B)-1 of
	false ->
		% Snake diagonals of the p-band edges towards delta middle.
		Diag1 = pband(A, B, Diag,  Delta, -P),
		Diag2 = pband(A, B, Diag1, Delta, Delta+P),
		% Snake delta after the lo and hi bands worked in from edges.
		Diag3 = snake(A, B, Diag2, Delta),
		edit_distance(A, B, Diag3, Delta, P+1);
	true ->
		{Y, Script} = arr:get(Delta, Diag),
		{Delta + 2 * (P - 1), lists:reverse(Script)}
	end.

% P-band Lo
%
% for (k = -p; k < delta; k++)
%	diag[k] = snake(k);
pband(A, B, Diag, Delta, K) when K < Delta ->
	Diag1 = snake(A, B, Diag, K),
	pband(A, B, Diag1, Delta, K+1);

% P-band Hi
%
% for (k = delta + p; delta < k; k--)
%	diag[k] = snake(k);
pband(A, B, Diag, Delta, K) when Delta < K ->
	Diag1 = snake(A, B, Diag, K),
	pband(A, B, Diag1, Delta, K+1);

% P-band end of loop.
pband(_A, _B, Diag, Delta, Delta) ->
	Diag.

snake(A, B, Diag, K) ->
	{H, _} = arr:get(K-1, Diag) + 1,
	{V, _} = arr:get(K+1, Diag),
	Y = max(H, V),

	% Diagonal k = y - x of matrix [-(M+1), (N-1)], where
	% row 0 and column 0 are initialised with sentenial -1.
	%
	% k-diagonal	= A and B match
	% y-horizontal	= insert from B
	% x-vertical	= delete from A
	%
	X = Y - K,

	arr:set(K, slide(X, Y, A, B), Diag).

slide(X, Y, A, B) ->
	% Algorithm assumes 1-based indexing, A[1..M] and B[1..N].
	% NOTE no hash collision checking yet.
	%
	case
		X < array:size(A)-1 andalso Y < array:size(B)-1
		andalso array:get(X+1, A) == array:get(Y+1, B)
	of
	true ->
		% Slide along diagonal.
		snake(X+1, Y+1, A, B);
	false ->
		Y
	end.

dump_script(Script) ->
	io:format("~w~n", [Script]).
