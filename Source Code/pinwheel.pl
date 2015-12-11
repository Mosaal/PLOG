:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(random)).

% Aux. Variables
:- dynamic randomSolution/1.
randomSolution(0).

% Interface
display_row([]).
display_row([H|T]) :-
	(H < 10 -> format('|  ~w  ', [H]), !;
	(H > 9, H < 99 -> format('| ~w  ', [H]), !;
	(H > 99, H < 999 -> format('| ~w ', [H]), !))),
	display_row(T).

display_board([]).
display_board([H|T]) :-
	display_row(H), write('|'), nl,
	display_board(T).

reset_timer :- statistics(walltime, _).
print_time :-
	statistics(walltime, [_, T]),
	TS is ((T // 10) * 10) / 1000,
	nl, format('Time: ~ws', [TS]), nl.

% Logic
create_matrix(0, _, _).
create_matrix(Rows, Columns, Matrix) :-
	NewRows is Rows - 1,
	length(InsideRow, Columns),
	create_matrix(NewRows, Columns, NewRow),
	append([InsideRow], NewRow, Matrix).

apply_restriction_by_row([], _).
apply_restriction_by_row([H|T], Sum) :-
	domain(H, 1, Sum),
	apply_restriction_by_row(T, Sum).

apply_restriction_by_column([], _).
apply_restriction_by_column([H|T], Sum) :-
	sum(H, #=, Sum),
	apply_restriction_by_column(T, Sum).

generate_random_solution(Rows, Columns, Sum) :-
	Temp is Rows * Columns * Sum,
	% Temp is Rows * Columns * Sum,
	random(1, Temp, RandomValue),
	retract(randomSolution(_)),
	assert(randomSolution(RandomValue)).

% Starting function
main(Rows, Columns, Sum, List) :-
	(Sum < Rows -> write('The Sum must be greater or equal to the number of Rows!'), fail);
	% Max is Sum - (Rows - 1),
	create_matrix(Rows, Columns, Matrix),
	transpose(Matrix, TransposedMatrix),
	apply_restriction_by_row(Matrix, Sum),
	append(Matrix, List),
	% random_permutation(List, RandomList),
	apply_restriction_by_column(TransposedMatrix, Sum),
	% generate_random_solution(Rows, Columns, Sum),
	reset_timer,
	labeling([], List),
	% randomSolution(V),
	% (Sum = Rows -> display_board(Matrix);
	% (V = 0 -> display_board(Matrix);
	% (V \= 0 -> randomSolution(N), N1 is N - 1,
	%		   retract(randomSolution(_)),
	%		   assert(randomSolution(N1)),
	%		   fail))),
	display_board(Matrix),
	print_time,
	fd_statistics.