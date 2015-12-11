:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(samsort)).

% Boards
smallMatrix([[5, 5, 4, 6, 4, 6, 6, 4, 5],
			 [7, 5, 7, 8, 4, 5, 3, 2, 6],
			 [4, 8, 5, 6, 2, 1, 4, 6, 7]]).

largeMatrix([[3, 3, 4, 5, 7, 4, 8, 6, 5],
			 [5, 5, 4, 6, 4, 6, 6, 4, 5],
			 [7, 5, 7, 8, 4, 5, 3, 2, 6],
			 [4, 8, 5, 6, 2, 1, 4, 6, 7]]).

% Interface
display_row([]).
display_row([H|T]) :-
	format('| ~w ', [H]),
	display_row(T).

display_matrix([]).
display_matrix([H|T]) :-
	display_row(H), write('|'), nl,
	display_matrix(T).

display_board(Matrix) :-
	write('+-----------------------------------+'), nl,
	display_matrix(Matrix),
	write('+-----------------------------------+'), nl.

display_info(Input) :-
	(Input = e -> write('The sum of each column is 15!');
	(Input = h -> write('The sum of each column is 20!'))).

valid_input(Input) :-
	Input = e, !;
	Input = h, !;
	fail.

choose_difficulty(Input) :-
	write('Choose difficulty:'), nl,
	write(' - Easy (e)'), nl,
	write(' - Hard (h)'), nl,
	read(Input), nl,
	valid_input(Input), !;
	choose_difficulty(Input).

reset_timer :- statistics(walltime, _).
print_time :-
	statistics(walltime, [_, T]),
	TS is ((T // 10) * 10) / 1000,
	nl, format('Time: ~ws', [TS]), nl.

% Logic
get_small_matrix(Matrix) :-
	smallMatrix(Matrix).

get_large_matrix(Matrix) :-
	largeMatrix(Matrix).

get_matrix(Input, Matrix) :-
	(Input = e -> get_small_matrix(Matrix);
	(Input = h -> get_large_matrix(Matrix))).

label_matrix([]).
label_matrix([Row | Rest]) :-
	labeling([], Row),
	label_matrix(Rest).

apply_restriction([], _).
apply_restriction([H|T], Sum) :-
	sum(H, #=, Sum),
	apply_restriction(T, Sum).

order_matrix([], []).
order_matrix([Row | Rest], [OrderedRow | OrderedRest]) :-
	samsort(Row, OrderedRow),
	order_matrix(Rest, OrderedRest).

sort_matrix([], []).
sort_matrix([Row | Rest], [SortedRow | SortedRest]) :-
	length(Row, Size),
	length(SortedRow, Size),
	length(Temp, Size),
	sorting(SortedRow, Temp, Row),
	sort_matrix(Rest, SortedRest).

solve_puzzle(Input, Matrix, FinalMatrix) :-
	order_matrix(Matrix, OrderedMatrix),
	sort_matrix(OrderedMatrix, FinalMatrix),
	transpose(FinalMatrix, TransposedMatrix),
	(Input = e -> apply_restriction(TransposedMatrix, 15);
	(Input = h -> apply_restriction(TransposedMatrix, 20))).

% Starting function
main :-
	choose_difficulty(Input),
	get_matrix(Input, Matrix),
	solve_puzzle(Input, Matrix, FinalMatrix),
	label_matrix(FinalMatrix),
	display_board(FinalMatrix),
	display_info(Input).