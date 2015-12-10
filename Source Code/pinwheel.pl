:- use_module(library(aggregate)).
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

% Board
outerWheel([3, 3, 4, 4, 5, 5, 6, 7, 8]).
innerWheel([4, 4, 4, 5, 5, 5, 6, 6, 6]).
innerWheel([2, 3, 4, 5, 5, 6, 7, 7, 8]).
innerWheel([1, 2, 4, 4, 5, 6, 6, 7, 8]).

% Interface
valid_input(Input) :-
	Input = e, !;
	Input = h, !;
	fail.

choose_difficulty(Input) :-
	write('Choose difficulty:\n'),
	write(' - Easy (e)\n'),
	write(' - Hard (h)\n'),
	read(Input),
	valid_input(Input), !;
	choose_difficulty(Input).

display_inner_wheels([]).
display_inner_wheels([H|T]) :-
	print(H), nl,
	display_inner_wheels(T).

% Logic
get_outer_wheel(OuterWheel) :-
	outerWheel(OuterWheel).

get_existing_wheels(ExistingWheels) :-
	findall(List, innerWheel(List), ExistingWheels).

create_wheel_list(WheelList) :-
	innerWheel(List),
	length(List, Size),
	length(WheelList, Size).

create_inner_wheels_list([], 0).
create_inner_wheels_list(InnerWheelsList, N) :-
	create_wheel_list(WheelList),
	domain(WheelList, 1, 8),
	N1 is N - 1,
	create_inner_wheels_list(NewWheelList, N1),
	append([WheelList], NewWheelList, InnerWheelsList).

count_value_in_list(List, Value, NumberOfReps) :-
	findall(Value, member(Value, List), L),
	length(L, NumberOfReps).

create_cardinality(_, [], 10).
create_cardinality(ExistingWheel, CardinalityList, N) :-
	N1 is N + 1,
	count_value_in_list(ExistingWheel, N, NumberOfReps),
	create_cardinality(ExistingWheel, NewCardinalityList, N1),
	append([N - NumberOfReps], NewCardinalityList, CardinalityList).

list_valid(ExistingWheels, InnerWheelsList, Index) :-
	nth1(Index, ExistingWheels, ExistingWheel),
	nth1(Index, InnerWheelsList, InnerWheelList),
	create_cardinality(ExistingWheel, CardinalityList, 1),
	global_cardinality(InnerWheelList, CardinalityList).

restrict_valid_lists(ExistingWheels, InnerWheelsList) :-
	length(ExistingWheels, Size),
	foreach(between(1, Size, Index), list_valid(ExistingWheels, InnerWheelsList, Index)).

label_inner_wheels([]).
label_inner_wheels([H|T]) :-
	labeling([], H),
	label_inner_wheels(T).

create_list_by_index([], _, []).
create_list_by_index([H|T], Index, List) :-
	nth1(Index, H, Elem),
	create_list_by_index(T, Index, NewList),
	append([Elem], NewList, List).

restrict_sum_column(InnerWheelsList, Index) :-
	create_list_by_index(InnerWheelsList, Index, Column),
	sum(Column, #=, 15).

restrict_sum(InnerWheelsList) :-
	innerWheel(List),
	length(List, Size),
	foreach(between(1, Size, Index), restrict_sum_column(InnerWheelsList, Index)).

% Main function
main :-
	choose_difficulty(Input),
	get_existing_wheels(ExistingWheels),
	length(ExistingWheels, Size),
	create_inner_wheels_list(InnerWheelsList, Size),
	restrict_valid_lists(ExistingWheels, InnerWheelsList),
	(Input = e -> restrict_sum(InnerWheelsList),
				  label_inner_wheels(InnerWheelsList),
				  display_inner_wheels(InnerWheelsList), !),
	(Input = h -> get_outer_wheel(OuterWheel),
	%			  restrict_sum(OuterWheel, InnerWheelsList),
	%			  label_all_wheels(OuterWheel, InnerWheelsList),
	%			  display_all_wheels(OuterWheel, InnerWheelsList), !),
	write('The End!').