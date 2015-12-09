:- use_module(library(aggregate)).
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

% Wheels' values
outerWheel([3, 3, 4, 4, 5, 5, 6, 7, 8]).
innerWheel([4, 4, 4, 5, 5, 5, 6, 6, 6]).
innerWheel([2, 3, 4, 5, 5, 6, 7, 7, 8]).
innerWheel([1, 2, 4, 4, 5, 6, 6, 7, 8]).

% Interface
valid_input(Input) :-
	Input = e, !;
	Input = h, !;
	fail.

choose_difficulty(M) :-
	write('Choose difficulty:\n'),
	write(' - Easy (e)\n'),
	write(' - Hard (h)\n'),
	read(Input),
	valid_input(Input), !;
	choose_difficulty(Input).

% Logic
get_existing_wheels(existingWheels) :-
	findall(List, innerWheel(List), existingWheels).

create_wheel_list(wheelList) :-
	innerWheel(List),
	length(List, Si)

create_inner_wheels_list([_]).
create_inner_wheels_list(innerWheelsList) :-



	
% Main function
main :-
	choose_difficulty(M),
	get_existing_wheels(existingWheels),
	length(existingWheels, Size),
	create_inner_wheels_list(innerWheelsList),
	
	
	
	
	
	
	
	
	
	
	
	
	