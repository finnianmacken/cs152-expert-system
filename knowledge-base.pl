%  Tell prolog that known/3 and multivalued/1 will be added later
:- dynamic known/3, multivalued/1.

problem(battery) :- \+engine(turning_over), battery(bad).
problem(engine_oil_low) :- \+engine(turning_over), warning_light(oil).
problem(out_of_gas) :- engine(turning_over), gas_gauge(empty).
problem(engine_flooded) :- engine(turning_over), smell(gas).
battery(bad) :- lights(weak).
battery(bad) :- radio(weak).

% The code below implements the prompting to ask the user:

gas_gauge(X) :- ask(gas_gauge, X).
engine(X) :- ask(engine, X).
lights(X) :- ask(lights, X).
radio(X) :- ask(radio, X).
smell(X) :- ask(smell, X).
warning_light(X) :- ask(warning_light,X).

% Asking clauses

ask(A, V):-
known(yes, A, V), % succeed if true
!.	% stop looking

ask(A, V):-
known(_, A, V), % fail if false
!, fail.

ask(A, V):-
\+multivalued(A),
known(yes, A, V2),
V \== V2,
!, fail.

ask(A, V):-
read_py(A,V,Y), % get the answer
assertz(known(Y, A, V)), % remember it
Y == "yes".