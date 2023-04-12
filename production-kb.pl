
% Used for predicates that are added later
:- dynamic known/3, multivalued/1.


% Distances
distance(museum, 3).
distance(monument, 3).
distance(barracks, 7).
distance(art_gallery, 9).
distance(fairground, 9).
distance(cafe, 15).

distance_appropriate(Location) :- distance(Location, Distance), between(0, 4, Distance), distance(close).
distance_appropriate(Location) :- distance(Location, Distance), between(4, 10, Distance), distance(medium).
distance_appropriate(Location) :- distance(Location, Distance), between(10, 100, Distance), distance(far).

% Rules

% Rule 1:

recommended(monument):- experience(historical).

% Rule 2

recommended(museum) :- experience(cultural).

% Rule 3

recommended(barracks) :- experience(historical).

% Rule 4

recommended(art_gallery) :- experience(cultural).

% Rule 5

recommended(cafe) :- experience(culinary).


final_recommendation(X) :- recommended(X), distance_appropriate(X).

% Askables

distance(Value) :- menuask(distance, Value, [close, medium, far]).

experience(Value) :- menuask(experience, Value, [cultural, historical, culinary]).


% System Framework

%% ASK

ask(A, V):-
known(yes, A, V), % succeed if true
!.	% stop looking

ask(A, V):-
known(_, A, V), % fail if false
!, fail.

% If not multivalued, and already known to be something else, don't ask again for a different value.
ask(A, V):-
\+multivalued(A),
known(yes, A, V2),
V \== V2,
!, fail.

ask(A, V):-
read_py(A,V,Y), % get the answer
assertz(known(Y, A, V)), % remember it
atom_string(Z, Y),
Z == yes.	% succeed or fail

%% MENU ASK
menuask(A, V, Menu):-
known(yes, A, V), % succeed if true
!.	% stop looking

menuask(A, V, Menu):-
known(yes, A, _), % fail if it's not a provided value
!, fail.

% If not multivalued, and already known to be something else, don't ask again for a different value.
menuask(A, V):-
\+multivalued(A),
known(yes, A, V2),
V \== V2,
!, fail.

menuask(A, V, Menu):-
read_menu_py(A, V, X, Menu), % get the answer
write(X),
atom_string(Z, X),
check_val(Z, A, V, Menu),
asserta(known(yes, A, Z)),
Z == V.

check_val(Z, A, V, Menu) :-
member(Z, Menu), !.

check_val(Z, A, V, Menu) :-
ask_menu_again_py(Z, A, V),
menuask(A, V, Menu).