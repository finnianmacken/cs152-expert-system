% Used for predicates that are added later
:- dynamic known/3, multivalued/1.


% Time Required
time_required(museo_evita, 2).
time_required(science_museum, 3).
time_required(fine_arts, 5).
time_required(borges_center, 4).
time_required(silvori_museum, 1).
time_required(malba, 4).
time_required(museo_modern, 4).

time_required(library, 1).
time_required(recoletta_cemetery, 4).
time_required(basilica, 3).
time_required(jazz_backroom, 2).
time_required(tango, 3).
time_required(teatro_colon, 3).

time_required(botanical_gardens, 2).
time_required(football_centro_garrigos, 4).
time_required(ecological_reserve, 4).
time_required(centenario_park, 2).
time_required(rock_climb, 3).
time_required(peru_beach, 6).


% 
time_appropriate(Location) :- time_required(Location, Time_Required), between(1, 2, Time_Required), time_required(short).
time_appropriate(Location) :- time_required(Location, Time_Required), between(3, 100, Time_Required), time_required(long).


% RULES 

%% Museums %%
recommended(museo_evita):- experience(cultural), time_appropriate(museo_evita), budget(free), culture_type(museum).

recommended(science_museum):- experience(cultural), time_appropriate(science_museum), distance(close), budget(free), culture_type(museum).

recommended(fine_arts):- experience(cultural), time_appropriate(fine_arts), distance(far), budget(free), culture_type(museum), museum_type(art).
recommended(borges_center):- experience(cultural), time_appropriate(borges_center), distance(far), budget(free), culture_type(museum), museum_type(culture).

recommended(silvori_museum):- experience(cultural), time_appropriate(silvori_museum),  \+ budget(free), culture_type(museum).

recommended(malba):- experience(cultural), time_appropriate(malba), distance(close), \+ budget(free), culture_type(museum).

recommended(museo_modern):- experience(cultural), time_appropriate(museo_modern), distance(far), \+ budget(free), culture_type(museum).

%% Not Museums %%
recommended(library):- experience(cultural), time_appropriate(library), budget(free), \+ culture_type(museum).

recommended(recoletta_cemetery):- experience(cultural), time_appropriate(recoletta_cemetery), distance(close), budget(free), \+ culture_type(museum).

recommended(basilica):- experience(cultural), time_appropriate(basilica), distance(far), budget(free), \+ culture_type(museum).

recommended(jazz_backroom):- experience(cultural), time_appropriate(jazz_backroom), \+ budget(free), \+ culture_type(museum).

recommended(tango):- experience(cultural), time_appropriate(tango), distance(close), \+ budget(free), \+ culture_type(museum).

recommended(teatro_colon):- experience(cultural), time_appropriate(teatro_colon), distance(far), \+ budget(free), \+ culture_type(museum).


%% Physical Activities %%
recommended(botanical_gardens):- experience(physical_activity), time_appropriate(botanical_gardens), budget(free).

recommended(football_centro_garrigos):- experience(physical_activity), time_appropriate(football_centro_garrigos), distance(close), budget(free).

recommended(ecological_reserve):- experience(physical_activity), time_appropriate(ecological_reserve), distance(far), budget(free).

recommended(centenario_park):- experience(physical_activity), time_appropriate(centenario_park), \+ budget(free).

recommended(rock_climb):- experience(physical_activity), time_appropriate(rock_climb), distance(close), \+ budget(free).

recommended(peru_beach):- experience(physical_activity), time_appropriate(peru_beach), distance(far), \+ budget(free).


%% Culinary: Vegetarian %%
recommended(veganious) :- experience(culinary), distance(close), price_point(low), diet(vegetarian). 

recommended(toque_perfecto) :- experience(culinary), distance(far), price_point(low), diet(vegetarian). 

recommended(cang_tin) :- experience(culinary), distance(close), price_point(medium), diet(vegetarian). 

recommended(saigon_noodle) :- experience(culinary), distance(far), price_point(medium), diet(vegetarian). 

recommended(chui) :- experience(culinary), distance(close), price_point(high), diet(vegetarian). 
recommended(mudra) :- experience(culinary), distance(close), price_point(high), diet(vegetarian). 

recommended(tandoor) :- experience(culinary), distance(far), price_point(high), diet(vegetarian). 


%% Culinary: Meat Eaters %%
recommended(chori) :- experience(culinary), distance(close), price_point(low), \+ diet(vegetarian). 

recommended(sazon_cuyagua) :- experience(culinary), distance(far), price_point(low), \+ diet(vegetarian). 

recommended(koi_dumplings) :- experience(culinary), distance(close), price_point(medium), \+ diet(vegetarian). 
recommended(kefi_greek) :- experience(culinary), distance(close), price_point(medium), \+ diet(vegetarian). 

recommended(concina_yovita) :- experience(culinary), distance(far), price_point(medium), \+ diet(vegetarian). 
recommended(mr_ho_korean) :- experience(culinary), distance(far), price_point(medium), \+ diet(vegetarian). 

recommended(don_julio) :- experience(culinary), distance(close), price_point(high), \+ diet(vegetarian). 

recommended(sagardi_argentina) :- experience(culinary), distance(far), price_point(high), \+ diet(vegetarian). 


% Askables

experience(Value) :- menuask(experience, Value, [cultural, physical_activity, culinary]).

time_required(Value) :- menuask(time_required, Value, [short, long]).

distance(Value) :- menuask(distance, Value, [close, far]).

budget(Value) :- ask(budget, Value). 

price_point(Value) :- menuask(price_point, Value, [low, medium, high]).

culture_type(Value) :- ask(culture_type, Value). 

museum_type(Value) :- menuask(museum_type, Value, [art, culture]).

diet(Value) :- ask(diet, Value). 


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








