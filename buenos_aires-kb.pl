% Used for predicates that are added later
:- dynamic known/3, multivalued/1.


% Distances
distance(museo_evita, 4).
distance(science_museum, 4).
distance(fine_arts, 10).
distance(borges_center, 10).
distance(silvori_museum, 4).
distance(malba, 4).
distance(museo_modern, 10).

distance(library, 4).
distance(recoletta_cemetery, 4).
distance(basilica, 10).
distance(jazz_backroom, 4).
distance(tango, 4).
distance(teatro_colon, 10).

distance(botanical_gardens, 4).
distance(football_centro_garrigos, 4).
distance(ecological_reserve, 10).
distance(centenario_park, 4).
distance(rock_climb, 4).
distance(peru_beach, 10).

distance(vege_buffet, 4).
distance(toque_perfecto, 10).
distance(cang_tin, 4).
distance(saigon_noodle, 10).
distance(chui, 4).
distance(tandoor, 10).
distance(chori, 4).
distance(sazon_cuyagua, 10).
distance(koi_dumplings, 4).
distance(concina_yovita, 10).
distance(don_julio, 4).
distance(sagardi_argentina, 10).


% Time Required
time_required(museo_evita, 1).
time_required(science_museum, 3).
time_required(fine_arts, 4).
time_required(borges_center, 4).
time_required(silvori_museum, 1).
time_required(malba, 4).
time_required(museo_modern, 4).

time_required(library, 1).
time_required(recoletta_cemetery, 4).
time_required(basilica, 4).
time_required(jazz_backroom, 1).
time_required(tango, 4).
time_required(teatro_colon, 4).

time_required(botanical_gardens, 1).
time_required(football_centro_garrigos, 4).
time_required(ecological_reserve, 4).
time_required(centenario_park, 1).
time_required(rock_climb, 4).
time_required(peru_beach, 4).


% 
distance_appropriate(Location) :- distance(Location, Distance), between(0, 5, Distance), distance(close).
distance_appropriate(Location) :- distance(Location, Distance), between(5, 100, Distance), distance(far).

% 
time_appropriate(Location) :- time_required(Location, Time_Required), between(1, 2, Time_Required), time_required(short).
time_appropriate(Location) :- time_required(Location, Time_Required), between(3, 100, Time_Required), time_required(long).


% RULES 

%% Museums %%
recommended(museo_evita):- experience(cultural), time_appropriate(museo_evita), budget(free), culture_type(museum).

recommended(science_museum):- experience(cultural), time_appropriate(science_museum), distance_appropriate(science_museum), budget(free), culture_type(museum).

recommended(fine_arts):- experience(cultural), time_appropriate(fine_arts), distance_appropriate(fine_arts), budget(free), culture_type(museum), museum_type(art).
recommended(borges_center):- experience(cultural), time_appropriate(borges_center), distance_appropriate(borges_center), budget(free), culture_type(museum), museum_type(culture).

recommended(silvori_museum):- experience(cultural), time_appropriate(silvori_museum),  budget(silvori_museum), culture_type(museum).

recommended(malba):- experience(cultural), time_appropriate(malba), distance_appropriate(malba), \+ budget(free), culture_type(museum).

recommended(museo_modern):- experience(cultural), time_appropriate(museo_modern), distance_appropriate(museo_modern), \+ budget(free), culture_type(museum).


%% Not Museums %%
recommended(library):- experience(cultural), time_appropriate(library), budget(free), \+ culture_type(museum).

recommended(recoletta_cemetery):- experience(cultural), time_appropriate(recoletta_cemetery), distance_appropriate(recoletta_cemetery), budget(free), \+ culture_type(museum).

recommended(basilica):- experience(cultural), time_appropriate(basilica), distance_appropriate(basilica), budget(free), \+ culture_type(museum).

recommended(jazz_backroom):- experience(cultural), time_appropriate(jazz_backroom), \+ budget(free), \+ culture_type(museum).

recommended(tango):- experience(cultural), time_appropriate(tango), distance_appropriate(tango), \+ budget(free), \+ culture_type(museum).

recommended(teatro_colon):- experience(cultural), time_appropriate(teatro_colon), distance_appropriate(teatro_colon), \+ budget(free), \+ culture_type(museum).


%% Physical Activities %%
recommended(botanical_gardens):- experience(physical_activity), time_appropriate(botanical_gardens), budget(free).

recommended(football_centro_garrigos):- experience(physical_activity), time_appropriate(football_centro_garrigos), distance_appropriate(football_centro_garrigos), budget(free).

recommended(ecological_reserve):- experience(physical_activity), time_appropriate(ecological_reserve), distance_appropriate(ecological_reserve), budget(free).

recommended(centenario_park):- experience(physical_activity), time_appropriate(centenario_park), \+ budget(free).

recommended(rock_climb):- experience(physical_activity), time_appropriate(rock_climb), distance_appropriate(rock_climb), \+ budget(free).

recommended(peru_beach):- experience(physical_activity), time_appropriate(peru_beach), distance_appropriate(peru_beach), \+ budget(free).


%% Culinary: Vegetarian %%
recommended(vege_buffet) :- experience(culinary), distance_appropriate(vege_buffet), price_point(low), diet(vegetarian). 

recommended(toque_perfecto) :- experience(culinary), distance_appropriate(toque_perfecto), price_point(low), diet(vegetarian). 

recommended(cang_tin) :- experience(culinary), distance_appropriate(cang_tin), price_point(medium), diet(vegetarian). 

recommended(saigon_noodle) :- experience(culinary), distance_appropriate(saigon_noodle), price_point(medium), diet(vegetarian). 

recommended(chui) :- experience(culinary), distance_appropriate(chui), price_point(high), diet(vegetarian). 

recommended(tandoor) :- experience(culinary), distance_appropriate(tandoor), price_point(high), diet(vegetarian). 


%% Culinary: Meat Eaters %%
recommended(chori) :- experience(culinary), distance_appropriate(chori), price_point(low), \+ diet(vegetarian). 

recommended(sazon_cuyagua) :- experience(culinary), distance_appropriate(sazon_cuyagua), price_point(low), \+ diet(vegetarian). 

recommended(koi_dumplings) :- experience(culinary), distance_appropriate(koi_dumplings), price_point(medium), \+ diet(vegetarian). 

recommended(concina_yovita) :- experience(culinary), distance_appropriate(concina_yovita), price_point(medium), \+ diet(vegetarian). 

recommended(don_julio) :- experience(culinary), distance_appropriate(don_julio), price_point(high), \+ diet(vegetarian). 

recommended(sagardi_argentina) :- experience(culinary), distance_appropriate(sagardi_argentina), price_point(high), \+ diet(vegetarian). 



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








