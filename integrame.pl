:- ensure_loaded('checker.pl').

test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un srting, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de stringuri reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un string)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.

% Caz de baza 
intrebari(integ(_, _, [], _), []).

% Caz 1:
% Daca in celula curenta nu se gaseste o intrebare (lista)
intrebari(integ(H, W, [List_H|List_T], Vocab), Result):-
		\+este_intrebare(List_H),
		intrebari(integ(H, W, List_T, Vocab), Result).

% Caz 2:
% Daca in celula curenta se gaseste o singura intrebare 
intrebari(integ(H, W, [((R, C), [(Text, Dir, ID)])|List_T], Vocab),
[((R, C), Text, Dir, ID)|Result]) :-
		intrebari(integ(H, W, List_T, Vocab), Result).
% Caz 3:
% Daca in celula curenta se gasesc 2 intrebari (dd sau jd sau dj sau jj)
intrebari(integ(H, W, [((R, C), [(T1, D1, I1), (T2, D2, I2)])|List_T], Vocab),
[((R, C), T1, D1, I1), ((R, C), T2, D2, I2)|Result]) :-
		intrebari(integ(H, W, List_T, Vocab), Result).

% Functie ajutatoare
este_intrebare((_, X)) :- is_list(X).


% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.

id_intrebare(Integ, Intrebare, Q_ID) :-
		intrebari(Integ, Q), % Q - lista de intrebari
		find_question(Q, Intrebare, Q_ID).

% Functie ajutatoare
% Caz de baza
find_question([], _, _) :- false.

% Caz in care se gaseste intrebarea cautata
% (fie dupa id, fie dupa text)
find_question([((_, _), T, _, I) | _], T, I).

% Caz in care nu se gaseste intrebarea cautata
find_question([((_, _), _, _, _) | Tail], Text, ID) :-
		find_question(Tail, Text, ID).


% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvânt
% de completat; ambele sunt stringuri.
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).

completare(_, [], _).

% Trece la alta celula atunci cand in celula curenta nu se afla o intrebare
completare(integ(H, W, [List_H|List_T], Vocab), Solutie, Rezultat) :-
		\+este_intrebare(List_H),
		completare(integ(H, W, List_T, Vocab), Solutie, Rezultat).

% Daca celula contine o intrebare
%completare(integ(H, W, [List_H|List_T], Vocab), Solutie, Rezultat):-
%		o_intrebare(List_H).


% Daca celula contine 2 intrebari
%completare(integ(H, W, [List_H|List_T], Vocab), Solutie, Rezultat):-
%		doua_intrebari(List_H).

% completare_dreapta(+Raspuns, +Inceput, +Final, -Integrama)
% Final este Inceput + Lungime raspuns
completare_dreapta([], _, _, _).
completare_dreapta(_, N, N, _).
completare_dreapta([Head|Tail], Inceput, Final, integ(_, _, [((R, C), _)], _)):-
		Inceput1 is Inceput + 1,
		C1 is C + 1,
		completare_dreapta(Tail, Inceput1, Final, integ(_, _, [(R, C1), Head], _)).
		

%atom_chars(Raspuns, Lista_chars)
%nth0(0, Lista_chars, Char)

o_intrebare((_, X)):-
		este_intrebare(x),
		length(X, 1).

doua_intrebari((_, X)):-
		este_intrebare(X),
		length(X, 2).


% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).
lungime_spatiu(_, _, _) :- false.

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).
intersectie(_, _, _, _, _) :- false.

% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de stringuri, fiecare string
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])
solutii_posibile(_, _) :- false.

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de stringuri, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca string) care este
% răspunsul la întrebare.
rezolvare(_, _) :- false.
