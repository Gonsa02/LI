% 1. Escribe un predicado prod(L,P) que signifique: “P es el producto de los elementos de la lista 
% de enteros dada L”. Debe poder generar la P y tambi ́en comprobar una P dada
%prod(L, P)
prod([], 1).
prod([X|L], P):- prod(L, P1), P is X*P1. 

%Escribe un predicado pescalar(L1,L2,P) que signifique: “P es el producto escalar de los
%vectores L1 y L2”, donde los vectores se representan como listas de enteros. El predicado debe
%fallar si los dos vectores tienen longitudes distintas.
%pescalar(L1,L2,P)
pescalar([], [], 0).
pescalar([X1|L1], [X2|L2], P):- pescalar(L1, L2, P1), P is X1*X2+P1.

% 3. Representando conjuntos con listas sin repeticiones, escribe predicados para las operaciones de
% intersecci ́on y uni ́on de conjuntos dados.
%union (L1, L2, L3)
union([], L, L).
union([X|L1], L2, [X|L3]):- not(member(X, L2)), !, union(L1,L2,L3).
union([_|L1], L2, L3):- union(L1,L2,L3).

inter([], [_|_], []).
inter([X|L1], L2, [X|L3]):- member(X,L2), !, inter(L1,L2,L3).
inter([_|L1], L2, L3):- inter(L1,L2,L3).

% 4. Usando append, escribe un predicado para calcular el  ́ultimo elemento de una lista dada, y otro
% para calcular la lista inversa de una lista dada.
last([X], X):-!.
last([_|L], Y):- last(L, Y).

inverse([X], [X]).
inverse([X|L], LR):- inverse(L, LRR), append(LRR,[X], LR).

%5. Escribe un predicado fib(N,F) que signifique: “F es el N- ́esimo n ́umero de Fibonacci para la
%N dada”. Estos n ́umeros se definen as ́ı: fib(1) = 1, fib(2) = 1, y si N > 2 entonces fib(N ) =
%fib(N − 1) + fib(N − 2).
fib(0,1):-!.
fib(1,2):-!.
fib(N,F):- N1 is N-1, N2 is N-2, fib(N1, F1), fib(N2, F2), F is F1+F2.

%6. Escribe un predicado dados(P,N,L) que signifique: “la lista L expresa una manera de sumar
%P puntos lanzando N dados”. Por ejemplo: si P es 5 y N es 2, una soluci ́on ser ́ıa [1,4] (n ́otese
%que la longitud de L es N). Tanto P como N vienen instanciados. El predicado debe ser capaz de
%generar todas las soluciones posibles.
dados(P,N,L):- genera_daus(N,L), msort(L,L), sum_list(L, S), S is P, write(L), nl, fail.

genera_daus(0, []):-!.
genera_daus(N, [D|L1]):- between(1,6,D), N1 is N-1, genera_daus(N1, L1).


%7. Escribe un predicado suma_demas(L) que, dada una lista de enteros L, se satisface si existe alg´un
%elemento en L que es igual a la suma de los dem´as elementos de L, y falla en caso contrario.
suma_demas(L):- select(X, L, L1), sum_list(L1, Sum), X =:= Sum.

%8. Escribe un predicado suma_ants(L) que, dada una lista de enteros L, se satisface si existe alg´un
%elemento en L que es igual a la suma de los elementos anteriores a él en L, y falla en caso contrario.
suma_ants(L):- suma_ants(L, 0).
suma_ants([X|_], S):- X==S, true, !.
suma_ants([X|L], S):- S1 is X+S, suma_ants(L, S1).
suma_ants([], _):- fail.

%9. Escribe un predicado card(L) que, dada una lista de enteros L, escriba la lista que, para cada
%elemento de L, dice cuántas veces aparece este elemento en L. Por ejemplo, si hacemos la consulta
%card( [1,2,1,5,1,3,3,7] ) el intérprete escribirá:
card(L) :-
    findall((X, Count), (bagof(true, member(X, L), Xs), length(Xs, Count)), Counts),
    write(Counts).

%10. Escribe un predicado esta ordenada(L) que signifique: “la lista L de números enteros está
%ordenada de menor a mayor”. Por ejemplo, a la consulta:
esta_ordenada([]).
esta_ordenada([_]).
esta_ordenada([X, Y | L]):- X =< Y, esta_ordenada([Y|L]).

%11. Escribe un predicado ord(L1,L2) que signifique: “L2 es la lista de enteros L1 ordenada de
%menor a mayor”. Por ejemplo: si L1 es [4,5,3,3,2] entonces L2 será [2,3,3,4,5]. Hazlo en
%una lı́nea, usando sólo los predicados permutacion y esta ordenada.
ord(L1,L2):- permutation(L1, L2), esta_ordenada(L2).

%12. Escribe un predicado diccionario(A,N) que, dado un alfabeto A de sı́mbolos y un natural N,
%escriba todas las palabras de N sı́mbolos, por orden alfabético (el orden alfabético es según el
%alfabeto A dado). Por ejemplo, diccionario( [ga,chu,le],2) escribirá:
%gaga gachu gale chuga chuchu chule lega lechu lele.
diccionario(A, N):- diccionario(A, N, []).
diccionario(_, 0, F):- inverse(F,X), write(X), !.
diccionario(A, N, F):- N > 0 , N1 is N-1, forall(member(X,A), diccionario(A, N1, [X|F])).

%13. Escribe un predicado palindromos(L) que, dada una lista de letras L, escriba todas las per-
%mutaciones de sus elementos que sean palı́ndromos (capicúas). Por ejemplo, con la consulta
%palindromos([a,a,c,c]) se escribe [a,c,c,a] y [c,a,a,c].
palindromos(L):- setof(X, (permutation(L, X), inverse(X, Y), igual(X, Y)), R), write(R).

igual([], []).
igual([X|L], [Y|S]):- X == Y, igual(L,S).

%14
sendMoreMoney:-
    Letters = [S, E, N, D, M, O, R, Y, _, _],
    Numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    permutation(Numbers, Letters),
    S1 is 1000*S + 100*E + 10*N + D + 1000*M + 100*O + 10*R + E,
    S2 is 10000 * M + 1000 * O + 100 * N + 10 * E + Y,
    S1 == S2,
    writeLetters(Letters), !.

writeLetters([S, E, N, D, M, O, R, Y, _, _]):-
    write('S = '), write(S), nl,
    write('E = '), write(E), nl,
    write('N = '), write(N), nl,
    write('D = '), write(D), nl,
    write('M = '), write(M), nl,
    write('O = '), write(O), nl,                                                 
    write('R = '), write(R), nl,
    write('Y = '), write(Y), nl,
    writeSuma([S, E, N, D, M, O, R, Y]).

writeSuma([S, E, N, D, M, O, R, Y]):-
    S1 is 1000 * S + 100 * E + 10 * N + D,
    S2 is 1000 * M + 100 * O + 10 * R + E,
    S3 is S1 + S2,
    S4 is  10000 * M + 1000 * O + 100 * N + 10 * E + Y,
    write('SEND = '), write(S1), 
    write(' + '), 
    write('MORE = '), write(S2), nl,
    write('      = '), write(S3), nl,
    write( '----------------------------------------' ), nl, 
    write('MONEY = '), write(S4).

%15. Escribe un predicado simplifica que pueda usarse en combinación con el programa de calcular
%derivadas
der(X, X, 1):-!.
der(C, _, 0) :- number(C).
der(A+B, X, A1+B1) :- der(A, X, A1), der(B, X, B1).
der(A-B, X, A1-B1) :- der(A, X, A1), der(B, X, B1).
der(A*B, X, A*B1+B*A1) :- der(A, X, A1), der(B, X, B1).
der(sin(A), X, cos(A)*B) :- der(A, X, B).
der(cos(A), X, -sin(A)*B) :- der(A, X, B).
der(e^A, X, B*e^A) :- der(A, X, B).
der(ln(A), X, B*1/A) :- der(A, X, B).

simplifica(A,B):- unpas(A,C),!, simplifica(C,B).
	simplifica(A,A):-!.

	unpas(A,B):- pasSuperior(A,B).
	unpas(A,B):- pasSubExpressio(A,B).

	pasSubExpressio(A,B):-
		A=..[F|La],
		append(L1,[Ea|L2],La),
		unpas(Ea,Eb),
		append(L1,[Eb|L2],Lb),
		B=..[F|Lb].

	pasSuperior(A+0,A).
	pasSuperior(0+B,B).
	pasSuperior(A-0,A).
	pasSuperior(0-B,-B).
	pasSuperior(A-A,0).
	pasSuperior(A*1,A).
	pasSuperior(1*B,B).
	pasSuperior(A/1,A).
	pasSuperior(_*0,0).
	pasSuperior(0*_,0).
	pasSuperior(0/_,0).
	pasSuperior(A+B,N ):- integer(A), integer(B), N is A+B.
	pasSuperior(A*B,N ):- integer(A), integer(B), N is A*B.
	pasSuperior(A-B,N ):- integer(A), integer(B), N is A-B.
	pasSuperior(A//B,N):- integer(A), integer(B), B\=0, N is A//B.


%16
p([],[]).
p(L, [f(X,Y)|P]):- select(f(X,Y), L, R), p(R, P).
p(L, [f(Y,X)|P]):- select(f(X,Y), L, R), p(R, P).

dom(L):- p(L,P), ok(P), write(P), nl.
dom(_):- write('no hay cadena'), nl.

ok([]).
ok([f(_,_)]).
ok([f(_,Y), f(Y, Z)|P]):- ok([f(Y,Z)|P]).

gir([f(X,Y)|_], [f(Y,X)|_]).

%17. Complete the following backtracking procedure for SAT in Prolog. Program everything, except
%the predicate readclauses(F), which reads a list of clauses, where each clause is a list of integers.
%For example, p3 ∨ ¬p6 ∨ p2 is represented by [3,-6,2]. Do things as simple as possible.
%p:- readclauses(F), sat([],F).
%p:- write(’UNSAT’),nl.sat(I,[]):- write(’IT IS SATISFIABLE. Model: ’), write(I),nl,!.
sat(I,F):-
    decision_lit(F,Lit), % Select unit clause if any; otherwise, an arbitrary one.
    simplif(Lit,F,F1), % Simplifies F. Warning: may fail and cause backtracking
    sat([Lit|I], F1).

decision_lit([X|_], Lit):- member(Lit, X), length(X,1), !.
decision_lit([X|_], Lit):- member(Lit, X).

simplif(_, [], []).
%Si es miembro de una clausula
simplif(Lit, [X|F], F1):- member(Lit, X), simplif(Lit, F, F1), !.
%Caso donde -Lit es miembro de una clausula
simplif(Lit, [X|F], F1):-
    Aux is 0 - Lit,
    member(Aux, X),
    quitar_literal(Aux, X, X1),
    simplif(Lit, F, F2),
    append([X1], F2, F1).
%Caso donde Lit no es miembro.
simplif(Lit, [X|F], [X|F1]):- simplif(Lit, F, F1).

quitar_literal(_, [], []):- !.
quitar_literal(Lit, [Lit|F], F1):- quitar_literal(Lit, F, F1).
quitar_literal(Lit, [X|F], [X|F1]):- quitar_literal(Lit, F, F1).

%18. Consider two groups of 10 people each. In the first group, as expected, the percentage of people
%with lung cancer among smokers is higher than among non-smokers. In the second group, the
%same is the case. But if we consider the 20 people of the two groups together, then the situation
%is the opposite: the proportion of people with lung cancer is higher among non-smokers than
%among smokers! Can this be true? Write a little Prolog program to find it out.

lung_cancer_comparison:-
    num(NCNF1), num(NCSF1), num(SCNF1), num(SCSF1),
    10 is NCNF1 + NCSF1 + SCNF1 + SCSF1,

    SCNF1 / (SCNF1 + NCNF1) < SCSF1 / (SCSF1 + NCSF1),

    num(NCNF2), num(NCSF2), num(SCNF2), num(SCSF2),
    10 is NCNF2 + NCSF2 + SCNF2 + SCSF2,

    SCNF2 / (SCNF2 + NCNF2) < SCSF2 /(SCSF2 +NCSF2),

    (SCNF1 + SCNF2) / (SCNF1 + SCNF2 + NCNF1+ NCNF2) > (SCSF1 + SCSF2) /(SCSF1 + SCSF2 + NCSF1 + NCSF2),

    write([NCNF1, NCSF1, SCNF1, SCSF1, ' ', NCNF2, NCSF2, SCNF2, SCSF1]), nl, fail.

num(X):- between(1, 10, X).

%19. Supongamos que tenemos una máquina que dispone de monedas de valores [X1,...Xn] y tiene
%que devolver una cantidad C de cambio utilizando el mı́nimo número de monedas. Escribe un
%programa Prolog maq(L,C,M) que, dada la lista de monedas L y la cantidad C, genere en M la
%lista de monedas a devolver de cada tipo. Por ejemplo, si L es [1,2,5,13,17,35,157], y C es
%361, entonces una respuesta es [0,0,0,1,2,0,2] (5 monedas).
%Note: greedy algorithms (starting with the largest coin, etc.) do not always work!
maq(L,C,M):- maq_aux(L,C,M, 0).

maq_aux(L,C,M,N):- length(L, A), genera_comb(A,N,M),pescalar(L,M,C).
maq_aux(L,C,M,N):- N1 is N+1, maq_aux(L,C,M,N1).

genera_comb(0,_,[]):- !.
genera_comb(A, N, M):- between(0, N, X), A2 is A-1, N2 is N-X, genera_comb(A2, N2, M2), append([X], M2, M).

%20. Write in Prolog a predicate flatten(L,F) that “flattens” (cast.: “aplana”) the list F as in the
%example:
%?-flatten( [a,b,[c,[d],e,[]],f,[g,h]], F ).
%F=[a,b,c,d,e,f,g,h]?
flatten([],[]):- !.
flatten([X|L],F):- flatten(X, F1), flatten(L, F2), append(F1, F2, F).
flatten(L,[L]).

%21. Escribe un predicado Prolog log(B,N,L) que calcula la parte entera L del logaritmo en base B
%de N, donde B y N son naturales positivos dados. Por ejemplo, ?- log(2,1020,L). escribe L=9?
%Podéis usar la exponenciación, como en 125 is 5**3. El programa (completo) no debe ocupar
%más de 3 lineas.
%log(B,N,L):-
log(_,1,0):- !.
log(B,N,L):- between(1,N,L), L2 is L+1, R1 is B**L, R2 is B**L2, R1 =< N, R2 > N, !.

%22. Supongamos que N estudiantes (identificados por un número entre 1 y N) se quieren matricular
%de LI, pero sólo hay espacio para M, con M < N. Además nos dan una lista L de pares de estos
%estudiantes que son incompatibles entre sı́ (por ejemplo, porque siempre se copian). Queremos
%obtener un programa Prolog li(N,M,L,S) que, para N, M y L dados, genere un subconjunto S
%con M de los N estudiantes tal que si [x, y] ∈ L entonces {x, y} ̸⊆ S. Por ejemplo, una solución de
%li( 20, 16, [[8,11],[8,15],[11,6],[4,9],[18,13],[7,9],[16,8],[18,10],[6,17],[8,20]], S )
%es [20,19,17,16,15,14,13,12,11,10,7,5,4,3,2,1].
%li(N,M,L,S):-
li(N,M,L,S):- genera_lista(N, X), subcjto(X, S), length(S, M), compatible(L, S).
genera_lista(0, []):- !.
genera_lista(N, [N|L]):- N1 is N-1, genera_lista(N1, L).

subcjto([], []).
subcjto([_|L], S):- subcjto(L,S).
subcjto([X|L], [X|S]):- subcjto(L,S).

compatible([], _):- !.
compatible([[X,_]|S], L):- not(member(X, L)), compatible(S, L).
compatible([[_,Y]|S], L):- not(member(Y, L)), compatible(S, L).

%23. Given a list of integers L, and a maximum sum K, write the subsets Sm of L such that:
% sum(Sm) =< K, and
% no element in L \ Sm can be added to Sm without exceeding the sum K.
%% Example:
%numbers([2,5,7,-2,2,9,3,4,1]).
%maxSum(6).
%% subsetWithRest(L, Subset, Rest) holds
%% if Subset is a subset of L and Rest is the rest of the elements.
subsetWithRest([], [], []).
subsetWithRest([X|L], [X|S], R):- subsetWithRest(L, S, R).
subsetWithRest([X|L], S, [X|R]):- subsetWithRest(L, S, R).

%% maxSubset(K, L, Sm) holds
%% if Sm is a subset of numbers of L such that
%% it sums at most K
%% and if we try to add any other element, the sum exceeds K
maxSubset(K, L, Sm):-
    subsetWithRest(L, Sm, Rest),
    sum_list(Sm, X),
    X =< K,
	K1 is K - X,
	findall(V, (member(V, Rest), V > K1), Set), 
	Set = Rest.


%main:-
%numbers(L), maxSum(K),
%maxSubset(K, L, Sm),
%write(Sm), nl, fail.
%main:- halt.

%24. Given a graph declared as in the example below, write all its cliques of size at least minCliqueSize.
%Remember, a clique is a complete subgraph: a subset {textttS of the vertices such that for all
%U,V in S there is an edge U-V.
numVertices(10).
minCliqueSize(4).
vertices(Vs):- numVertices(N), findall(I,between(1,N,I),Vs).
vertex(V):- vertices(Vs), member(V,Vs).
edge(U,V):- edge1(U,V).
edge(U,V):- edge1(V,U).
edge1(9,8).
edge1(8,2).
edge1(7,4).
edge1(5,7).
edge1(4,2).
edge1(5,2).
edge1(2,7).
edge1(7,9).
edge1(2,9).
edge1(4,8).
edge1(4,9).
edge1(9,5).
edge1(4,5).
%%==========================================================
main:- findall(S, (vertices(Vs), subconjunto(Vs, S), length(S, N), minCliqueSize(Size), N >= Size, isClique(S)), Clique), writeAll(Clique).
main:- halt.

isClique([]).
isClique([_]).
%Es una clica si per tot parell de vertex hi ha aresta
isClique(S):- forall((member(U,S), member(V,S), U \= V), edge(U,V)).

writeAll([]).
writeAll([C|Cs]):- write(C), nl, writeAll(Cs).

subconjunto([], []).
subconjunto([_|L], S):- subconjunto(L,S).
subconjunto([X|L], [X|S]):- subconjunto(L,S).

%25. Complete the following predicate in prolog.
% nthRoot( N, K, R ) === "Given positive integers N and K,
% the integer part of the Nth root of K is R".
% Example: the integer part of the 2th root (square root) of 16 is 4.
% Example: the integer part of the 3rd root (cubic root) of 8 is 2.
% Example: the integer part of the 4th root
%of 16 is 2.
% Example: the integer part of the 4th root
%of 15 is 1.
% base case: the Nth root of 1 is always 1
nthRoot(N, K, R) :- R is floor(K**(1/N)).

%26
allSSSS(L):- subconjunto(L, S), sum_list(S, X), is_square(X), write(X), write("-"), write(S), nl.

is_square(X):- nthRoot(2, X, R), R*R =:= X.