/*
% Pizarra usada en las primeras dos clases de labo de Prolog, con ejemplos de Prolog

% 
% Una base de datos relacional es un conjunto de "relaciones". 
% Cada relación viene expresada mediante "tuplas".
%   por ejemplo, una relación "estudiante" puede tener las tuplas:
%    estudiante( juan,  21, barcelona, 'Carrer Aragó 453',    ... ). 
%    estudiante( pedro, 22, barcelona, 'Carrer Viladomat 4',  ... ). 
% 
% Si tenemos las relaciones de "padre" y de "hermano":
%  padre(juan,pedro).                  % el padre de juan es pedro
%  padre(maria,pedro).
%  ...                                 % n tuplas en la relación padre

%  hermano(pedro,vicente).             % el hermano de pedro es vicente
%  hermano(pedro,alberto).
%  ...                                 % m tuplas en la relación hermano
% 
% 
% ¿Cómo podríamos hacer la relación "tio"?  Con n x m tuplas?
%        malgastamos espacio (es información que en realidad ya tenemos!!)
%        problemas de consistencia....   restricciones de integridad
%
%  Solución: bases de datos deductivas
%
%  ponemos una regla:
%     forall S,T   tio(S,T) if exists P  such that padre(S,P) AND hermano(P,T)
%
%    diferentes maneras de expresar estas reglas ("cláusulas de Horn de LPO"):
%  
%          A S,T    tio(S,T)  <--    E P  (  padre(S,P)  &   hermano(P,T)  )
%          A S,T    tio(S,T)   v   - E P  (  padre(S,P)  &   hermano(P,T)  )     -E x p(x)   ===   A x  -p(x)
%          A S,T    tio(S,T)   v     A P -(  padre(S,P)  &   hermano(P,T)  )
%          A S,T    tio(S,T)   v     A P  ( -padre(S,P)  v  -hermano(P,T)  )
%          A S,T,P  tio(S,T)   v            -padre(S,P)  v  -hermano(P,T)
%
%                   tio(S,T)   v  -padre(S,P)  v  -hermano(P,T)        % en las cláusulas de LPO no escribimos los As
%  
%  en prolog:       tio(S,T):-     padre(S,P), hermano(P,T).   ("el tio de S es T si el padre de S es P y el hermano de P es T")
%
% programa Prolog === BD deductiva !!


%1 
padre(juan,pedro).      % el padre de juan es pedro
%2 
padre(maria,pedro).
%3 
hermano(pedro,vicente). % el hermano de pedro es vicente
%4 
hermano(pedro,alberto).
%5 
tio(S,T):- padre(S,P), hermano(P,T).          %el ámbito de las variables es la misma cláusula


%?- tio(juan,Z).
% La pila de backtracking:  cuando llama a hermano(pedro,T), usa la 3. y empila la 4.
% da la respuesta Z = vicente
% y si pido más respuestas (con el ;), entonces el backtracking usa lo empilado como alternativa, y usa la 4.


%?- tio(A,B)
% La pila de backtracking:  
%        cuando llama a padre(S,P), usa la 1. y empila la 2.
%        cuando llama a hermano(pedro,T), usa la 3. y empila la 4.
% da la respuesta:
%     A = juan
%     B = vicente
% en este momento la pila contiene  2,4
% y si pido más respuestas (con el ;), entonces el backtracking usa lo último empilado como alternativa, y usa la 4.
% da la respuesta:
%     A = juan
%     B = alberto
% en este momento la pila contiene  2
% y si pido más respuestas (con el ;), entonces el backtracking usa la 2
%        cuando llama a hermano(pedro,T), usa la 3. y empila la 4.
% da la respuesta:
%     A = maria
%     B = vicente
% en este momento la pila contiene  4
% y si pido más respuestas (con el ;), entonces el backtracking usa la 4
% da la respuesta:
%     A = maria
%     B = alberto
% en este momento la pila queda vacía y termina

%




% Lo que hay en una línea detrás del porcentaje % es un comentario.
% En este archivo pizarraLaboProlog.pl  hay muchos comentarios!

% En Prolog, lo que comienza por Mayúscula o por subrayado  _  son variables.
%
% Lo demás son términos;    juan,  hermano(X,juan),  f(X,a),   f(g(a),Z),   ...

% entramos (en linux) en el swi prolog con el comando swipl
% nos da mensaje de bienvenida y saca el prompt:
% ?-
%
% Ahora podemos decirle que lea este archivo, que se llama  pizarraLaboProlog.pl   :
% ?- [pizarraLaboProlog].
% después de esto, ya podemos hacer consultas:
% ?- padre(juan,X).      % esto es nuestro "objetivo"   (inglés: goal)
% ?- padre(Y,pedro).


% ¿Cómo hace el intérprete de Prolog para "ejecutar" un programa Prolog?
% Busca la primera cláusula en la base de datos cuya "cabeza" sea "unificable" con el objetivo:
%  "cabeza".
%  "cabeza" :- "cola".

%  "unificar" = dadas dos expresiones, dar valores a sus variables, para que sean iguales:

%  el simbolo  = significa "es unificable"

% | ?- f(X,a) = f(b,Y).
% | ?- f(f(X),a)  =  f(Y,Y).
% | ?- f(f(X),a) \=  f(Y,Y).
% | ?- f(f(X),a)  =  f(Y,Z).




%"listas"  [ a, b, f(a), g(f(a,X)), [c], X ]
% []                es la lista vacía
% [ a, b | L ]      la | separa los primeros elementos de la LISTA de los demás elementos

% ?- [a,b,c] = [X|L].

% nota: en realidad una lista [a,b,c] es una notación elegante para el término:   .( a, .(b, .(c,[]) ) )




% Prolog es programación DECLARATIVA. NO imperativa
% Esto hace que los programas sean versátiles: la misma definición nos sirve para muchos tipos de consultas.
% Por ejemplo, podemos declarar (definir) qué es pertenecer a una lista: pert(X,L) "X pertenece a la lista L"


% pert(X,L) = "X pertenece a la lista L"      (en realidad ya existe en swi Prolog, y se llama member)
pert(X, [X|_] ).
pert(X, [_|L] ):- pert(X,L).


%concat(L1,L2,L3) = "L3 es la concatenacion de L1 con L2"                     ya existe y se llama append
concat( [],     L,  L       ).
concat( [X|L1], L2, [X|L3] ):- concat( L1, L2, L3).

% aritmetica!!
% Var is Expresion   = "unifica el resultado de evaluar Expresion con Var"

%fact(N,F) = "F es el factorial de N"  F será  N * (N-1) * ... * 1
fact(0,1):- !.
fact(N,F):- N1 is N-1,  fact(N1,F1), F is N * F1.
% en fact(3,F):  para la 3a llamada recursiva con fact(0,...), usa la cláusula 1 y empila la 2!!

%long(L,N) = "la longitud de L es N"                     ya existe y se llama length
long([],0).
long([_|L],N):- long(L,N1), N is N1+1.


%permutacion(L,P) = "P es una permutacion de la lista L"     n!                     ya existe y se llama permutation

permutacion([],[]).
permutacion([X|L], P):- permutacion(L,P1),     
			concat( Pa,    Pb, P1),
			concat( Pa, [X|Pb], P).


%subcjto(L,S) = "S es un subconjunto de L"  2^n
% Si L es [e1 ... en-1 en]
%           0 ...  0    1  
%                       hay tantos subconjuntos como tiras de n bits
subcjto( [], [] ).
subcjto( [_|L],    S  ):-  subcjto(L,S).
subcjto( [X|L], [X|S] ):-  subcjto(L,S).


%cifras( L, N ) escribe las maneras de obtener N a partir de + - * /      de los elementos de la lista L
% ejemplo:
% ?- cifras( [4,9,8,7,100,4], 380 ).
%    4 * (100-7) + 8         <-------------
%    ((100-9) + 4 ) * 4
%    ...

cifras(L,N):-
    subcjto(L,S),         % S = [4,8,7,100]
    permutation(S,P),     % P = [4,100,7,8]
    expresion(P,E),       % E = 4 * (100-7) + 8 
    N is E,
    write(E), nl, fail.


% E = ( 4  *  (100-7) )    +    8
%            +
%          /   \
%         *     8
%        / \
%       4   -
%          / \
%        100  7


expresion([X],X).
expresion( L, E1 +  E2 ):- append( L1, L2, L), 
			  L1 \= [], L2 \= [],
			  expresion( L1, E1 ),
			  expresion( L2, E2 ).
expresion( L, E1 -  E2 ):- append( L1, L2, L), 
			  L1 \= [], L2 \= [],
			  expresion( L1, E1 ),
			  expresion( L2, E2 ).
expresion( L, E1 *  E2 ):- append( L1, L2, L), 
			  L1 \= [], L2 \= [],
			  expresion( L1, E1 ),
			  expresion( L2, E2 ).
expresion( L, E1 // E2 ):- append( L1, L2, L), 
			  L1 \= [], L2 \= [],
			  expresion( L1, E1 ),
			  expresion( L2, E2 ),
                          K is E2, K\=0.              % evitamos que se produzcan divisiones por cero



% Ejemplo para explicar el operador de corte "!":

p(1).                           %1
p(2).                           %2

q(a).                           %3
q(b).                           %4

r(3,4,5).                       %5
r(X,Y,Z):- p(X), q(Y), !, s(Z). %6    el corte ! quita de la pila las alternativas para p(..) q(..) y r(..)
r(5,6,7).                       %7

s(3).                           %8
s(4).                           %9

h(X,Y,Z):- r(X,Y,Z).            %10
% A B C      A B C  

h(a,b,c).                       %11

%% Se comporta así:
%% ?- h(A,B,C), write( [A,B,C] ), nl, fail.
%% [3,4,5]
%% [1,a,3]
%% [1,a,4]
%% [a,b,c]
%% false.



% der( Expr, Var, Der )  == "la derivada de Expr con respecto Var es Der"
der( X, X, 1):- !.
der( C, _, 0):- atomic(C).     % atomic significa que es una expresion constante o un entero
der( A+B, X, U+V ):- der(A,X,U), der(B,X,V). 
der( A*B, X, A*V+B*U ):- der(A,X,U), der(B,X,V). 
% ...


% union( L1, L2, U ) == "U es la union de L1 con L1 (como conjuntos, sin repeticiones)"
union( [],     L,  L ).
union( [X|L1], L2, U     ):-     member(X,L2),   union( L1, L2, U ).
union( [X|L1], L2, [X|U] ):- not(member(X,L2)),  union( L1, L2, U ).


% "not" es "negacion por fallo finito"  "negation by finite failure"
% en realidad el "not" ya está definido en swiprolog, pero si lo tuviéramos que definir, "minot", sería así:
minot( X ):- call(X), !, fail.       %minot( X ):-  X, !, fail.   <--- esto daría error sintáctico; por eso existe el "call"
minot( _ ).




% Haced también los ejercicios de Prolog de los exámenes finales más antiguos (de antes de que existiera el examen de labo).


*/


% ///////////////////////////////////////////// EJERCICIOS ////////////////////////////////////////////////////////////////


% 1:
	%%% Producto de los elementos de una lista
	
    prod([], 1).    % si la llista està buida, la solució és 1
    % prod([X], X).   % si la llista té un element, la solució és aquest element
    prod([X|List], P):- prod(List, P1), P is X*P1.    % si la llista té més elements, els multiplica
    
    % execució: prod([1,2,3], L). --> L = [6].


% -------------------------------------------------------------------------------------------------------------------------

% 2:
	%%% -- Producto escalar de dos vectores --
	
    prescalar([], [], 0).
    prescalar([X|L1], [Y|L2], P):- prescalar(L1, L2, P1), P is P1+X*Y.
    
    % execució: prescalar([1,2], [3, 4], P) --> P = 1*3 + 2*4 = 11


% -------------------------------------------------------------------------------------------------------------------------

% 3:
	%%% -- Unión de dos listas --
	
	union([], L, L).
	union([X|L1], L2, L3):- member(X, L2), !, union(L1, L2, L3). 
	union([X|L1], L2, [X|L3]):- union(L1, L2, L3).
	
	% execució: union([2,3], [4,5], L). --> L = [2,3,4,5].
	
	%%% -- Interseccion de dos listas --
	
	%interseccion(L1, L2, L).
	interseccion([], _, []).
	interseccion([X|L1], L2, [X|L3]):- member(X, L2), !, remove_element(X, L2, NewL2), interseccion(L1, NewL2, L3).
    interseccion([_|L1], L2, L3):- interseccion(L1, L2, L3).
    
    % execució: interseccio([1,2,3], [2,1,4], L). --> L = [1,2].
    
    remove_element(X, L, NewL):- append(L1, [X|L2], L), append(L1, L2, NewL).

		
% -------------------------------------------------------------------------------------------------------------------------

%4:
	%%% -- Último elemento de una lista --
	
	lastElement(L, X):- append(_, [X], L).
	
	% execució: lastElement([2,4,5,6,6,2,3,5,6,7,2,3,4], X). --> X = 4.
    
    %%% -- Lista inversa de una lista --
    
    reverseList([], []).
    reverseList(L, [X|RL]):- lastElement(L, X), removeLast(L, NewL), reverseList(NewL, RL).
    
    % execució: reverseList([1,2,3,4,2], L). --> L = [2,4,3,2,1]. 
    
    removeLast(L, NewL):- append(NewL, [_], L).


% -------------------------------------------------------------------------------------------------------------------------

% 5:
	%%% -- fib(N,F) N-ésimo número de Fibonacci --
	
	fib(1, 1).
    fib(2, 1).
    fib(N, F):- N1 is N - 1, N2 is N - 2, 
                fib(N1, F1), fib(N2, F2),
                F is F1 + F2.
                
    % execució: fib(3, F). --> F = 2.  (fibonacci: 1, 1, 2, 3, 5, 8, ...)


% -------------------------------------------------------------------------------------------------------------------------

% 6:
	%%% -- dados(P,N,L) --> todas posibles soluciones de máximo N elementos que sumen P --
	
	dados(0, 0, []).
    dados(P, N, [X|L]):- N > 0,  member(X, [1,2,3,4,5,6]), N1 is N - 1, P1 is P - X, dados(P1, N1, L).
    
    % execució: dados(30, 6, L). --> L = [1, 5, 6, 6, 6, 6].
	
	
% -------------------------------------------------------------------------------------------------------------------------

% 7:
	%%% -- Indica si un elemento es el resultado de la suma de los demas elementos --
	
	concat([],L,L).
	concat([X|L1],L2,[X|L3]) :- concat(L1,L2,L3).

	pert_con_resto(X,L,R) :- concat(L1,[X|L2],L), concat(L1,L2,R). 
	
	suma([],0).
	suma([X|L],S) :- suma(L,S1), S is S1+X.
	
	suma_demas(L) :- pert_con_resto(X,L,R), suma(R,X), !. % si encontramos uno basta
	
	% execucion: suma_demas([1,2,3]). --> true.


% -------------------------------------------------------------------------------------------------------------------------

% 8:
	%%% -- Indica si un elemento es el resultado de la suma de los elementos anteriores a él --
	
	suma_ants(L):- append(L1, [X|_], L), suma(L1, X).
	
	% execució: suma_ants([1,2,4,7,3]). --> true.


% -------------------------------------------------------------------------------------------------------------------------

% 9: 
	%%% -- Cuantas veces aparece cada elemento de la lista en ella	
	
	card([], []).
	card([X1|L1], [[X1,N1] | Cr]):- card(L1, C), pert_con_resto([X1,N], C, Cr), N1 is N + 1.
	card([X1|L1], [[X1, 1] | C ]):- card(L1, C).
	
	card(L1):- card(L1, C), write(C).
	
	% execucio: card([1,1,4,2,1,2]). --> [[1,3], [4,1], [2,2]].


% -------------------------------------------------------------------------------------------------------------------------

% 10:
	%%% -- Indica si una llista esta ordenada --

	esta_ordenada([]).  % una llista buida esta ordenada
	esta_ordenada([_]).  % es va fent la comprovacio de que el element anterior es menor al següent, fins que hem comprovat tota la llista i queda buida
	esta_ordenada([X1, X2|List]):- X1 < X2, esta_ordenada([X2|List]). 
	
	% execució: esta_ordenada([1,2,4,7]). --> true.


% -------------------------------------------------------------------------------------------------------------------------

% 11:
	%%% -- Ordena la lista de menor a mayor --
	
	ord([],[]).
	ord(L1, L2):- permutation(L2, L1), esta_ordenada(L2).
	
	% execucio: ord([4,1,5,3], L). --> L = [1,3,4,5].


% -------------------------------------------------------------------------------------------------------------------------

% 12:
	%%% -- Dado un diccionario A de símbolos, escribe todas las palabras de N símbolos por orden alfabético --
	
	diccionario(A,N):-  nperts(A,N,S), escribir(S), fail.

	nperts(_,0,[]):-!.
	nperts(L,N,[X|S]):- pert(X,L), N1 is N-1, nperts(L,N1,S).
		% pert(X,L) --> mira que X pertanyi a L
		% si és cert, la mida de N disminueix un valor (fins que valgui 0) i
		% 			  comprovem si es compleix el mateix per la resta de la solució


	pert(X,[X|_]). % mira si X es el primer element d'aquesta llista
	pert(X,[_|L]):- pert(X,L). % sino mira si X pertany a la resta de la llista

	escribir([]):-write(' '),nl,!.
	escribir([X|L]):- write(X), escribir(L).

	% execucio: diccionario([ga, chu, le], 2). --> gaga, gachu, gale, chuga, chuchu, chule, lega, lechu, lele.


% -------------------------------------------------------------------------------------------------------------------------

% 13:
	%%% -- Palindromos: escriu totes les possibles capicues --
	
	palindromos(L) :- setof(P, (permutation(L,P), es_palindromo(P)), S), write(S).  % setof per no tenir solucions repetides
	palindromos(_). 

	es_palindromo([]).
	es_palindromo([_]) :- !. % regla adecuada
	es_palindromo([X|L]) :- concat(L1,[X],L), es_palindromo(L1). 
	
	% execucio: palindromos([a,c,a,c]). --> [[a,c,c,a], [c,a,a,c]].


% -------------------------------------------------------------------------------------------------------------------------

% 14:
	%%% -- per quins valors de les lletres es compleix:
	%				SEND + MORE = MONEY					--
	
	suma([],[],[],C,C).
	suma([X1|L1],[X2|L2],[X3|L3],Cin,Cout) :-
	X3 is (X1 + X2 + Cin) mod 10,
	C  is (X1 + X2 + Cin) //  10,
	suma(L1,L2,L3,C,Cout).

send_more_money1 :-

	L = [S, E, N, D, M, O, R, Y, _, _],
	permutation(L, [0,1,2,3,4,5,6,7,8,9]),
	suma([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

	write('S = '), write(S), nl,
	write('E = '), write(E), nl,
	write('N = '), write(N), nl,
	write('D = '), write(D), nl,
	write('M = '), write(M), nl,
	write('O = '), write(O), nl,
	write('R = '), write(R), nl,
	write('Y = '), write(Y), nl,
	write('  '), write([S,E,N,D]), nl,
	write('  '), write([M,O,R,E]), nl,
	write('-------------------'), nl,
	write([M,O,N,E,Y]), nl.
	
send_more_money2 :-

	L = [0,1,2,3,4,5,6,7,8,9],
	pert_con_resto(M,  [0,1], _),
	pert_con_resto(M,  L,  L0),
	pert_con_resto(O, L0, L1),
	pert_con_resto(R, L1, L2),
	pert_con_resto(Y, L2, L3),
	pert_con_resto(S, L3, L4),
	pert_con_resto(E, L4, L5),
	pert_con_resto(N, L5, L6),
	pert_con_resto(D, L6, _),
	suma([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

	write('S = '), write(S), nl,
	write('E = '), write(E), nl,
	write('N = '), write(N), nl,
	write('D = '), write(D), nl,
	write('M = '), write(M), nl,
	write('O = '), write(O), nl,
	write('R = '), write(R), nl,
	write('Y = '), write(Y), nl,
	write('  '), write([S,E,N,D]), nl,
	write('  '), write([M,O,R,E]), nl,
	write('-------------------'), nl,
	write([M,O,N,E,Y]), nl.

	% -----------------------------------------------------

	sendMoreMoney:-  Letters = [S, E, N, D, M, O, R, Y, _, _],
                                Numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                                permutation(Letters, Numbers),
                                S1 is 1000 * S + 100 * E + 10 * N + D +
                                         1000 * M + 100 * O + 10 * R + E,
                                S1 is 10000 * M + 1000 * O + 100 * N + 10 * E + Y,
                                writeLetters(Letters), ! .
                                
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

	

% -------------------------------------------------------------------------------------------------------------------------

% 15:
	%%% -- Simplifica --
	
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


% -------------------------------------------------------------------------------------------------------------------------

% 16 -- Escriu la cadena de domino amb totes les fitxes o indica que no és possible --

	dom(L) :- p(L,P), ok(P), write(P), nl.
	dom(_) :- write("no hay cadena"), nl.

	%Comprova que P és permutació de L
	p([],[]).
	p(L,[X|P]) :- select(X,L,R), p(R,P).
	p(L,[f(X,Y)|P]) :- select(f(Y,X),L,R), p(R,P).

	%Comprova que P és una cadena de domino
	ok([f(_,_)]).
	ok([f(_,F2),f(S1,S2)|P]) :- F2 = S1, ok([f(S1,S2)|P]).

	%execucio: dom([f(3,4), f(3,2)]). --> [f(4,3), f(3,2)].


% -------------------------------------------------------------------------------------------------------------------------

% 17:

	p:- readclauses(F), sat([],F).
	p:- write("UNSAT"),nl.
	sat(I,[]):- write("IT IS SATISFIABLE. Model: "), write(I),nl,!.
	sat(I,F):-
		%write("sat"-I-F), nl,
		decisionlit(F,Lit), % Select unit clause if any; otherwise, an arbitrary one.
		simplif(Lit,F,F1), % Simplifies F. Warning: may fail and cause backtracking
		sat([Lit|I], F1), !.

	decisionlit([X|_], Lit) :-
		%write("d"),
		X = [Lit|_].

	simplif(_, [], []).
	simplif(Lit, [X|F], F1) :-
		member(Lit, X),
		%write("  simp1" - Lit - X - F), nl,
		simplif(Lit, F, F1).

	simplif(Lit, [X|F], F2) :-
		LitN is 0 - Lit,
		pert_con_resto(LitN, X, R),
		%write("  simp2" - Lit - X - F), nl,
		append(F1, [R], F2),
		simplif(Lit, F, F1).

	simplif(Lit, [X|F], F2) :-
		not(member(Lit, X)),
		LitN is 0 - Lit, not(member(LitN, X)),
		%write("  simp3" - Lit - X - F), nl,
		append(F1, [X], F2),
		simplif(Lit, F, F1).

	% execucio: sat([], [[1,2], [-1,2], [1,-2]]). --> IT IS SATISFACTIBLE. Model: [2,1].


% -------------------------------------------------------------------------------------------------------------------------

% 18: -- percentatge cancer de pulmo --

	num(X):- between(1, 10, X).

	smoker:- num(FC1), num(FNC1), num(NFC1), num(NFNC1),
		10 is FC1 + FNC1 + NFC1 + NFNC1,
		FC1 / (FC1 + FNC1) > NFC1 / (NFC1 + NFNC1),
		num(FC2), num(FNC2), num(NFC2), num(NFNC2),
		10 is FC2 + FNC2 + NFC2 + NFNC2,
		FC2 / (FC2 + FNC2) > NFC2 / (NFC2 + NFNC2),
		(FC1+FC2) / (FC1+FNC1+FC2+FNC2) < (NFC1+NFC2) / (NFC1+NFNC1+NFC2+NFNC2),
		write([FC1, FNC1, NFC1, NFNC1, ' ', FC2, FNC2, NFC2, NFNC2]), nl, fail.


% -------------------------------------------------------------------------------------------------------------------------

% 19: -- maq(L,C,M): M retorna la quatitat C amb el menor nombre de monedes de la llista L -- 

	maq(V,C,M) :-
		length(V,VL),
		length(M,VL),
		between(0,C,MIN),
		lessThan(MIN,M),
		maqSum(V,C,M),
		write(M), write("\n"), halt.

	maqSum(_,0,[]).
	maqSum([VF|V],C,[LF|L]) :-
		LF >= 0,
		C >= 0,
		C2 is C - VF*LF,
		maqSum(V,C2,L).

	lessThan(_,[]).
	lessThan(MAX,[F|L]) :-
		between(0,MAX,F),
		MAX2 is MAX - F,
		lessThan(MAX2,L).


% -------------------------------------------------------------------------------------------------------------------------

% 20: -- "aplana": treu [ ] --

	flatten([],[]) :- !.
	flatten([X|L],R) :-
		flatten(X,XF),
		flatten(L,LF),
		append(XF,LF,R), !.
	flatten(X,[X]).

	% execucio: flatten([ a, [], b, [[c]] ], F). --> F = [a,b,c].


% -------------------------------------------------------------------------------------------------------------------------

% 21: -- L es la parte entera del logaritmo de N en base B --

	log(_, 1, 0) :- !.
	log(B, N, L) :- N1 is div(N,B), log2(B, N1, L1), L is L1+1.

	% execucio: log(2, 1020, L). --> L = 9.


% -------------------------------------------------------------------------------------------------------------------------

% 22: -- matricula M alumnos que no se copian -- 

	li(N,M,L,S):- genera_n(N, LN), genera_solucio(LN, M, S), es_solucio(S, L), !.

	genera_n(N, N2):- genera_n_invers(N, N1), inversa(N1, N2).

	genera_n_invers(0, []):- !.
	genera_n_invers(N, [N|LN]):- N1 is N - 1, genera_n_invers(N1, LN).

	genera_solucio(_, 0, []):- !.
	genera_solucio(N, M, [X|S]):- pert_con_resto(X, N, N1), M1 is M - 1, genera_solucio(N1, M1, S).

	es_solucio(_, []):- !.
	es_solucio(S, [FL|L]):- FL = [X, Y], nand(member(X, S), member(Y, S)), es_solucio(S, L).


% -------------------------------------------------------------------------------------------------------------------------
% -------------------------------------------------------------------------------------------------------------------------

% ----- EXTRES -----

% 23 -- Ordenación por fusión -> Merge sort. --

	split([],[],[]).
	split([A],[A],[]).
	split([A,B|R],[A|Ra],[B|Rb]) :-  split(R,Ra,Rb).

	mergeSort([], []):- !.
	mergeSort([L], [L]):- !.
	mergeSort(List, Res):- split(List, L1, L2), % si aqui faig servir append, em dóna out of local stack
                                    	mergeSort(L1, L1sort),
                                    	mergeSort(L2, L2sort),
	                                    merge(L1sort, L2sort, Res).
                                    
	merge(L, [], L):- !.                                
	merge([], L, L):- !.
	merge([L1|List1], [L2|List2], [L1|Res]):- L1 =< L2, 
                                                                merge(List1, [L2|List2], Res).
	merge([L1|List1], [L2|List2], [L2|Res]):- merge([L1|List1], List2, Res).

	% execucio: mergeSort([3,5,7,2,8,4], L). --> L = [2,3,4,5,7,8].


% -------------------------------------------------------------------------------------------------------------------------

% 24 -- Ordenación por inserción --

	ordenacionInsercion( [], [] ).
	ordenacionInsercion([L|L1], L2):- ordenacionInsercion(L1, L1ord),
                                                     insert(L, L1ord, L2).
                                                     
	insert(L, [], [L]).
	insert(L, [K|List], [L, K|List]):- L =< K.
	insert(L, [K|List], [K|Res]):- L > K,  insert(L, List, Res).

	% execucio: ordenacionInsercion([2,5,4,6], L). --> L = [2,4,5,6].



