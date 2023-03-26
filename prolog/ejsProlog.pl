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