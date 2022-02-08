% Jogo dos 8

% Estado inicial: [2,8,3,1,0,4,7,6,5]
% Representação Matricial do Estado Inicial
% | 2 8 3 |
% | 1 0 4 |
% | 7 6 5 | 
% 
% Estado objetivo: [1,2,3,8,0,4,7,6,5]
% Representação Matricial do Estado Objetivo
% | 1 2 3 |
% | 8 0 4 |
% | 7 6 5 | 
% 
% Função de avaliação - h(n)
% Soma das diferenças: 1+6+0+7+0+0+0+0+0 = 14
% • é escolhido o operador que gerar a menor
% diferença depois de aplicado
% • O objetivo é alcançado quando a soma das
% diferenças for igual a zero.

% Objetivo do jogo
% | 1 2 3 |
% | 8 0 4 |
% | 7 6 5 |
objetivo([1,2,3,8,0,4,7,6,5]).

% Operações Possíveis
mover([A,B,C,D,E,F,G,H,0], S) :- S = [A,B,C,D,E,F,G,0,H].
mover([A,B,C,D,E,F,G,H,0], S) :- S = [A,B,C,D,E,0,G,H,F].
mover([A,B,C,D,E,F,G,0,H], S) :- S = [A,B,C,D,E,F,0,G,H].
mover([A,B,C,D,E,F,G,0,H], S) :- S = [A,B,C,D,0,F,G,E,H].
mover([A,B,C,D,E,F,G,0,H], S) :- S = [A,B,C,D,E,F,G,H,0].
mover([A,B,C,D,E,F,0,G,H], S) :- S = [A,B,C,0,E,F,D,G,H].
mover([A,B,C,D,E,F,0,G,H], S) :- S = [A,B,C,D,E,F,G,0,H].
mover([A,B,C,D,E,0,F,G,H], S) :- S = [A,B,C,D,0,E,F,G,H].
mover([A,B,C,D,E,0,F,G,H], S) :- S = [A,B,0,D,E,C,F,G,H].
mover([A,B,C,D,E,0,F,G,H], S) :- S = [A,B,C,D,E,H,F,G,0].
mover([A,B,C,D,0,E,F,G,H], S) :- S = [A,B,C,0,D,E,F,G,H].
mover([A,B,C,D,0,E,F,G,H], S) :- S = [A,0,C,D,B,E,F,G,H].
mover([A,B,C,D,0,E,F,G,H], S) :- S = [A,B,C,D,E,0,F,G,H].
mover([A,B,C,D,0,E,F,G,H], S) :- S = [A,B,C,D,G,E,F,0,H].
mover([A,B,C,0,D,E,F,G,H], S) :- S = [0,B,C,A,D,E,F,G,H].
mover([A,B,C,0,D,E,F,G,H], S) :- S = [A,B,C,D,0,E,F,G,H].
mover([A,B,C,0,D,E,F,G,H], S) :- S = [A,B,C,F,D,E,0,G,H].
mover([A,B,0,C,D,E,F,G,H], S) :- S = [A,0,B,C,D,E,F,G,H].
mover([A,B,0,C,D,E,F,G,H], S) :- S = [A,B,E,C,D,0,F,G,H].
mover([A,0,B,C,D,E,F,G,H], S) :- S = [0,A,B,C,D,E,F,G,H].
mover([A,0,B,C,D,E,F,G,H], S) :- S = [A,B,0,C,D,E,F,G,H].
mover([A,0,B,C,D,E,F,G,H], S) :- S = [A,D,B,C,0,E,F,G,H].
mover([0,A,B,C,D,E,F,G,H], S) :- S = [A,0,B,C,D,E,F,G,H].
mover([0,A,B,C,D,E,F,G,H], S) :- S = [C,A,B,0,D,E,F,G,H].

% Subtrai os valores dos elementos de duas listas dois a dois
lista_sub([],[],[]).
lista_sub([H1| T1], [H2| T2], [ResH| ResT]) :- 
        lista_sub(T1, T2, ResT),
        ResH is abs(H1 - H2).

% Somatorio dos valores de uma lista
lista_somar_todos([], 0).
lista_somar_todos([H|T], Sum) :-
   lista_somar_todos(T, Rest),
   Sum is H + Rest.
  
% h(n)
avaliacao(Lista, Resultado) :-
    objetivo(X),
    lista_sub(Lista, X, ListaDiferenca),
    lista_somar_todos(ListaDiferenca, Resultado).

% Estende os caminhos
estende([GC,_,_,Lista|Caminho],NovosCaminhos):-
    findall([GNovo, HNovo, FNovo, NovaLista,Lista|Caminho],
    (	
    	mover(Lista, NovaLista),
        avaliacao(NovaLista, HNovo),
        not(member(NovaLista,[Lista|Caminho])),
        GNovo is GC + 1,
        FNovo is GNovo + HNovo),
    NovosCaminhos).

% Caso Base
jogo([[G,_,_,Lista|Caminho]|_], [G,S]) :-
	objetivo(Lista),
	reverse([Lista|Caminho], S),!.

% Chamada Recursiva
jogo([Caminho|Caminhos], S) :-
    estende(Caminho, CaminhosPossiveis),    
    append(Caminhos, CaminhosPossiveis, ListaCaminhos),
    ordena(ListaCaminhos, CaminhosOrdenados),
    jogo(CaminhosOrdenados, S),!. 

% Verifica se é possível chegar ao objetivo com a entrada dada
% Conta inversões
%possivel(Lista) :-
%    10 < 11.

% Entry Point
app(Lista, P) :-
%    possivel(Lista),
    jogo([[0,0,0,Lista]], P).

% Algoritmo de ordenacao
quicksort([],[]).
quicksort([X|Cauda],ListaOrd):-
	particionar(X,Cauda,Menor,Maior),
	quicksort(Menor,MenorOrd),
	quicksort(Maior,MaiorOrd),
	append(MenorOrd,[X|MaiorOrd],ListaOrd).

particionar(_,[],[],[]).
particionar(X,[Y|Cauda],[Y|Menor],Maior):-
	maior(X,Y),!,
	particionar(X,Cauda,Menor,Maior).
particionar(X,[Y|Cauda],Menor,[Y|Maior]):-
	particionar(X,Cauda,Menor,Maior).

maior([_,_,F1|_],[_,_,F2|_]) :- F1 > F2.

ordena(Caminhos,CaminhosOrd):-
	quicksort(Caminhos,CaminhosOrd).

% app([2,8,3,1,6,4,7,5,0], S).
