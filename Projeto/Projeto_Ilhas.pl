:- [codigo_comum].
%Bernardo Galante LEIC-T 1o Ano
/*
---------------------------Funcao_extrai_ilhas_linha_1----------------------------------
----------------------------------------------------------------------------------------
extrai_ilhas_linha(N_L, Linha, Ilhas), em que N_L eh um inteiro positivo, correspondente 
ao numero de uma linha e Linha eh uma lista correspondente a uma linha de um puzzle, 
significa que Ilhas eh a lista ordenada (ilhas da esquerda para a direita)
cujos elementos sao as ilhas da linha Linha.
----------------------------------------------------------------------------------------
*/
%implementacao_iterativa
extrai_ilhas_linha(N_L, Linha, Ilhas):- 
    extrai_ilhas_linha(N_L, Linha, Ilhas, [], 1).

%base_da_recursao
extrai_ilhas_linha(_,[], Ilhas, Ilhas,_).


%Quando_aparece_um_0_ignorar
extrai_ilhas_linha(N_L,[Cabeca | Resto], Ilhas, Aux, Coluna) :- 
     Cabeca is 0,
     ColunaNova is Coluna + 1,
     extrai_ilhas_linha(N_L, Resto, Ilhas, Aux, ColunaNova).

%Quando_aparece_um_numero_maior_que_0
extrai_ilhas_linha(N_L,[Cabeca | Resto], Ilhas, Aux, Coluna) :- 
    Cabeca > 0,
    append(Aux,[ilha(Cabeca,(N_L, Coluna))],NovoAux),
    ColunaNova is Coluna + 1,
    extrai_ilhas_linha(N_L, Resto, Ilhas, NovoAux, ColunaNova).


/*
--------------------------------------Funcao_Ilhas_2------------------------------------
----------------------------------------------------------------------------------------
ilhas(Puz, Ilhas), em que Puz eh um puzzle, significa que Ilhas eh a lista ordenada
(ilhas da esquerda para a direita e de cima para baixo) cujos elementos sao as ilhas de
Puz.
----------------------------------------------------------------------------------------
*/
%implementacao_iterativa
ilhas(Puz, Ilhas) :- 
    ilhas(1, Puz, Ilhas,[]).
    
%base_da_recursao
ilhas(_, [], Ilhas, Ilhas).

ilhas(N_L, [Head | Tail], Ilhas_de_N_L, Aux) :-
    extrai_ilhas_linha(N_L, Head, Ilhas),
    append(Aux, Ilhas, Novo_Aux),
    Novo_N_L is N_L + 1,
    ilhas(Novo_N_L, Tail, Ilhas_de_N_L, Novo_Aux).

/*
----------------------------------Funcoes_Auxiliares------------------------------------
----------------------------------------------------------------------------------------
Devolve o ultimo/primeiro de uma Lista, ou [] caso a Lista seja ja Vazia
----------------------------------------------------------------------------------------
*/
%Funcao Auxiliar ultimo elemento
ultimo_elemento(Lista,Ultimo):- ultimo_elemento(Lista,Ultimo, []).
ultimo_elemento([],Ultimo, Ultimo).
ultimo_elemento(Lista,Ultimo,Aux):-
    length(Lista, X),
    X = 0,                                    %Devolve Lista vazia caso o argumento Lista seja uma lista vazia
    ultimo_elemento([],Ultimo,Aux).           
ultimo_elemento(Lista,Ultimo,Aux):-
    length(Lista, X),
    X > 0,
    nth1(X,Lista,UltimoEle),
    append(Aux, [UltimoEle], Aux_final),
    ultimo_elemento([],Ultimo, Aux_final).

%Funcao Auxiliar primeiro elemento
primeiro_elemento(Lista,Primeiro):- primeiro_elemento(Lista,Primeiro, []).
primeiro_elemento([],Primeiro, Primeiro).
primeiro_elemento(Lista,Primeiro,Aux):-
    length(Lista, X),
    X = 0,
    primeiro_elemento([],Primeiro,Aux).
primeiro_elemento(Lista,Primeiro,Aux):-       %Devolve Lista vazia caso o argumento Lista seja uma lista vazia
    length(Lista, X),
    X > 0,
    nth1(1,Lista,PrimeiroEle),
    append(Aux, [PrimeiroEle], Aux_final),
    primeiro_elemento([],Primeiro, Aux_final).



/*
-----------------------------Funcao_Ilhas_Vizinhas_3------------------------------------
----------------------------------------------------------------------------------------
vizinhas(Ilhas, Ilha, Vizinhas), em que Ilhas eh a lista de ilhas de um puzzle
e Ilha eh uma dessas ilhas, significa que Vizinhas eh a lista ordenada (ilhas de cima para
baixo e da esquerda para a direita ) cujos elementos sao as ilhas vizinhas de Ilha.
----------------------------------------------------------------------------------------
*/
vizinhas(Ilhas, Ilha, Vizinhas):- 
    vizinhas(Ilhas, Ilha, Vizinhas, [],   []  ,      []  ,   [] ,  []).
                                       %   ^         ^        ^     ^ 
                                        %Esquerda  Direita   Cima  Baixo


vizinhas([], _, Vizinhas,Aux_principal, Aux_esquerda, Aux_direita, Aux_cima, Aux_baixo):-
    ultimo_elemento(Aux_esquerda,Ultimo_E),
    ultimo_elemento(Aux_cima,Ultimo_C),
    primeiro_elemento(Aux_baixo,Primeiro_B),
    primeiro_elemento(Aux_direita,Primeiro_D),
    append(Aux_principal, Ultimo_C, Novo_aux1), 
    append(Novo_aux1, Ultimo_E, Novo_aux2),
    append(Novo_aux2, Primeiro_D, Novo_aux3),
    append(Novo_aux3, Primeiro_B, Novo_aux4),
    findall(X,(member(X,Novo_aux4)),Vizinhas).

%LISTAS ESQUERDA BAIXO CIMA DIREITA---------------------------------------------------------------------------
vizinhas([H|T],Ilha,Vizinhas,Aux_principal,Aux_esquerda, Aux_direita, Aux_cima, Aux_baixo):-
    Ilha =  ilha(_, (Xi, Yi)),
    H = ilha(_, (Xv, Yv)),                  %Mesma Linha para a esquerda (Yilha > Yvizinha)
    Xi = Xv,
    Yi > Yv,
    append(Aux_esquerda, [H], Novo_aux_esquerda),
    vizinhas(T,Ilha,Vizinhas,Aux_principal,Novo_aux_esquerda, Aux_direita, Aux_cima, Aux_baixo).


vizinhas([H|T],Ilha,Vizinhas,Aux_principal,Aux_esquerda, Aux_direita, Aux_cima, Aux_baixo):-
    Ilha =  ilha(_, (Xi, Yi)),
    H = ilha(_, (Xv, Yv)),                  %Mesma Linha para a Direita (Yilha < Yvizinha)
    Xi = Xv,
    Yi < Yv,
    append(Aux_direita, [H], Novo_aux_direita),
    vizinhas(T,Ilha,Vizinhas,Aux_principal,Aux_esquerda, Novo_aux_direita, Aux_cima, Aux_baixo).


vizinhas([H|T],Ilha,Vizinhas,Aux_principal,Aux_esquerda, Aux_direita, Aux_cima, Aux_baixo):-
    Ilha =  ilha(_, (Xi, Yi)),
    H = ilha(_, (Xv, Yv)),                  %Mesma Coluna para Cima (Xilha > Xvizinha)
    Xi > Xv,
    Yi = Yv,
    append(Aux_cima, [H], Novo_aux_cima),
    vizinhas(T,Ilha,Vizinhas,Aux_principal,Aux_esquerda, Aux_direita, Novo_aux_cima, Aux_baixo).
    
vizinhas([H|T],Ilha,Vizinhas,Aux_principal,Aux_esquerda, Aux_direita, Aux_cima, Aux_baixo):-
    Ilha =  ilha(_, (Xi, Yi)),
    H = ilha(_, (Xv, Yv)),                  %Mesma coluna para baixo (Xilha < Xvizinha)
    Xi < Xv,
    Yi = Yv,
    append(Aux_baixo, [H], Novo_aux_baixo),
    vizinhas(T,Ilha,Vizinhas,Aux_principal,Aux_esquerda, Aux_direita, Aux_cima, Novo_aux_baixo).

vizinhas([H|T],Ilha,Vizinhas,Aux_principal,Aux_esquerda, Aux_direita, Aux_cima, Aux_baixo):-
    Ilha =  ilha(_, (Xi, Yi)),
    H = ilha(_, (Xv, Yv)),                  %Coluna e Linha Diferentes
    not(Xi = Xv),
    not(Yi = Yv),
    vizinhas(T,Ilha,Vizinhas,Aux_principal,Aux_esquerda, Aux_direita, Aux_cima, Aux_baixo).

vizinhas([H|T],Ilha,Vizinhas,Aux_principal,Aux_esquerda, Aux_direita, Aux_cima, Aux_baixo):-
    Ilha =  ilha(_, (Xi, Yi)),
    H = ilha(_, (Xv, Yv)),                  %Mesma Posicao
    Xi = Xv,
    Yi = Yv,
    vizinhas(T,Ilha,Vizinhas,Aux_principal,Aux_esquerda, Aux_direita, Aux_cima, Aux_baixo).

/*
----------------------------------Funcao_Estado_4---------------------------------------
----------------------------------------------------------------------------------------
estado(Ilhas, Estado), em que Ilhas eh a lista de ilhas de um puzzle, significa que
Estado eh a lista ordenada cujos elementos sao as entradas referentes a cada uma das
ilhas de Ilhas.
Uma entrada eh uma lista em que:
O 1o elemento eh uma ilha.
O 2o elemento eh lista de vizinhas dessa ilha.
O 3o elemento eh a lista de pontes da ilha; esta lista eh vazia no estado inicial.
----------------------------------------------------------------------------------------
*/
estado(Ilhas,Estado):- estado(Ilhas,Ilhas,Estado,[],[]).
estado([],_,Estado,_,Estado).
estado([H|T],Ilhas,Estado,Aux, Aux_final):-
    vizinhas(Ilhas, H, Vizinhas_de_H),
    append(Aux, [H], Aux1),
    append(Aux1, [Vizinhas_de_H], Aux2),
    append(Aux2, [[]], Aux3),
    append(Aux_final, [Aux3], Novo_Aux_final),
    estado(T,Ilhas,Estado, [], Novo_Aux_final).


/*
-----------------------------Funcao_posicoes_entre_5------------------------------------
----------------------------------------------------------------------------------------
posicoes_entre(Pos1, Pos2, Posicoes), em que Pos1 e Pos2 sao posicoes, significa 
que Posicoes eh a lista ordenada de posicoes entre Pos1 e Pos2 (excluindo Pos1 e
Pos2). Se Pos1 e Pos2 nao pertencerem ah mesma linha ou ah mesma coluna, o resultado
eh false.
----------------------------------------------------------------------------------------
*/
%Mesma_Coluna_L1>L2:
posicoes_entre((L1,C1), (L2,C2), Posicoes):-
    C1 = C2,
    L1>L2,
    Novo_L2 is L2 + 1,
    Novo_L1 is L1 - 1,
    bagof(X,between(Novo_L2,Novo_L1,X),Lista),
    findall((X,C1), member(X,Lista), Posicoes).

%Mesma_Coluna_L2>L1:
posicoes_entre((L1,C1), (L2,C2), Posicoes):-
    C1 = C2,
    L2>L1,
    Novo_L1 is L1 + 1,
    Novo_L2 is L2 - 1,
    bagof(X,between(Novo_L1,Novo_L2,X),Lista),
    findall((X,C1), member(X,Lista), Posicoes).

%Mesma_Linha_C1>C2:
posicoes_entre((L1,C1), (L2,C2), Posicoes):-
    L1 = L2,
    C1>C2,
    Novo_C2 is C2 + 1,
    Novo_C1 is C1 - 1,
    bagof(X,between(Novo_C2,Novo_C1,X),Lista),
    findall((L1,X), member(X,Lista), Posicoes).

%Mesma_Linha_C2>C2:
posicoes_entre((L1,C1), (L2,C2), Posicoes):-
    L1 = L2,
    C2>C1,
    Novo_C1 is C1 + 1,
    Novo_C2 is C2 - 1,
    bagof(X,between(Novo_C1,Novo_C2,X),Lista),
    findall((L1,X), member(X,Lista), Posicoes).


/*
---------------------------------Funcao_Cria_Ponte_6------------------------------------
----------------------------------------------------------------------------------------
cria_ponte(Pos1, Pos2, Ponte), em que Pos1 e Pos2 sao 2 posicoes, significa
que Ponte eh uma ponte entre essas 2 posicoes.
----------------------------------------------------------------------------------------
*/

%(X1, Y1), (X1, Y2)
cria_ponte((X1,Y1),(X2,Y2),Ponte):-
    X1 = X2,                                                            
    findall((ponte((X1, Y3),(X2, Y4))),(Y3 is min(Y1, Y2), Y4 is max(Y1,Y2)),Lista_Ponte),    %Ponte Vertical
    member(Ponte,Lista_Ponte).

%(X1, Y1), (X2, Y1)
cria_ponte((X1,Y1),(X2,Y2),Ponte):-
    Y1 = Y2,
    findall((ponte((X3, Y1),(X4, Y2))),(X3 is min(X1, X2), X4 is max(X1,X2)),Lista_Ponte),    %Ponte Horizontal
    member(Ponte, Lista_Ponte).


/*
------------------------------Funcao_caminho_livre_7------------------------------------
----------------------------------------------------------------------------------------
caminho_livre(Pos1, Pos2, Posicoes, I, Vz), em que Pos1 e Pos2 sao posicoes, 
Posicoes eh a lista ordenada de posicoes entre Pos1 e Pos2, I eh uma ilha, e Vz
eh uma das suas vizinhas, significa que a adicao da Ponte ponte(Pos1, Pos2) nao faz
com que I e Vz deixem de ser vizinhas.
----------------------------------------------------------------------------------------
*/

caminho_livre((X1,Y1), (X2,Y2), Posicoes, I, Vz):-
    I = ilha(_,(X,Y)),
    Vz = ilha(_,(Xv,Yv)),
    X1 is X, 
    Y1 is Y,                                            %Caso em que Pos1 e Pos2
    X2 is Xv,                                           %Sao Iguais a I e Vz
    Y2 is Yv,                                           %Respetivamente
    posicoes_entre((X,Y),(Xv,Yv),Posicoes_I_Vz),
    intersection(Posicoes_I_Vz,Posicoes, Inter),
    member(_,Inter).                                 

caminho_livre((X1,Y1), (X2,Y2), Posicoes, I, Vz):-
    I = ilha(_,(X,Y)),
    Vz = ilha(_,(Xv,Yv)),
    X1 is Xv, 
    Y1 is Yv,                                          %Caso em que Pos1 e Pos2
    X2 is X,                                           %Sao Iguais a Vz e I
    Y2 is Y,                                           %Respetivamente
    posicoes_entre((X,Y),(Xv,Yv),Posicoes_I_Vz),
    intersection(Posicoes_I_Vz,Posicoes, Inter),
    member(_,Inter).                                    

caminho_livre(_, _, Posicoes, I, Vz):-
    I = ilha(_,(X,Y)),
    Vz = ilha(_,(Xv,Yv)),
    posicoes_entre((X,Y),(Xv,Yv),Posicoes_I_Vz),
    intersection(Posicoes_I_Vz,Posicoes, Inter),
    not(member(_,Inter)). %Se Inter for [] retorna verdadeiro. Falso caso Contrario.

/*
-----------------------Funcao Actualiza_Vizinhas_entrada_8------------------------------
----------------------------------------------------------------------------------------
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada,Nova_Entrada), em que Pos1 e 
Pos2 sao as posicoes entre as quais ira ser adicionada uma ponte, Posicoes eh a lista 
ordenada de posicoes entre Pos1 e Pos2, e Entrada eh uma entrada, significa que Nova_Entrada 
eh igual a Entrada, excepto no que diz respeito ah lista de ilhas vizinhas; esta deve ser 
actualizada, removendo as ilhas que deixaram de ser vizinhas, apos a adicao da ponte.
----------------------------------------------------------------------------------------
*/

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada):-
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada, []).

actualiza_vizinhas_entrada(_, _, _, [Ilha,[],Pontes], Nova_Entrada, Auxa):-
    length(Auxa,LenAuxa),
    LenAuxa > 0,
    Nova_Entrada = [Ilha, Auxa, Pontes];
    Nova_Entrada = [Ilha, [], Pontes].



actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha,[H|T],Pontes], Nova_Entrada, Auxa):-
    caminho_livre(Pos1, Pos2, Posicoes, Ilha, H),
    append(Auxa, [H] , Auxa_novo),
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, T, Pontes], Nova_Entrada, Auxa_novo);
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, T, Pontes], Nova_Entrada, Auxa).


/*
--------------------------Funcao_Actualiza_vizinhas_apos_pontes_9-----------------------
----------------------------------------------------------------------------------------
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) , em que Estado eh um estado, 
Pos1 e Pos2 sao as posicoes entre as quais foi adicionada uma ponte, significa que Novo_estado 
eh o estado que se obtem de Estado apos a actualizacao das ilhas vizinhas de cada uma das suas 
entradas.
----------------------------------------------------------------------------------------
*/

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado):-
    actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado,[]).

actualiza_vizinhas_apos_pontes([], _, _, Novo_estado, Novo_estado).

actualiza_vizinhas_apos_pontes([H|T], Pos1, Pos2, Novo_estado, Aux):-
    posicoes_entre(Pos1, Pos2, Posicoesentre_1_2),
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoesentre_1_2, H, Nova_Entrada),
    append(Aux, [Nova_Entrada], Novo_Aux),
    actualiza_vizinhas_apos_pontes(T, Pos1, Pos2, Novo_estado, Novo_Aux).


/*
-----------------------------Funcao_ilhas_terminadas_10---------------------------------
----------------------------------------------------------------------------------------
ilhas_terminadas(Estado, Ilhas_term), em que Estado eh um estado, significa que Ilhas_term eh a 
lista de ilhas que ja tem todas as pontes associadas, designadas por ilhas terminadas. Se a 
entrada referente a uma ilha for [ilha(N_pontes,Pos), Vizinhas, Pontes], esta ilha esta terminada 
se N_pontes for diferente de 'X' (a razao para esta condicao ficara aparente mais a frente) e o 
comprimento da lista Pontes for N_pontes.
----------------------------------------------------------------------------------------
*/

ilhas_terminadas(Estado, Ilhas_term):- 
    ilhas_terminadas(Estado, Ilhas_term, []).
ilhas_terminadas([], Ilhas_term, Ilhas_term).
ilhas_terminadas([H|T], Ilhas_term, Aux):-
    H = [ilha(N_L,(P1,P2)), _, Pontes],
    length(Pontes,X),
    X = N_L,
    append(Aux, [ilha(N_L,(P1,P2))], Novo_Aux),
    ilhas_terminadas(T, Ilhas_term, Novo_Aux);
    ilhas_terminadas(T, Ilhas_term, Aux).


/*
-----------------------------Funcao_Tira_Ilhas_terminadas_Entrada_11--------------------
----------------------------------------------------------------------------------------
tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada), em que Ilhas_term eh uma 
lista de ilhas terminadas e Entrada eh uma entrada, significa que Nova_entrada eh a entrada 
resultante de remover as ilhas de Ilhas_term, da lista de ilhas vizinhas de entrada.
----------------------------------------------------------------------------------------
*/
tira_ilhas_terminadas_entrada([], Nova_Entrada, Nova_Entrada).
tira_ilhas_terminadas_entrada([H|T], [Ilha, Vizinhas, Pontes], Nova_Entrada):-
    member(H,Vizinhas),
    delete(Vizinhas, H, Novo_Vizinhas),
    tira_ilhas_terminadas_entrada(T, [Ilha, Novo_Vizinhas, Pontes], Nova_Entrada);
    tira_ilhas_terminadas_entrada(T, [Ilha, Vizinhas, Pontes], Nova_Entrada).

    
/*
-----------------------------Funcao_Tira_Ilhas_terminadas_12----------------------------
----------------------------------------------------------------------------------------
tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado), em que Estado eh um estado e Ilhas_term 
eh uma lista de ilhas terminadas, significa que Novo_estado eh o estado resultante de aplicar o 
predicado tira_ilhas_terminadas_entrada a cada uma das entradas de Estado.
----------------------------------------------------------------------------------------
*/
tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):-
    tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado, []).

tira_ilhas_terminadas([], _, Novo_estado, Novo_estado).

tira_ilhas_terminadas([H|T], Ilhas_term, Novo_estado, Aux):-   
    tira_ilhas_terminadas_entrada(Ilhas_term, H, Nova_Entrada),
    append(Aux, [Nova_Entrada], Novo_Aux),
    tira_ilhas_terminadas(T, Ilhas_term, Novo_estado, Novo_Aux).   

/*
---------------------------Funcao_marca_ilhas_terminadas_entrada_13---------------------
----------------------------------------------------------------------------------------
marca_ilhas_terminadas_entrada(Ilhas_term, Entrada,Nova_entrada), em que Ilhas_term eh 
uma lista de ilhas terminadas e Entrada eh uma entrada, significa que Nova_entrada eh a entrada 
obtida de Entrada da seguinte forma: se a ilha de Entrada pertencer a Ilhas_term, o 
numero de pontes desta eh substituido por 'X'; em caso contrario Nova_entrada eh igual a Entrada.
----------------------------------------------------------------------------------------
*/
marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(N_L, Pos_Ilha), Vizinhas, Pontes], Nova_entrada):-
    member(ilha(N_L,Pos_Ilha),Ilhas_term),
    Nova_entrada = [ilha('X',Pos_Ilha), Vizinhas, Pontes];
    Nova_entrada = [ilha(N_L, Pos_Ilha), Vizinhas, Pontes].

/*
--------------------------Funcao_marca_ilhas_terminadas_14------------------------------
----------------------------------------------------------------------------------------
marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado), em que Estado eh um estado e 
Ilhas_term eh uma lista de ilhas terminadas, significa que Novo_estado eh o estado resultante 
de aplicar o predicado marca_ilhas_terminadas_entrada a cada uma das entradas de Estado.
----------------------------------------------------------------------------------------
*/

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):-
    marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado, []).

marca_ilhas_terminadas([],_, Novo_estado, Novo_estado).

marca_ilhas_terminadas([H|T], Ilhas_term, Novo_estado, Aux):-
    marca_ilhas_terminadas_entrada(Ilhas_term, H, Nova_entrada),
    append(Aux, [Nova_entrada], Novo_Aux),
    marca_ilhas_terminadas(T, Ilhas_term, Novo_estado, Novo_Aux).

/*
--------------------------Funcao_trata_ilhas_terminadas_15------------------------------
----------------------------------------------------------------------------------------
trata_ilhas_terminadas(Estado, Novo_estado), em que Estado eh um estado, significa que Novo_estado 
eh o estado resultante de aplicar os predicados tira_ilhas_terminadas e marca_ilhas_terminadas a Estado.
----------------------------------------------------------------------------------------
*/
trata_ilhas_terminadas(Estado, Novo_estado):-
    ilhas_terminadas(Estado, Ilhas_term),
    tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado_1),
    marca_ilhas_terminadas(Novo_estado_1, Ilhas_term, Novo_estado).

/*
-----------------------------Funcao_junta_pontes_16-------------------------------------
----------------------------------------------------------------------------------------
junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado), em que Estado eh um estado e 
Ilha1 e Ilha2 sao 2 ilhas, significa que Novo_estado eh o estado que se obtem de Estado por 
adicao de Num_pontes pontes entre Ilha1 e Ilha2 .
----------------------------------------------------------------------------------------
*/

junta_pontes(Estado, Num_Pontes, Ilha1, Ilha2, Novo_estado):-
    junta_pontes(Estado, Num_Pontes, Ilha1, Ilha2, Novo_estado, []).

junta_pontes([],_,ilha(_, Pos1),ilha(_, Pos2), Novo_estado, Novo_estado_sem_actualiza_trata):-
    actualiza_vizinhas_apos_pontes(Novo_estado_sem_actualiza_trata, Pos1, Pos2, Estado_intermedio),
    trata_ilhas_terminadas(Estado_intermedio, Novo_estado).

junta_pontes([H|T], Num_Pontes, ilha(N_L, Pos1), ilha(N_L2, Pos2), Novo_estado, Aux):-
    cria_ponte(Pos1, Pos2, Ponte),
    H = [Ilha, Vizinhas, Pontes_H],
    member(ilha(N_L, Pos1), H),                          %Adiciona Pontes ah primeira ilha     
    repete_el(Ponte, Num_Pontes, Nova_Pontes_H),
    append(Pontes_H, Nova_Pontes_H, Pontes_H_Final),
    Novo_H = [Ilha, Vizinhas, Pontes_H_Final],
    append(Aux, [Novo_H], Novo_Aux),
    junta_pontes(T, Num_Pontes, ilha(N_L, Pos1), ilha(N_L2, Pos2), Novo_estado, Novo_Aux).

junta_pontes([H|T], Num_Pontes, ilha(N_L, Pos1), ilha(N_L2, Pos2), Novo_estado, Aux):-
    cria_ponte(Pos1, Pos2, Ponte),
    H = [Ilha, Vizinhas, Pontes_H],
    member(ilha(N_L2, Pos2), H),                         %Adiciona Pontes ah segunda ilha 
    repete_el(Ponte, Num_Pontes, Nova_Pontes_H),
    append(Pontes_H, Nova_Pontes_H, Pontes_H_Final),
    Novo_H = [Ilha, Vizinhas, Pontes_H_Final],
    append(Aux, [Novo_H], Novo_Aux),
    junta_pontes(T, Num_Pontes, ilha(N_L, Pos1), ilha(N_L2, Pos2), Novo_estado, Novo_Aux).

junta_pontes([H|T], Num_Pontes, ilha(N_L, Pos1), ilha(N_L2, Pos2), Novo_estado, Aux):-
    append(Aux, [H], Novo_Aux),                                                             %Caso em que Ilha da Entrada nao recebe pontes
    junta_pontes(T, Num_Pontes, ilha(N_L, Pos1), ilha(N_L2, Pos2), Novo_estado, Novo_Aux).



/*
----------------------------------Funcao_Auxiliar_repete--------------------------------
----------------------------------------------------------------------------------------
Devolve uma Lista (L) em que o Elemento (E) se repete N vezes
----------------------------------------------------------------------------------------
*/
%Funcao_auxiliar_repete
repete_el(E, N, L) :-
  % afirmar que L, desconhecida, tem comprimento N
  length(L, N),
  % o predicado fara com que todos os espacos vazios sejam "unificados" com E
  maplist(=(E), L).