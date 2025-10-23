:- encoding(utf8). % permite acentuação

estado_final(estado(_, _, peito, costas, braco, perna)).

% DEFINIÇÃO DO CENÁRIO
peso(coord(5, 2)).
invalidos([coord(9,2), coord(1,2), coord(2,2)]).
maquinas_peito([coord(1,1), coord(1,2), coord(1,3)]).
maquinas_braco([coord(9,1), coord(9,2), coord(9,3)]).
maquinas_costas([coord(4,6), coord(5,6), coord(6,6)]).

% VALIDAÇÃO DE COORDENADA
eh_valido(coord(X, Y)) :- 
    invalidos(L),
    \+ pertence(coord(X,Y), L),
    ((X =< 9, Y =< 3, X >= 1, Y >= 1);
     (X =:= 5, Y =:= 0);
     (X >= 4, X =< 6, Y >= 4, Y =< 6)).

% AÇÕES DE MOVIMENTO
acao(anda_cima, estado(coord(X, Y), A, B, C, D, E), estado(coord(X, NewY), A, B, C, D, E)) :-
    NewY is Y - 1, 
    eh_valido(coord(X, NewY)),
    ((NewY =:= 0 ; NewY =:= 3 , X =:= 5) ; (NewY =\= 0 , NewY =\= 3)).

acao(anda_baixo, estado(coord(X, Y), A, B, C, D, E), estado(coord(X, NewY), A, B, C, D, E)) :-
    NewY is Y + 1, 
    eh_valido(coord(X, NewY)),
    ((NewY =:= 1 ; NewY =:= 4 , X =:= 5); (NewY =\= 1 , NewY =\= 4)).

acao(anda_esq, estado(coord(X, Y), A, B, C, D, E), estado(coord(NewX, Y), A, B, C, D, E)) :-
    NewX is X - 1, 
    eh_valido(coord(NewX, Y)),
    (((NewX =:= 3 ; NewX =:= 6) , Y =:= 2); (NewX =\= 3 , NewX =\= 6)).

acao(anda_dir, estado(coord(X, Y), A, B, C, D, E), estado(coord(NewX, Y), A, B, C, D, E)) :-
    NewX is X + 1, 
    eh_valido(coord(NewX, Y)),
    (((NewX =:= 4 ; NewX =:= 7) , Y =:= 2); (NewX =\= 4 , NewX =\= 7)).


% AÇÕES DE INTERAÇÃO
acao(pega_peso, estado(Coord_atual, sem_peso, M, C, D, E), estado(Coord_atual, com_peso, M, C, D, E)) :-
    peso(Coord_atual).

acao(treina_peito, estado(Coord_atual, com_peso, sem_peito, C, D, E), estado(Coord_atual, sem_peso, peito, C, D, E)) :- 
    maquinas_peito(Lista), pertence(Coord_atual,Lista). 

acao(treina_costas, estado(Coord_atual, com_peso, C, sem_costas, D, E), estado(Coord_atual, sem_peso, C, costas, D, E)) :- 
    maquinas_costas(Lista), pertence(Coord_atual,Lista).

acao(treina_braco, estado(Coord_atual, com_peso, D, C, sem_braco, E), estado(Coord_atual, sem_peso, D, C, braco, E)) :- 
    maquinas_braco(Lista), pertence(Coord_atual,Lista).

acao(treina_perna, estado(A, B, peito, costas, braco, sem_perna), estado(A, B, peito, costas, braco, perna)).


% PRIMITIVAS DE LISTA
pertence(Elem, [Elem|_]).
pertence(Elem, [_|Cauda]) :- pertence(Elem, Cauda).

concatena([], L, L).
concatena([Cab|Cauda], L2, [Cab|Resultado]) :-
    concatena(Cauda, L2, Resultado).

reverter([], []).
reverter([H|T], R) :-
    reverter(T, RT),
    append(RT, [H], R).


%BUSCA EM LARGURA
%Ponto de entrada
solucao_bl(EstadoInicial, CaminhoCorreto) :- 
    bl([[acao(inicio, EstadoInicial)]], CaminhoInvertido),
    reverter(CaminhoInvertido, CaminhoCorreto).

% Caso base: encontrou o estado final
bl([[acao(Acao, Estado) | Caminho] | _], [acao(Acao, Estado) | Caminho]) :- 
    estado_final(Estado).

% Caso recursivo: expande a fronteira
bl([Primeiro | Outros], Solucao) :-
    estende(Primeiro, Sucessores),
    concatena(Outros, Sucessores, NovaFronteira),
    bl(NovaFronteira, Solucao).

% Geração de sucessores (mantendo ações corretamente)
estende([acao(AcaoAtual, EstadoAtual) | Caminho], ListaSucessores) :-
    findall(
        NovoCaminho,
        (
            acao(AcaoNova, EstadoAtual, Sucessor),
            \+ (member(acao(_, S), [acao(AcaoAtual, EstadoAtual) | Caminho]), S == Sucessor),
            copy_term([acao(AcaoNova, Sucessor), acao(AcaoAtual, EstadoAtual) | Caminho], NovoCaminho)
        ),
        ListaSucessores
    ).



%Predicado para identificar ações de movimento
movimento(anda_cima).
movimento(anda_baixo).
movimento(anda_esq).
movimento(anda_dir).

%DESCRIÇÃO DAS AÇÕES
descreve_acao(anda_cima,   "Nurilo Maldi andou para cima").
descreve_acao(anda_baixo,  "Nurilo Maldi andou para baixo").
descreve_acao(anda_esq,    "Nurilo Maldi andou para a esquerda").
descreve_acao(anda_dir,    "Nurilo Maldi andou para a direita").
descreve_acao(pega_peso,   "Nurilo Maldi pegou o peso").
descreve_acao(treina_peito, "Nurilo Maldi treinou peito").
descreve_acao(treina_costas,"Nurilo Maldi treinou costas").
descreve_acao(treina_braco, "Nurilo Maldi treinou braço").
descreve_acao(treina_perna, "Como Nurilo Maldi andou muito, considera-se que ele treinou perna").
descreve_acao(inicio,      "Início do treino").

%IMPRIMIR CAMINHO
imprimir_caminho([]).
imprimir_caminho([acao(Acao, Estado) | Resto]) :-
    ( Estado = estado(coord(X, Y), _, _, _, _, _) -> true ; X = _, Y = _ ),
    descreve_acao(Acao, Texto),
    (   movimento(Acao)
    ->
        format("~w e agora está em (~w,~w).~n", [Texto, X, Y])
    ;
        format("~w.~n", [Texto])
    ),
    imprimir_caminho(Resto).


% EXEMPLO DE CONSULTA
% ?- solucao_bl(estado(coord(5,0), sem_peso, peito, sem_costas, braco, sem_perna), Caminho), imprimir_caminho(Caminho).
%CONSULTA MAIS TRABALHOSA POSSIVEL:
%  solucao_bl(estado(coord(5,0),sem_peso, sem_peito, costas, sem_braco, sem_perna), X), imprimir_caminho(X).