estado( [[A,B,C,D],
		[E,F,G,H],
		[I,J,K,L],
		[M,N,O,b]]).

estado_final(	[[1,2,3,4],
				[5,6,7,8],
				[9,10,11,12],
				[13,14,15,b]]).

%% MOVIMIENTOS
% derecha
% primera fila
derecha([[b,A,B,C],[D,E,F,G],[H,I,J,K],[L,M,N,O]],
		[[A,b,B,C],[D,E,F,G],[H,I,J,K],[L,M,N,O]]).
derecha([[A,b,B,C],[D,E,F,G],[H,I,J,K],[L,M,N,O]],
		[[A,B,b,C],[D,E,F,G],[H,I,J,K],[L,M,N,O]]).
derecha([[A,B,b,C],[D,E,F,G],[H,I,J,K],[L,M,N,O]],
		[[A,B,C,b],[D,E,F,G],[H,I,J,K],[L,M,N,O]]).
% segunda fila
derecha([[A,B,C,D],[b,E,F,G],[H,I,J,K],[L,M,N,O]],
		[[A,B,C,D],[E,b,F,G],[H,I,J,K],[L,M,N,O]]).
derecha([[A,B,C,D],[E,b,F,G],[H,I,J,K],[L,M,N,O]],
		[[A,B,C,D],[E,F,b,G],[H,I,J,K],[L,M,N,O]]).
derecha([[A,B,C,D],[E,F,b,G],[H,I,J,K],[L,M,N,O]],
		[[A,B,C,D],[E,F,G,b],[H,I,J,K],[L,M,N,O]]).
%tercera fila
derecha([[A,B,C,D],[E,F,G,H],[b,I,J,K],[L,M,N,O]],
		[[A,B,C,D],[E,F,G,H],[I,b,J,K],[L,M,N,O]]).
derecha([[A,B,C,D],[E,F,G,H],[I,b,J,K],[L,M,N,O]],
		[[A,B,C,D],[E,F,G,H],[I,J,b,K],[L,M,N,O]]).
derecha([[A,B,C,D],[E,F,G,H],[I,J,b,K],[L,M,N,O]],
		[[A,B,C,D],[E,F,G,H],[I,J,K,b],[L,M,N,O]]).
%cuarta fila
derecha([[A,B,C,D],[E,F,G,H],[I,J,K,L],[b,M,N,O]],
		[[A,B,C,D],[E,F,G,H],[I,J,K,L],[M,b,N,O]]).
derecha([[A,B,C,D],[E,F,G,H],[I,J,K,L],[M,b,N,O]],
		[[A,B,C,D],[E,F,G,H],[I,J,K,L],[M,N,b,O]]).
derecha([[A,B,C,D],[E,F,G,H],[I,J,K,L],[M,N,b,O]],
		[[A,B,C,D],[E,F,G,H],[I,J,K,L],[M,N,O,b]]).

%izquierda
%primera fila
izquierda(	[[A,b,B,C],[D,E,F,G],[H,I,J,K],[L,M,N,O]],
			[[b,A,B,C],[D,E,F,G],[H,I,J,K],[L,M,N,O]]).
izquierda(	[[A,B,b,C],[D,E,F,G],[H,I,J,K],[L,M,N,O]],
			[[A,b,B,C],[D,E,F,G],[H,I,J,K],[L,M,N,O]]).
izquierda(	[[A,B,C,b],[D,E,F,G],[H,I,J,K],[L,M,N,O]],
			[[A,B,b,C],[D,E,F,G],[H,I,J,K],[L,M,N,O]]).
%segunda fila
izquierda(	[[A,B,C,D],[E,b,F,G],[H,I,J,K],[L,M,N,O]],
			[[A,B,C,D],[b,E,F,G],[H,I,J,K],[L,M,N,O]]).
izquierda(	[[A,B,C,D],[E,F,b,G],[H,I,J,K],[L,M,N,O]],
			[[A,B,C,D],[E,b,F,G],[H,I,J,K],[L,M,N,O]]).
izquierda(	[[A,B,C,D],[E,F,G,b],[H,I,J,K],[L,M,N,O]],
			[[A,B,C,D],[E,F,b,G],[H,I,J,K],[L,M,N,O]]).
%tercera fila
izquierda(	[[A,B,C,D],[E,F,G,H],[I,b,J,K],[L,M,N,O]],
			[[A,B,C,D],[E,F,G,H],[b,I,J,K],[L,M,N,O]]).
izquierda(	[[A,B,C,D],[E,F,G,H],[I,J,b,K],[L,M,N,O]],
			[[A,B,C,D],[E,F,G,H],[I,b,J,K],[L,M,N,O]]).
izquierda(	[[A,B,C,D],[E,F,G,H],[I,J,K,b],[L,M,N,O]],
			[[A,B,C,D],[E,F,G,H],[I,J,b,K],[L,M,N,O]]).
%cuarta fila
izquierda(	[[A,B,C,D],[E,F,G,H],[I,J,K,L],[M,b,N,O]],
			[[A,B,C,D],[E,F,G,H],[I,J,K,L],[b,M,N,O]]).
izquierda(	[[A,B,C,D],[E,F,G,H],[I,J,K,L],[M,N,b,O]],
			[[A,B,C,D],[E,F,G,H],[I,J,K,L],[M,b,N,O]]).
izquierda(	[[A,B,C,D],[E,F,G,H],[I,J,K,L],[M,N,O,b]],
			[[A,B,C,D],[E,F,G,H],[I,J,K,L],[M,N,b,O]]).


%arriba
%primera no arriba
%segunda fila
arriba(	[[A,B,C,D],
		[b,E,F,G],
		[H,I,J,K],
		[L,M,N,O]],
		[[b,B,C,D],
		[A,E,F,G],
		[H,I,J,K],
		[L,M,N,O]]).
arriba(	[[A,B,C,D],
		[E,b,F,G],
		[H,I,J,K],
		[L,M,N,O]],
		[[A,b,C,D],
		[E,B,F,G],
		[H,I,J,K],
		[L,M,N,O]]).
arriba(	[[A,B,C,D],
		[E,F,b,G],
		[H,I,J,K],
		[L,M,N,O]],
		[[A,B,b,D],
		[E,F,C,G],
		[H,I,J,K],
		[L,M,N,O]]).
arriba(	[[A,B,C,D],
		[E,F,G,b],
		[H,I,J,K],
		[L,M,N,O]],
		[[A,B,C,b],
		[E,F,G,D],
		[H,I,J,K],
		[L,M,N,O]]).

%tercera fila

arriba(	[[A,B,C,D],
		[E,F,G,H],
		[b,I,J,K],
		[L,M,N,O]],
		[[A,B,C,D],
		[b,F,G,H],
		[E,I,J,K],
		[L,M,N,O]]).
arriba(	[[A,B,C,D],
		[E,F,G,H],
		[I,b,J,K],
		[L,M,N,O]],
		[[A,B,C,D],
		[E,b,G,H],
		[I,F,J,K],
		[L,M,N,O]]).
arriba(	[[A,B,C,D],
		[E,F,G,H],
		[I,J,b,K],
		[L,M,N,O]],
		[[A,B,C,D],
		[E,F,b,H],
		[I,J,G,K],
		[L,M,N,O]]).
arriba(	[[A,B,C,D],
		[E,F,G,H],
		[I,J,K,b],
		[L,M,N,O]],
		[[A,B,C,D],
		[E,F,G,b],
		[I,J,K,H],
		[L,M,N,O]]).

%cuarta fila
arriba(	[[A,B,C,D],
		[E,F,G,H],
		[I,J,K,L],
		[b,M,N,O]],
		[[A,B,C,D],
		[E,F,G,H],
		[b,J,K,L],
		[I,M,N,O]]).
arriba(	[[A,B,C,D],
		[E,F,G,H],
		[I,J,K,L],
		[M,b,N,O]],
		[[A,B,C,D],
		[E,F,G,H],
		[I,b,K,L],
		[M,J,N,O]]).
arriba(	[[A,B,C,D],
		[E,F,G,H],
		[I,J,K,L],
		[M,N,b,O]],
		[[A,B,C,D],
		[E,F,G,H],
		[I,J,b,L],
		[M,N,K,O]]).
arriba(	[[A,B,C,D],
		[E,F,G,H],
		[I,J,K,L],
		[M,N,O,b]],
		[[A,B,C,D],
		[E,F,G,H],
		[I,J,K,b],
		[M,N,O,L]]).


%abajo
%primera fila
abajo(	[[b,A,B,C],
		[D,E,F,G],
		[H,I,J,K],
		[L,M,N,O]],
		[[D,A,B,C],
		[b,E,F,G],
		[H,I,J,K],
		[L,M,N,O]]).
abajo(	[[A,b,B,C],
		[D,E,F,G],
		[H,I,J,K],
		[L,M,N,O]],
		[[A,E,B,C],
		[D,b,F,G],
		[H,I,J,K],
		[L,M,N,O]]).
abajo(	[[A,B,b,C],
		[D,E,F,G],
		[H,I,J,K],
		[L,M,N,O]],
		[[A,B,F,C],
		[D,E,b,G],
		[H,I,J,K],
		[L,M,N,O]]).
abajo(	[[A,B,C,b],
		[D,E,F,G],
		[H,I,J,K],
		[L,M,N,O]],
		[[A,B,C,G],
		[D,E,F,b],
		[H,I,J,K],
		[L,M,N,O]]).

%segunda fila
abajo(	[[A,B,C,D],
		[b,E,F,G],
		[H,I,J,K],
		[L,M,N,O]],
		[[A,B,C,D],
		[H,E,F,G],
		[b,I,J,K],
		[L,M,N,O]]).
abajo(	[[A,B,C,D],
		[E,b,F,G],
		[H,I,J,K],
		[L,M,N,O]],
		[[A,B,C,D],
		[E,I,F,G],
		[H,b,J,K],
		[L,M,N,O]]).
abajo(	[[A,B,C,D],
		[E,F,b,G],
		[H,I,J,K],
		[L,M,N,O]],
		[[A,B,C,D],
		[E,F,J,G],
		[H,I,b,K],
		[L,M,N,O]]).
abajo(	[[A,B,C,D],
		[E,F,G,b],
		[H,I,J,K],
		[L,M,N,O]],
		[[A,B,C,D],
		[E,F,G,K],
		[H,I,J,b],
		[L,M,N,O]]).

%tercera linea

abajo(	[[A,B,C,D],
		[E,F,G,H],
		[b,I,J,K],
		[L,M,N,O]],
		[[A,B,C,D],
		[E,F,G,H],
		[L,I,J,K],
		[b,M,N,O]]).
abajo(	[[A,B,C,D],
		[E,F,G,H],
		[I,b,J,K],
		[L,M,N,O]],
		[[A,B,C,D],
		[E,F,G,H],
		[I,M,J,K],
		[L,b,N,O]]).
abajo(	[[A,B,C,D],
		[E,F,G,H],
		[I,J,b,K],
		[L,M,N,O]],
		[[A,B,C,D],
		[E,F,G,H],
		[I,J,N,K],
		[L,M,b,O]]).
abajo(	[[A,B,C,D],
		[E,F,G,H],
		[I,J,K,b],
		[L,M,N,O]],
		[[A,B,C,D],
		[E,F,G,H],
		[I,J,K,O],
		[L,M,N,b]]).
%cuarta linea no tiene movimiento abajo


%predicados de movimiento para el tablero

operador(T,izquierda,Tn,1) 	:-  izquierda(T,Tn).
operador(T,arriba,Tn,1)		:-  arriba(T,Tn).
operador(T,derecha,Tn,1)	:-  derecha(T,Tn).
operador(T,abajo,Tn,1)		:-  abajo(T,Tn).

%lista de peradores izquierda,arriba,derecha,abajo.
% el coste de un movimiento es 1



					
%%%%%% MODELIZAMOS LOS NODOS
%% nodo(Estado, Padre, Operador, Coste).

%%%%%%%%%%%% HASTA AQUÍ ESTÁ EL JUEGO MODELADO %%%%%%%%%%%%


a_estrella(EstadoInicial) :- 
  % creamos el nodo inicial e inicializamos la lista
  % abiertos con el estado inicial y la lista cerrados vacia
  a_estrella([nodo(EstadoInicial, none, 0, 0)], [], Operadores),
  % una vez ha acabado la recursividad ya tenemos una solución
  % (si ha llegado hasta aquí es que es verdadero)
  % y lo que hacemos es darle la vuelta a los operadores.
  reverse(Operadores, OperadoresOrdenados),
  writeln('SOLUCION: ' + OperadoresOrdenados).


%% 
%% if ACTUAL es ESTADO_FINAL then RESUELTO := true
a_estrella([NodoActual|_], _,Operadores) :-
  % instanciamos el nodo actual para sacar el estado y comprobar si es la solucion
  NodoActual = nodo(EstadoActual,_,_,_), 
  estado_final(EstadoActual), 
  % si el estado actual es la solución hemos terminado,
  % se extraen los operadores y se corta la expansión.
  extraerOperadores(NodoActual, Operadores), !.
  


a_estrella([NodoActual|RestoAbiertos], Cerrados, Operadores) :- 
  NodoActual = nodo(EstadoActual,_,_,_),
  % expandimos el nodo actual
  generar_sucesores(NodoActual, [EstadoActual|Cerrados], NodosSucesores),
  % insertamos de forma ordenada los sucesores en la lista abiertos
  insertar_sucesores(NodosSucesores, RestoAbiertos, NuevoAbiertos),
  % volvemos a buscar una vez añadidos los nodos expandidos
  a_estrella(NuevoAbiertos, [EstadoActual|Cerrados], Operadores).

insertar_sucesores([CabezaSuc|RestoSuc],AbiertosOrg,AbiertosFinal) :- insertar(CabezaSuc,AbiertosOrg,AbiertosTrans),
                                 									insertar_sucesores(RestoSuc,AbiertosTrans,AbiertosFinal).
insertar_sucesores([],Abiertos,Abiertos).
%% se queda como está si no hay sucesores


insertar(Nodo,Abiertos,Abiertos) :- nodo_repetido(Nodo,Abiertos), ! .
insertar(Nodo,[H|T],[Nodo,H|T]) :- masBarato(Nodo,H), ! .
%% si el nodo es más barato que la cabeza se pone de primero
insertar(Nodo,[Nodo1|R],[Nodo1|S]) :- insertar(Nodo,R,S), !.
%% si no se cumplió el anterior predicado, entonces tenemos que meterlo en
%% la posición que le corresponda respetando el orden ascendente.
insertar(Nodo,[],[Nodo]).
%% si no se añade al final


nodo_repetido(NodoA, [NodoB|_]) :- NodoA = nodo(Est,_,_,_),
								NodoB = nodo(Est,_,_,_).

masBarato( NodoA , NodoB ) :- fn(NodoA, FnA),
								fn(NodoB, FnB),
								FnA < FnB.


generar_sucesores(NodoActual, Cerrados, NodosSucesores) :- 
	  NodoActual = nodo(EstadoActual,_,_,CostePadre), /* instanciar componentes del NodoActual*/
	  findall(nodo(EstadoSucesor, NodoActual, Operador, CosteSucesor), 
          (operador(EstadoActual, Operador, EstadoSucesor, CosteOperador),
           \+member(EstadoSucesor, Cerrados),
           CosteSucesor is CostePadre + CosteOperador), 
          NodosSucesores).
	  % cogemos todos los nodos resultado de aplicar los operadores posibilidades
	  % al nodo actual. En ese proceso se pone como padre de los nuevos nodos al
	  % nodo actual y se añade el coste del operador
%% para probar
%% generar_sucesores(nodo([[2,1,3,4],[5,6,b,7],[9,10,11,12],[13,14,15,8]],[[2,1,3,4],[5,b,6,7],[9,10,11,12],[13,14,15,8]],izquierda,5),[],NodosSucesores),length(NodosSucesores,L).
%% devuelve 4 nodos

extraerOperadores(nodo(_, none, _, _), []).
extraerOperadores(nodo(_, Padre, Operador, _), [Operador|Operadores]) :- extraerOperadores(Padre, Operadores).

%%%%%%%% FIN ALGORITMO %%%%%%%%%%%%


%%%%%%%% INICIO HEURÍSTICAS %%%%%%

fn(NodoActual,Fn) :- NodoActual = nodo(EstadoActual,_,_,Gn),
					heuristica1(EstadoActual, Hn),
					%heuristica2(EstadoActual, Hn),
					Fn is Gn + Hn.
%%  PREDICADO PARA APLICAR HEURÍSTICAS Y DAR COMO RESULTADO Fn




% HEURISTICA NÚMERO DE PIEZAS DESCOLOCADAS
heuristica1(Actual, Hn) :- estado_final(Final),
			flatten(Actual, ActualPlana),
			flatten(Final, FinalPlana),
			descolocados(ActualPlana,FinalPlana,Hn). 
descolocados([],[],0). 
descolocados([0|T1],[0|T2],Number) :- descolocados(T1,T2,Number). 
descolocados([H|T1],[H|T2],Number) :- descolocados(T1,T2,Number). 
descolocados([H1|T1],[H2|T2],Number) :- H1\==H2,
										descolocados(T1,T2,N),
										Number is N+1.
%% FALTA HEURÍSTICA MANHATTAN ----.----

heuristica2(Actual, Res) :-
	 Actual = [[A,B,C,D],[E,F,G,H],[I,J,K,L],[M,N,O,P]],
     a(A,Pa), b(B,Pb), c(C,Pc), d(D,Pd),
     e(E,Pe), f(F,Pf), g(G,Pg), h(H,Ph),
     i(I,Pi), j(J,Pj), k(K,Pk), l(L,Pl),
     m(M,Pm), n(N,Pn), o(O,Po), p(P,Pp),
     Res is Pa+Pb+Pc+Pd+Pe+Pf+Pg+Ph+Pi+Pj+Pk+Pl+Pm+Pn+Po+Pp.

a(b,6). a(1,0). a(2,1). a(3,2). a(4,3). 
		a(5,1). a(6,2). a(7,3). a(8,4). 
		a(9,2). a(10,3). a(11,4). a(12,5).
		a(13,3). a(14,4). a(15,5).

b(b,5). b(1,1). b(2,0). b(3,1). b(4,2).
		b(5,2). b(6,1). b(7,2). b(8,3).
		b(9,3). b(10,2). b(11,3). b(12,4).
		b(13,4). b(14,3). b(15,4).

c(b,4). c(1,2). c(2,1). c(3,0). c(4,1).
		c(5,3). c(6,2). c(7,1). c(8,2).
		c(9,4). c(10,3). c(11,2). c(12,3).
		c(13,5). c(14,4). c(15,3).

d(b,3). d(1,3). d(2,2). d(3,1). d(4,0).
		d(5,4). d(6,3). d(7,2). d(8,1).
		d(9,5). d(10,4). d(11,3). d(12,2).
		d(13,6). d(14,5). d(15,4).

e(b,5). e(1,1). e(2,2). e(3,3). e(4,4).
		e(5,0). e(6,1). e(7,2). e(8,3).
		e(9,1). e(10,2). e(11,3). e(12,4).
		e(13,2). e(14,3). e(15,4).

f(b,4). f(1,2). f(2,1). f(3,2). f(4,3).
		f(5,1). f(6,0). f(7,1). f(8,2).
		f(9,2). f(10,1). f(11,2). f(12,3).
		f(13,3). f(14,2). f(15,3).

g(b,3). g(1,3). g(2,2). g(3,1). g(4,2).
		g(5,2). g(6,1). g(7,0). g(8,1).
		g(9,3). g(10,2). g(11,1). g(12,2).
		g(13,4). g(14,3). g(15,2).

h(b,2). h(1,4). h(2,3). h(3,2). h(4,1).
		h(5,3). h(6,2). h(7,1). h(8,0).
		h(9,4). h(10,3). h(11,2). h(12,1).
		h(13,5). h(14,4). h(15,3).

i(b,4). i(1,2). i(2,3). i(3,4). i(4,5).
		i(5,1). i(6,2). i(7,3). i(8,4).
		i(9,0). i(10,1). i(11,2). i(12,3).
		i(13,1). i(14,2). i(15,3).
		
j(b,3). j(1,3). j(2,2). j(3,3). j(4,4).
		j(5,2). j(6,1). j(7,2). j(8,3).
		j(9,1). j(10,0). j(11,1). j(12,2).
		j(13,2). j(14,1). j(15,2).

k(b,2). k(1,4). k(2,3). k(3,2). k(4,3).
		k(5,3). k(6,2). k(7,1). k(8,2).
		k(9,2). k(10,1). k(11,0). k(12,1).
		k(13,3). k(14,2). k(15,1).

l(b,1). l(1,5). l(2,4). l(3,3). l(4,2).
		l(5,4). l(6,3). l(7,2). l(8,1).
		l(9,3). l(10,2). l(11,1). l(12,0).
		l(13,4). l(14,3). l(15,2).

m(b,3). m(1,3). m(2,4). m(3,5). m(4,6).
		m(5,2). m(6,3). m(7,4). m(8,5).
		m(9,1). m(10,2). m(11,3). m(12,4).
		m(13,0). m(14,1). m(15,2).

n(b,2). n(1,4). n(2,3). n(3,4). n(4,5).
		n(5,3). n(6,2). n(7,3). n(8,4).
		n(9,2). n(10,1). n(11,2). n(12,3).
		n(13,1). n(14,0). n(15,1).

o(b,1). o(1,5). o(2,4). o(3,3). o(4,4).
		o(5,4). o(6,3). o(7,2). o(8,3).
		o(9,3). o(10,2). o(11,1). o(12,2).
		o(13,2). o(14,1). o(15,0).

p(b,0). p(1,6). p(2,5). p(3,4). p(4,3).
		p(5,5). p(6,4). p(7,3). p(8,2).
		p(9,4). p(10,3). p(11,2). p(12,1).
		p(13,3). p(14,2). p(15,1).