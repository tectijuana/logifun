**Lista de Problemas en Prolog para Prácticas de Laboratorio**


BAsados en: https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

1. Encontrar el último elemento de una lista.
```prolog
% Encuentra el último elemento de una lista.
% La lista debe tener al menos un elemento.
my_last(X, [X]).
my_last(X, [_|T]) :- my_last(X, T).
```
2. Encontrar el penúltimo elemento de una lista.
```prolog
% Encuentra el penúltimo elemento de una lista.
% La lista debe tener al menos dos elementos.
penultimate(X, [X, _]).
penultimate(X, [_|T]) :- penultimate(X, T).
```
3. Encontrar el k-ésimo elemento de una lista.
```prolog
% Encuentra el k-ésimo elemento de una lista.
% El índice K debe ser mayor que 0.
element_at(X, [X|_], 1).
element_at(X, [_|T], K) :- K > 1, K1 is K - 1, element_at(X, T, K1).
```
4. Encontrar el número de elementos de una lista.
```prolog
% Calcula el número de elementos de una lista.
% Caso base: una lista vacía tiene longitud 0.
list_length([], 0).
% Caso recursivo: la longitud es 1 más que la longitud de la cola.
list_length([_|T], N) :- list_length(T, N1), N is N1 + 1.
```
5. Invertir una lista.
```prolog
% Invierte una lista.
% Utiliza un acumulador para ir construyendo la lista invertida.
reverse_list(L, R) :- reverse_list(L, [], R).
% Caso base: cuando la lista original está vacía, el acumulador contiene la lista invertida.
reverse_list([], Acc, Acc).
% Caso recursivo: mueve el elemento de la cabeza al acumulador.
reverse_list([H|T], Acc, R) :- reverse_list(T, [H|Acc], R).
```
6. Determinar si una lista es un palíndromo.
```prolog
% Verifica si una lista es un palíndromo.
% Una lista es un palíndromo si es igual a su inversa.
palindrome(L) :- reverse_list(L, L).
```
7. Aplanar una estructura de lista anidada.
```prolog
% Aplana una lista anidada en una lista simple.
% Caso base: la lista vacía se aplana a una lista vacía.
my_flatten([], []).
% Caso recursivo: si la cabeza es una lista, aplana la cabeza y la cola recursivamente.
my_flatten([H|T], Flat) :- my_flatten(H, FH), my_flatten(T, FT), append(FH, FT, Flat).
% Si la cabeza no es una lista, simplemente agrégala al resultado.
my_flatten(X, [X]) :- X \= [], \+ is_list(X).
```
8. Eliminar duplicados consecutivos de los elementos de una lista.
```prolog
% Elimina duplicados consecutivos de una lista.
% Caso base: una lista vacía se mantiene vacía.
compress([], []).
% Si queda un solo elemento, no hay duplicados.
compress([X], [X]).
% Caso recursivo: si el primer y segundo elemento son iguales, ignora el primero.
compress([X,X|T], R) :- compress([X|T], R).
% Si el primer y segundo elemento son distintos, conserva el primero.
compress([X,Y|T], [X|R]) :- X \= Y, compress([Y|T], R).
```
9. Agrupar duplicados consecutivos de los elementos de una lista en sublistas.
```prolog
% Agrupa duplicados consecutivos de una lista en sublistas.
% Caso base: una lista vacía se agrupa como una lista vacía.
pack([], []).
% Caso recursivo: transfiere todos los elementos consecutivos iguales a una sublista.
pack([X|Xs], [[X|Ys]|Zs]) :- transfer(X, Xs, Ys, Rest), pack(Rest, Zs).

% Transfiere los elementos duplicados consecutivos a una sublista.
transfer(X, [], [], []).
% Si el siguiente elemento es diferente, termina la transferencia.
transfer(X, [Y|Ys], [], [Y|Ys]) :- X \= Y.
% Si el siguiente elemento es igual, agrégalo a la sublista.
transfer(X, [X|Xs], [X|Ys], Rest) :- transfer(X, Xs, Ys, Rest).
```
10. Codificación Run-Length de una lista.
```prolog
% Codifica una lista usando codificación Run-Length.
% Primero agrupa los duplicados y luego transforma las sublistas en parejas (N, X).
encode(L, R) :- pack(L, P), transform(P, R).

% Transforma sublistas en parejas (N, X), donde N es la cantidad de elementos.
transform([], []).
transform([[X|Xs]|Ys], [[N,X]|Zs]) :- length([X|Xs], N), transform(Ys, Zs).
```
11. Modificar la codificación Run-Length de una lista.
```prolog
% Modifica la codificación Run-Length de una lista.
% Si un elemento no tiene duplicados, se mantiene sin empaquetar.
encode_modified(L, R) :- encode(L, Enc), modify(Enc, R).

% Caso base: lista vacía.
modify([], []).
% Si hay solo un elemento, agrégalo directamente.
modify([[1,X]|T], [X|R]) :- modify(T, R).
% Si hay más de un elemento, mantén el formato (N, X).
modify([[N,X]|T], [[N,X]|R]) :- N > 1, modify(T, R).
```
12. Decodificar una lista codificada mediante Run-Length.
```prolog
% Decodifica una lista codificada mediante Run-Length.
% Expande cada par (N, X) en una lista con N elementos X.
decode([], []).
decode([[N,X]|T], R) :- decode(T, R1), expand(X, N, E), append(E, R1, R).

% Expande un elemento X en una lista de longitud N.
expand(_, 0, []).
expand(X, N, [X|T]) :- N > 0, N1 is N - 1, expand(X, N1, T).
```
13. Codificación Run-Length de una lista (solución directa).
```prolog
% Codifica directamente una lista usando codificación Run-Length.
% Cuenta cuántos elementos consecutivos son iguales.
encode_direct([], []).
encode_direct([X|Xs], [[N,X]|R]) :- count(X, [X|Xs], N, Rest), encode_direct(Rest, R).

% Cuenta cuántos elementos consecutivos son iguales.
count(_, [], 0, []).
count(X, [X|Xs], N, Rest) :- count(X, Xs, N1, Rest), N is N1 + 1.
count(X, [Y|Ys], 0, [Y|Ys]) :- X \= Y.
```
14. Duplicar los elementos de una lista.
```prolog
% Duplica los elementos de una lista.
% Caso base: la lista vacía se mantiene vacía.
dupli([], []).
% Caso recursivo: duplica el primer elemento y continúa con el resto.
dupli([H|T], [H,H|R]) :- dupli(T, R).
```
15. Duplicar los elementos de una lista un número dado de veces.
```prolog
% Duplica los elementos de una lista un número dado de veces.
% Caso base: la lista vacía se mantiene vacía.
dupli([], _, []).
% Caso recursivo: duplica el primer elemento N veces y continúa con el resto.
dupli([H|T], N, R) :- duplicate(H, N, D), dupli(T, N, R1), append(D, R1, R).

% Crea una lista con N repeticiones del elemento X.
duplicate(_, 0, []).
duplicate(X, N, [X|R]) :- N > 0, N1 is N - 1, duplicate(X, N1, R).
```
16. Eliminar cada n-ésimo elemento de una lista.
```prolog
% Elimina cada n-ésimo elemento de una lista.
% Utiliza un contador para determinar cuándo eliminar un elemento.
drop(L, N, R) :- drop(L, N, N, R).

% Caso base: cuando la lista está vacía, el resultado es vacío.
drop([], _, _, []).
% Si el contador llega a 1, omite el primer elemento y reinicia el contador.
drop([_|T], 1, N, R) :- drop(T, N, N, R).
% Caso recursivo: disminuye el contador y conserva el elemento.
drop([H|T], K, N, [H|R]) :- K > 1, K1 is K - 1, drop(T, K1, N, R).
```
17. Dividir una lista en dos partes.
```prolog
% Divide una lista en dos partes.
% La longitud de la primera parte está dada por el segundo argumento.
split(L, 0, [], L).
split([H|T], N, [H|L1], L2) :- N > 0, N1 is N - 1, split(T, N1, L1, L2).
```
18. Extraer una porción de una lista.
```prolog
% Extrae una porción de una lista, dado un rango.
% Los índices I y K deben ser mayores que 0.
slice([H|_], 1, 1, [H]).
slice([H|T], 1, K, [H|R]) :- K > 1, K1 is K - 1, slice(T, 1, K1, R).
slice([_|T], I, K, R) :- I > 1, I1 is I - 1, K1 is K - 1, slice(T, I1, K1, R).
```
19. Rotar una lista un número N de lugares a la izquierda.
```prolog
% Rota una lista N lugares a la izquierda.
% Utiliza la longitud de la lista para manejar rotaciones mayores que la longitud.
rotate(L, N, R) :- length(L, Len), N1 is N mod Len, split(L, N1, L1, L2), append(L2, L1, R).
```
20. Eliminar el k-ésimo elemento de una lista.
```prolog
% Elimina el k-ésimo elemento de una lista.
% El índice K debe ser mayor que 0.
remove_at(X, [X|T], 1, T).
remove_at(X, [H|T], K, [H|R]) :- K > 1, K1 is K - 1, remove_at(X, T, K1, R).
```
21. Insertar un elemento en una lista en una posición dada.
```prolog
% Inserta un elemento en una lista en la posición dada.
% El índice K debe ser mayor que 0.
insert_at(X, L, 1, [X|L]).
insert_at(X, [H|T], K, [H|R]) :- K > 1, K1 is K - 1, insert_at(X, T, K1, R).
```
22. Crear una lista con todos los enteros dentro de un rango dado.
```prolog
% Crea una lista con todos los enteros dentro de un rango dado.
% Caso base: cuando el inicio y el final son iguales.
range(I, I, [I]).
% Caso recursivo: agrega el inicio a la lista y continúa con el siguiente número.
range(I, K, [I|R]) :- I < K, I1 is I + 1, range(I1, K, R).
```
23. Extraer un número dado de elementos seleccionados aleatoriamente de una lista.
```prolog
% Extrae un número dado de elementos aleatorios de una lista.
% Utiliza un generador de números aleatorios y elimina los elementos seleccionados.
rnd_select(_, 0, []).
rnd_select(L, N, [X|R]) :- length(L, Len), random(1, Len, I), element_at(X, L, I), delete(L, X, L1), N1 is N - 1, rnd_select(L1, N1, R).
```
**Problemas en Prolog para Prácticas de Laboratorio (del 24 al 40)**

24. Lotería: Extraer N números aleatorios de un rango.
```prolog
% Extrae N números aleatorios de un rango dado.
% Utiliza la función range para generar la lista de números y luego selecciona aleatoriamente.
lotto(N, M, L) :- range(1, M, R), rnd_select(R, N, L).
```

25. Generar una permutación aleatoria de los elementos de una lista.
```prolog
% Genera una permutación aleatoria de los elementos de una lista.
% Utiliza la selección aleatoria para construir la permutación.
rnd_permu(L, P) :- length(L, Len), rnd_select(L, Len, P).
```

26. Generar combinaciones de K elementos distintos seleccionados de una lista de N elementos.
```prolog
% Genera todas las combinaciones de K elementos seleccionados de una lista dada.
combination(0, _, []).
combination(K, [H|T], [H|Comb]) :- K > 0, K1 is K - 1, combination(K1, T, Comb).
combination(K, [_|T], Comb) :- K > 0, combination(K, T, Comb).
```

27. Agrupar los elementos de un conjunto en subconjuntos disjuntos.
```prolog
% Agrupa los elementos de un conjunto en subconjuntos disjuntos dados los tamaños de los subgrupos.
group([], [], []).
group([N|Ns], Es, [G|Gs]) :- combination(N, Es, G), subtract(Es, G, Rest), group(Ns, Rest, Gs).
```

28. Ordenar una lista de listas de acuerdo con la longitud de las sublistas.
```prolog
% Ordena una lista de listas de acuerdo con la longitud de las sublistas.
lsort(L, S) :- map_list_to_pairs(length, L, P), keysort(P, SP), pairs_values(SP, S).
```

29. Ordenar una lista de listas según la frecuencia de la longitud de las sublistas.
```prolog
% Ordena una lista de listas de acuerdo con la frecuencia de la longitud de las sublistas.
length_frequency(L, F) :- map_list_to_pairs(length, L, P), msort(P, SP), encode(SP, F).
```

30. Determinar si un número entero es primo.
```prolog
% Verifica si un número entero dado es primo.
is_prime(2).
is_prime(3).
is_prime(P) :- P > 3, P mod 2 =\= 0, \\+ has_factor(P, 3).

% Verifica si un número tiene factores.
has_factor(N, L) :- N mod L =:= 0.
has_factor(N, L) :- L * L < N, L2 is L + 2, has_factor(N, L2).
```

31. Determinar el máximo común divisor de dos números positivos.
```prolog
% Calcula el máximo común divisor usando el algoritmo de Euclides.
gcd(X, 0, X) :- X > 0.
gcd(X, Y, G) :- Y > 0, R is X mod Y, gcd(Y, R, G).
```

32. Determinar si dos números positivos son coprimos.
```prolog
% Dos números son coprimos si su máximo común divisor es 1.
coprime(X, Y) :- gcd(X, Y, 1).
```

33. Calcular la función totiente de Euler phi(m).
```prolog
% Calcula la función totiente de Euler, que cuenta cuántos números menores que M son coprimos con M.
totient(1, 1).
totient(M, Phi) :- M > 1, totient_acc(M, M, 0, Phi).

totient_acc(_, 0, Acc, Acc).
totient_acc(M, K, Acc, Phi) :- K > 0, coprime(M, K), Acc1 is Acc + 1, K1 is K - 1, totient_acc(M, K1, Acc1, Phi).
totient_acc(M, K, Acc, Phi) :- K > 0, \\+ coprime(M, K), K1 is K - 1, totient_acc(M, K1, Acc, Phi).
```

34. Determinar los factores primos de un número entero positivo.
```prolog
% Encuentra los factores primos de un número entero positivo.
prime_factors(N, L) :- N > 1, prime_factors(N, 2, L).

prime_factors(1, _, []).
prime_factors(N, F, [F|L]) :- N > 1, 0 is N mod F, N1 is N // F, prime_factors(N1, F, L).
prime_factors(N, F, L) :- N > 1, F * F < N, next_factor(F, F1), prime_factors(N, F1, L).

next_factor(2, 3).
next_factor(F, F1) :- F > 2, F1 is F + 2.
```

35. Determinar los factores primos de un número entero positivo (con multiplicidad).
```prolog
% Determina los factores primos con su multiplicidad.
prime_factors_mult(N, L) :- prime_factors(N, F), encode(F, L).
```

36. Calcular la función totiente de Euler phi(m) mejorada.
```prolog
% Calcula la función totiente usando los factores primos con multiplicidad para mejorar la eficiencia.
totient_improved(N, Phi) :- prime_factors_mult(N, F), totient_phi(F, Phi).

totient_phi([], 1).
totient_phi([[P, M]|T], Phi) :- totient_phi(T, Phi1), Phi is Phi1 * (P - 1) * P ** (M - 1).
```

37. Comparar dos métodos para calcular la función totiente de Euler.
```prolog
% Compara los dos métodos para calcular la función totiente.
compare_totient(N) :- totient(N, Phi1), totient_improved(N, Phi2), write('Phi (método básico): '), write(Phi1), nl, write('Phi (método mejorado): '), write(Phi2), nl.
```

38. Lista de números primos en un rango.
```prolog
% Genera una lista de números primos en un rango dado.
prime_list(Low, High, Primes) :- findall(P, (between(Low, High, P), is_prime(P)), Primes).
```

39. Conjetura de Goldbach.
```prolog
% Encuentra dos números primos que sumen un número par dado según la conjetura de Goldbach.
goldbach(N, [P1, P2]) :- N > 2, N mod 2 =:= 0, prime_list(2, N, Primes), member(P1, Primes), P2 is N - P1, is_prime(P2).
```

40. Lista de composiciones de Goldbach para un rango de números.
```prolog
% Encuentra las composiciones de Goldbach para todos los números pares dentro de un rango.
goldbach_list(Low, High, L) :- findall([N, P1, P2], (between(Low, High, N), N mod 2 =:= 0, goldbach(N, [P1, P2])), L).
```
**Problemas en Prolog para Prácticas de Laboratorio (del 41 al 60)**

41. Tabla de verdad para expresiones lógicas.
```prolog
% Genera una tabla de verdad para una expresión lógica en dos variables.
% A y B son las variables lógicas y Expr es la expresión lógica que se evalúa.
table(A, B, Expr) :- write(A), write(' '), write(B), write(' '), call(Expr), nl, fail.
table(_, _, _).
```

42. Tabla de verdad para expresiones lógicas (operadores).
```prolog
% Utiliza operadores lógicos para definir expresiones y generar tablas de verdad.
% Definición de los operadores básicos AND, OR, NAND, NOR, XOR, IMPLICACIÓN, EQUIVALENCIA.
and(A, B) :- A, B.
or(A, _) :- A.
or(_, B) :- B.
nand(A, B) :- \+ (A, B).
nor(A, B) :- \+ (A; B).
xor(A, B) :- A, \+ B; \+ A, B.
impl(A, B) :- \+ A; B.
equ(A, B) :- A, B; \+ A, \+ B.
```

43. Código Gray.
```prolog
% Genera el código Gray de N bits.
% La secuencia de código Gray es una secuencia binaria en la que dos valores consecutivos difieren en solo un bit.
gray(1, ['0', '1']).
gray(N, C) :- N > 1, N1 is N - 1, gray(N1, C1), maplist(atom_concat('0'), C1, C0), reverse(C1, C1R), maplist(atom_concat('1'), C1R, C1G), append(C0, C1G, C).
```

44. Código Huffman.
```prolog
% Genera un árbol de Huffman dado un conjunto de frecuencias.
% El algoritmo de Huffman se utiliza para la compresión de datos, construyendo un árbol binario óptimo para la codificación.
huffman(Fs, Hs) :- length(Fs, Len), Len > 1, sort(2, @=<, Fs, Sorted), huffman_tree(Sorted, Hs).

% Caso base para construir el árbol de Huffman.
huffman_tree([fr(X, _) | []], hc(X, '0')).
huffman_tree([fr(_, _) | [fr(_, _) | _]], _).
```

45. Comprobar si un término dado representa un árbol binario.
```prolog
% Verifica si un término dado representa un árbol binario.
% Un árbol binario está compuesto por un nodo raíz y dos subárboles.
istree(nil).
istree(t(_, L, R)) :- istree(L), istree(R).
```

46. Construir árboles binarios completamente equilibrados.
```prolog
% Construye un árbol binario completamente equilibrado con N nodos.
% Un árbol completamente equilibrado tiene subárboles cuya diferencia de tamaño es como máximo 1.
cbal_tree(0, nil).
cbal_tree(N, t('x', L, R)) :- N > 0, N1 is N - 1, divide(N1, N2, N3), cbal_tree(N2, L), cbal_tree(N3, R).

% Divide el número de nodos entre los dos subárboles.
divide(N, N1, N2) :- N1 is N // 2, N2 is N - N1.
```

47. Árboles binarios simétricos.
```prolog
% Comprueba si un árbol binario es simétrico.
% Un árbol es simétrico si sus subárboles izquierdo y derecho son espejos entre sí.
symmetric(nil).
symmetric(t(_, L, R)) :- mirror(L, R).

% Verifica si un árbol es el espejo de otro.
mirror(nil, nil).
mirror(t(_, L1, R1), t(_, L2, R2)) :- mirror(L1, R2), mirror(R1, L2).
```

48. Árboles binarios de búsqueda.
```prolog
% Construye un árbol binario de búsqueda a partir de una lista de enteros.
% Un árbol binario de búsqueda es un árbol en el cual, para cada nodo, todos los elementos en el subárbol izquierdo son menores y en el subárbol derecho son mayores.
construct([], nil).
construct([X|Xs], T) :- construct(Xs, T1), add(X, T1, T).

% Añade un nodo a un árbol binario de búsqueda.
add(X, nil, t(X, nil, nil)).
add(X, t(Root, L, R), t(Root, NL, R)) :- X < Root, add(X, L, NL).
add(X, t(Root, L, R), t(Root, L, NR)) :- X >= Root, add(X, R, NR).
```

49. Aplicar el paradigma de generar y probar para construir todos los árboles binarios simétricos y completamente equilibrados con un número dado de nodos.
```prolog
% Genera todos los árboles binarios simétricos y completamente equilibrados con N nodos.
% Utiliza el paradigma de generar y probar para encontrar todos los árboles que cumplen ambas propiedades.
sym_cbal_trees(N, Ts) :- findall(T, (cbal_tree(N, T), symmetric(T)), Ts).
```

50. Construir árboles binarios equilibrados en altura.
```prolog
% Construye un árbol binario equilibrado en altura con una altura dada.
% Un árbol equilibrado en altura tiene subárboles cuya diferencia de altura es como máximo 1.
hbal_tree(0, nil).
hbal_tree(H, t('x', L, R)) :- H > 0, H1 is H - 1, H2 is H1 - 1, hbal_tree(H1, L), hbal_tree(H2, R).
```

51. Construir árboles binarios equilibrados en altura con un número dado de nodos.
```prolog
% Construye un árbol binario equilibrado en altura con un número dado de nodos.
% Se determina la altura máxima posible para el número de nodos dado.
hbal_tree_nodes(N, T) :- hbal_height(N, H), hbal_tree(H, T), count_nodes(T, N).

% Calcula la altura máxima de un árbol equilibrado en altura con N nodos.
hbal_height(N, H) :- H is ceiling(log(N + 1) / log(2)).
```

52. Contar las hojas de un árbol binario.
```prolog
% Cuenta el número de hojas en un árbol binario.
% Una hoja es un nodo sin hijos.
count_leaves(nil, 0).
count_leaves(t(_, nil, nil), 1).
count_leaves(t(_, L, R), N) :- count_leaves(L, NL), count_leaves(R, NR), N is NL + NR.
```

53. Colectar las hojas de un árbol binario en una lista.
```prolog
% Colecta todas las hojas de un árbol binario en una lista.
% Las hojas son nodos que no tienen subárboles.
leaves(nil, []).
leaves(t(X, nil, nil), [X]).
leaves(t(_, L, R), Leaves) :- leaves(L, LL), leaves(R, LR), append(LL, LR, Leaves).
```

54. Colectar los nodos internos de un árbol binario en una lista.
```prolog
% Colecta todos los nodos internos de un árbol binario en una lista.
% Los nodos internos son aquellos que tienen al menos un subárbol.
internals(nil, []).
internals(t(_, nil, nil), []).
internals(t(X, L, R), [X|Internals]) :- (L \= nil; R \= nil), internals(L, IL), internals(R, IR), append(IL, IR, Internals).
```

55. Colectar los nodos en un nivel dado en una lista.
```prolog
% Colecta todos los nodos de un árbol binario en un nivel dado.
% El nivel se cuenta a partir de 1 (el nivel raíz).
atlevel(nil, _, []).
atlevel(t(X, _, _), 1, [X]).
atlevel(t(_, L, R), N, Nodes) :- N > 1, N1 is N - 1, atlevel(L, N1, LN), atlevel(R, N1, RN), append(LN, RN, Nodes).
```

**Problemas en Prolog para Prácticas de Laboratorio (del 56 al 66)**

56. Construir un árbol binario completo.
```prolog
% Construye un árbol binario completo con N nodos.
% Un árbol binario completo es un árbol donde todos los niveles, excepto posiblemente el último, están completamente llenos y todos los nodos están lo más a la izquierda posible.
complete_binary_tree(0, nil).
complete_binary_tree(N, t('x', L, R)) :- N > 0, N1 is N - 1, divide(N1, NL, NR), complete_binary_tree(NL, L), complete_binary_tree(NR, R).

% Divide el número de nodos entre los subárboles izquierdo y derecho.
divide(N, N1, N2) :- N1 is N // 2, N2 is N - N1.
```

57. Representación en cadena de un árbol binario.
```prolog
% Genera la representación en cadena de un árbol binario.
% La representación en cadena sigue la forma t(Raíz, Izquierdo, Derecho).
tree_string(nil, 'nil').
tree_string(t(X, L, R), S) :- tree_string(L, SL), tree_string(R, SR), format(atom(S), 't(~w, ~w, ~w)', [X, SL, SR]).

% Ejemplo de uso:
% ?- tree_string(t(a, t(b, nil, nil), t(c, nil, nil)), S).
% S = 't(a, t(b, nil, nil), t(c, nil, nil))'.
```

58. Secuencia de recorrido en preorden y en orden de un árbol binario.
```prolog
% Genera la secuencia de recorrido en preorden de un árbol binario.
% Preorden: visitar la raíz, luego el subárbol izquierdo, luego el subárbol derecho.
preorder(nil, []).
preorder(t(X, L, R), [X|Seq]) :- preorder(L, LL), preorder(R, LR), append(LL, LR, Seq).

% Genera la secuencia de recorrido en orden de un árbol binario.
% En orden: visitar el subárbol izquierdo, luego la raíz, luego el subárbol derecho.
inorder(nil, []).
inorder(t(X, L, R), Seq) :- inorder(L, LL), inorder(R, LR), append(LL, [X|LR], Seq).
```

59. Representación en cadena con puntos de un árbol binario.
```prolog
% Genera la representación en cadena con puntos de un árbol binario.
% La representación con puntos usa '.' para denotar subárboles vacíos.
tree_dotstring(nil, '.').
tree_dotstring(t(X, L, R), S) :- tree_dotstring(L, SL), tree_dotstring(R, SR), format(atom(S), '~w~w~w', [X, SL, SR]).

% Ejemplo de uso:
% ?- tree_dotstring(t(a, t(b, nil, nil), t(c, nil, nil)), S).
% S = 'ab..c..'.
```

60. Árboles multiway.
```prolog
% Verifica si un término dado representa un árbol multiway.
% Un árbol multiway tiene un nodo raíz y una lista de subárboles (que también son árboles multiway).
istree(t(_, [])) :- !.
istree(t(_, Forest)) :- is_forest(Forest).

% Verifica si una lista de términos representa un bosque de árboles multiway.
is_forest([]).
is_forest([T|Ts]) :- istree(T), is_forest(Ts).
```

61. Contar los nodos de un árbol multiway.
```prolog
% Cuenta el número de nodos en un árbol multiway.
% Cada nodo se cuenta una vez, incluyendo la raíz y todos los nodos en los subárboles.
nnodes(t(_, Forest), N) :- nnodes_forest(Forest, NF), N is NF + 1.

% Cuenta el número de nodos en un bosque de árboles multiway.
nnodes_forest([], 0).
nnodes_forest([T|Ts], N) :- nnodes(T, NT), nnodes_forest(Ts, NF), N is NT + NF.
```

62. Determinar la longitud del camino interno de un árbol multiway.
```prolog
% Calcula la longitud del camino interno de un árbol multiway.
% La longitud del camino interno es la suma de las longitudes de todos los caminos desde la raíz hasta cada nodo.
ipl(t(_, Forest), IPL) :- ipl_forest(Forest, 1, IPL).

% Calcula la longitud del camino interno para un bosque de árboles multiway.
ipl_forest([], _, 0).
ipl_forest([T|Ts], Depth, IPL) :- ipl(T, DIPL), NextDepth is Depth + 1, ipl_forest(Ts, NextDepth, RestIPL), IPL is DIPL + RestIPL.

% Calcula la longitud del camino interno de un árbol.
ipl(t(_, Forest), DIPL) :- ipl_forest(Forest, 1, IPL).
```

63. Construir un árbol binario completo.
```prolog
% Construye un árbol binario completo con N nodos.
% Un árbol binario completo es aquel donde todos los niveles, excepto posiblemente el último, están completamente llenos y los nodos están lo más a la izquierda posible.
complete_binary_tree(0, nil).
complete_binary_tree(N, t('x', L, R)) :- N > 0, N1 is N - 1, divide(N1, NL, NR), complete_binary_tree(NL, L), complete_binary_tree(NR, R).

% Divide el número de nodos para repartirlos entre los subárboles izquierdo y derecho.
divide(N, N1, N2) :- N1 is N // 2, N2 is N - N1.
```

64. Diseño de un árbol binario (1).
```prolog
% Genera el diseño de un árbol binario posicionando cada nodo en una cuadrícula.
% La posición x se determina según el orden en el que se visitan los nodos (inorder).
layout_binary_tree(T, PT) :- layout(T, 1, 1, PT, _).

% Layout con coordenadas iniciales (X, Y).
layout(nil, _, _, nil, 0).
layout(t(W, L, R), X, Y, t(W, X, Y, PL, PR), X2) :-
    X1 is X + 1,
    layout(L, X, Y + 1, PL, XL),
    layout(R, X1, Y + 1, PR, X2).
```

65. Diseño de un árbol binario (2).
```prolog
% Diseño alternativo donde los nodos se posicionan de manera que mantengan un espacio constante entre niveles.
layout_binary_tree_2(T, PT) :- layout2(T, 1, 1, PT, _).

% Layout con espaciamiento fijo entre los nodos en cada nivel.
layout2(nil, _, _, nil, 0).
layout2(t(W, L, R), X, Y, t(W, X, Y, PL, PR), X2) :-
    layout2(L, X, Y + 2, PL, XL),
    X1 is XL + 1,
    layout2(R, X1, Y + 2, PR, X2).
```

66. Diseño de un árbol binario (3).
```prolog
% Genera un diseño compacto de un árbol binario manteniendo una simetría adecuada en cada nodo.
% Este método se enfoca en minimizar el espacio horizontal requerido mientras se mantiene la simetría del árbol.
layout_binary_tree_3(T, PT) :- layout3(T, 1, 1, PT, _).

% Layout utilizando un enfoque compacto y simétrico.
layout3(nil, _, _, nil, 0).
layout3(t(W, L, R), X, Y, t(W, X, Y, PL, PR), X2) :-
    layout3(L, X, Y + 1, PL, XL),
    X1 is XL + 1,
    layout3(R, X1, Y + 1, PR, X2).
```

67. Representación de un árbol binario como una cadena.
```prolog
% Representa un árbol binario como una cadena y viceversa.
% Se usa una notación en la cual cada nodo se representa como una raíz con sus hijos izquierdo y derecho.
tree_string(nil, '').
tree_string(t(X, L, R), S) :- tree_string(L, SL), tree_string(R, SR), format(atom(S), '~w(~w,~w)', [X, SL, SR]).
```

68. Secuencias de recorrido (inorder y preorder) de un árbol binario.
```prolog
% Genera la secuencia de recorrido en inorder de un árbol binario.
% El recorrido inorder visita primero el subárbol izquierdo, luego la raíz, y finalmente el subárbol derecho.
inorder(nil, []).
inorder(t(X, L, R), Seq) :- inorder(L, LL), inorder(R, LR), append(LL, [X|LR], Seq).

% Genera la secuencia de recorrido en preorder de un árbol binario.
% El recorrido preorder visita la raíz primero, luego el subárbol izquierdo, y finalmente el subárbol derecho.
preorder(nil, []).
preorder(t(X, L, R), Seq) :- preorder(L, LL), preorder(R, LR), append([X|LL], LR, Seq).
```

69. Representación dotstring de un árbol binario.
```prolog
% Representa un árbol binario utilizando la notación dotstring.
% En una notación dotstring, los nodos vacíos se representan con un punto ('.').
tree_dotstring(nil, '.').
tree_dotstring(t(X, L, R), S) :- tree_dotstring(L, SL), tree_dotstring(R, SR), format(atom(S), '~w~w~w', [X, SL, SR]).
```

70. Árboles multiway: Verificar si un término dado representa un árbol multiway.
```prolog
% Verifica si un término representa un árbol multiway.
% Un árbol multiway está compuesto por un nodo raíz y un conjunto de subárboles.
istree(t(_, [])) :- !.
istree(t(_, Forest)) :- is_forest(Forest).

% Verifica si una lista de términos representa un bosque de árboles multiway.
is_forest([]).
is_forest([T|Ts]) :- istree(T), is_forest(Ts).
```

71. Longitud interna de un árbol multiway.
```prolog
% Calcula la longitud interna de un árbol multiway.
% La longitud interna es la suma de todas las longitudes de los caminos desde la raíz a cada nodo.
ipl(t(_, []), 0).
ipl(t(_, Forest), IPL) :- ipl_forest(Forest, 1, IPL).

% Calcula la longitud interna para cada subárbol en el bosque.
ipl_forest([], _, 0).
ipl_forest([T|Ts], Depth, IPL) :- ipl(T, IPLT), IPLT1 is IPLT + Depth, Depth1 is Depth + 1, ipl_forest(Ts, Depth1, IPLF), IPL is IPLT1 + IPLF.
```

72. Orden de recorrido de abajo hacia arriba de un árbol multiway.
```prolog
% Construye el orden de recorrido de abajo hacia arriba para un árbol multiway.
% El recorrido de abajo hacia arriba significa visitar primero todas las hojas y luego los nodos internos.
bottom_up(t(X, []), [X]).
bottom_up(t(_, Forest), Seq) :- maplist(bottom_up, Forest, SeqList), append(SeqList, [X], Seq).
```

73. Representación de un árbol multiway al estilo Lisp.
```prolog
% Representa un árbol multiway usando una notación al estilo Lisp.
% La notación Lisp representa un nodo con una lista, donde el primer elemento es la raíz y los demás son los subárboles.
tree_ltl(t(X, []), ['(', X, ')']).
tree_ltl(t(X, Forest), ['(', X | LTL]) :- maplist(tree_ltl, Forest, LTLList), append(LTLList, [')'], LTL).
```

74. Caminos desde un nodo a otro en un grafo.
```prolog
% Encuentra todos los caminos acíclicos entre dos nodos en un grafo.
% Un camino acíclico es un camino que no visita ningún nodo más de una vez.
path(G, A, B, P) :- travel(A, B, G, [A], Q), reverse(Q, P).

% Recorre el grafo para encontrar el camino entre A y B.
travel(A, B, _, P, [B|P]) :- edge(A, B).
travel(A, B, G, Visited, Path) :- edge(A, C), C \= B, \+ member(C, Visited), travel(C, B, G, [C|Visited], Path).
```

75. Ciclos en un grafo desde un nodo dado.
```prolog
% Encuentra todos los ciclos desde un nodo dado en un grafo.
% Un ciclo es un camino cerrado que regresa al nodo de inicio.
cycle(G, A, P) :- travel(A, A, G, [A], Q), reverse(Q, P).
```

76. Construcción de todos los árboles generadores de un grafo.
```prolog
% Construye todos los árboles generadores de un grafo dado.
% Un árbol generador es un subgrafo que conecta todos los nodos sin formar ciclos.
s_tree(Graph, Tree) :- findall(Node, member(edge(Node, _), Graph), Nodes), build_tree(Nodes, Graph, [], Tree).

% Construye el árbol generador.
build_tree([], _, T, T).
build_tree([N|Ns], Graph, Acc, T) :- member(edge(N, M), Graph), \+ member(edge(N, M), Acc), build_tree(Ns, Graph, [edge(N, M)|Acc], T).
```
**Problemas en Prolog para Prácticas de Laboratorio (del 77 al 99)**

77. Verificar si un término representa un árbol multiway.
```prolog
% Verifica si un término dado representa un árbol multiway.
% Un árbol multiway tiene un nodo raíz y una lista de subárboles (que también son árboles multiway).
istree(t(_, [])) :- !. % Caso base: un nodo con una lista vacía de hijos es un árbol multiway válido.
istree(t(_, Forest)) :- is_forest(Forest). % Verifica si todos los elementos de la lista de subárboles son también árboles.

% Verifica si una lista de términos representa un bosque de árboles multiway.
is_forest([]). % Una lista vacía es un bosque válido.
is_forest([T|Ts]) :- istree(T), is_forest(Ts). % Verifica que cada árbol en la lista sea un árbol multiway válido.
```

78. Contar el número de nodos de un árbol multiway.
```prolog
% Cuenta el número de nodos de un árbol multiway.
% Un nodo es un elemento del árbol, incluyendo el nodo raíz y todos sus hijos.
nnodes(t(_, []), 1). % Caso base: un árbol con solo un nodo tiene un nodo.
nnodes(t(_, Forest), N) :- nnodes_forest(Forest, NF), N is NF + 1. % Cuenta el nodo raíz y los nodos de cada subárbol.

% Cuenta el número de nodos en un bosque de árboles multiway.
nnodes_forest([], 0). % Un bosque vacío no tiene nodos.
nnodes_forest([T|Ts], N) :- nnodes(T, N1), nnodes_forest(Ts, N2), N is N1 + N2. % Suma los nodos de cada árbol en el bosque.
```

79. Construir un árbol a partir de una secuencia de nodos.
```prolog
% Construye un árbol multiway a partir de una secuencia de nodos y el carácter especial '^'.
% '^' indica el final de un conjunto de hijos de un nodo.
tree(String, Tree) :- atom_chars(String, Chars), tree_helper(Chars, Tree, []). % Convierte la cadena en una lista de caracteres y construye el árbol.

tree_helper([X|Chars], t(X, Forest), Rest) :- tree_forest(Chars, Forest, Rest).

% Construye el bosque asociado a un nodo.
tree_forest(['^'|Rest], [], Rest). % '^' indica el final de los hijos.
tree_forest(Chars, [T|Ts], Rest) :- tree_helper(Chars, T, Rest1), tree_forest(Rest1, Ts, Rest). % Construye cada subárbol y continúa con el resto de los hijos.
```

80. Determinar la longitud del camino interno de un árbol.
```prolog
% Calcula la longitud del camino interno de un árbol multiway.
% La longitud del camino interno es la suma de las distancias desde la raíz hasta cada nodo.
ipl(t(_, Forest), IPL) :- ipl_forest(Forest, 1, IPL). % La distancia inicial desde la raíz es 1.

% Calcula la longitud del camino interno para un bosque de árboles multiway.
ipl_forest([], _, 0). % Un bosque vacío tiene una longitud de camino interno de 0.
ipl_forest([T|Ts], D, IPL) :- ipl(T, D, IPL1), D1 is D + 1, ipl_forest(Ts, D1, IPL2), IPL is IPL1 + IPL2.

% Calcula la longitud del camino interno de un árbol dado un nivel.
ipl(t(_, Forest), D, IPL) :- ipl_forest(Forest, D + 1, IPL1), IPL is IPL1 + D.
```

81. Construir la secuencia en orden de abajo hacia arriba de los nodos del árbol.
```prolog
% Construye la secuencia de recorrido de abajo hacia arriba de un árbol multiway.
% La secuencia de abajo hacia arriba significa recorrer los hijos antes que el nodo raíz.
bottom_up(t(X, []), [X]). % Caso base: si el árbol no tiene hijos, la secuencia es solo el nodo raíz.
bottom_up(t(X, Forest), Seq) :- bottom_up_forest(Forest, SeqForest), append(SeqForest, [X], Seq). % Primero se recorren los hijos y luego se añade la raíz.

% Construye la secuencia de abajo hacia arriba para un bosque de árboles multiway.
bottom_up_forest([], []). % Un bosque vacío no tiene nodos que recorrer.
bottom_up_forest([T|Ts], Seq) :- bottom_up(T, SeqT), bottom_up_forest(Ts, SeqTs), append(SeqT, SeqTs, Seq). % Recorre cada árbol del bosque.
```

82. Representación de un árbol multiway en estilo Lisp.
```prolog
% Representa un árbol multiway en estilo Lisp.
% En estilo Lisp, un árbol se representa como '(raíz subárbol1 subárbol2 ... subárbolN)'.
tree_lisp(t(X, []), X). % Caso base: si el árbol no tiene hijos, se representa como el nodo.
tree_lisp(t(X, Forest), S) :- forest_lisp(Forest, SF), atomic_list_concat(['(', X, ' ', SF, ')'], S). % Concatena la raíz y sus subárboles.

% Representa un bosque de árboles multiway en estilo Lisp.
forest_lisp([], ''). % Un bosque vacío no tiene representación.
forest_lisp([T|Ts], S) :- tree_lisp(T, ST), forest_lisp(Ts, STs), atomic_list_concat([ST, ' ', STs], S). % Concatena las representaciones de cada árbol en el bosque.
```

83. Convertir entre diferentes representaciones de grafos.
```prolog
% Convierta entre diferentes representaciones de grafos.
% Un grafo puede representarse mediante aristas, lista de adyacencia o un término compuesto de nodos y aristas.
% Aquí se muestra cómo convertir una lista de aristas en una lista de adyacencia.
edges_to_adj_list(Edges, AdjList) :- findall(Node-Neighbors, (member(Node, Edges), findall(Neighbor, member(Node-Neighbor, Edges), Neighbors)), AdjList).
```

84. Camino de un nodo a otro en un grafo.
```prolog
% Encuentra un camino entre dos nodos en un grafo.
% El grafo se da como una lista de aristas.
path(Graph, A, B, Path) :- travel(A, B, [A], Q, Graph), reverse(Q, Path).

% Función auxiliar para encontrar un camino.
travel(A, B, P, [B|P], _) :- edge(A, B).
travel(A, B, Visited, Path, Graph) :- edge(A, C), C ≠ B,  B, \u+member(C, Visited), travel(C, B, [C|Visited], Path, Graph).
```

85. Ciclo desde un nodo dado.
```prolog
% Encuentra un ciclo en un grafo comenzando desde un nodo dado.
cycle(Graph, Start, Cycle) :- path(Graph, Start, Start, Cycle), length(Cycle, Len), Len > 2.
```

86. Construir todos los árboles generadores de un grafo.
```prolog
% Encuentra todos los árboles generadores de un grafo dado.
s_tree(Graph, Tree) :- subset(Graph, Tree), is_tree(Tree), is_connected(Tree).

% Verifica si un conjunto de aristas forma un árbol.
is_tree(Graph).
```

87. Árbol generador mínimo de un grafo etiquetado.
```prolog
% Construye el árbol generador mínimo de un grafo etiquetado.
ms_tree(Graph, Tree, Cost).
```

88. Isomorfismo de grafos.
```prolog
% Verifica si dos grafos son isomorfos.
isomorphic(G1, G2).
```

89. Determinar el grado de un nodo y la coloración del grafo.
```prolog
% Determina el grado de un nodo en un grafo.
degree(Graph, Node, Degree).
```

90. Problema de las ocho reinas.
```prolog
% Soluciona el problema de las ocho reinas.
eight_queens(Board).
```

91. Problema del paseo del caballo.
```prolog
% Encuentra un paseo del caballo en un tablero NxN.
knights_tour(N, Path).
```

92. Conjetura de Von Koch.
```prolog
% Soluciona la conjetura de Von Koch.
von_koch(Tree).
```

93. Puzzle aritmético.
```prolog
% Resuelve un puzzle aritmético insertando operadores entre números.
arithmetic_puzzle(Numbers, Result).
```

94. Generar grafos simples K-regulares.
```prolog
% Genera grafos K-regulares con N nodos.
k_regular_graph(N, K, Graph).
```

95. Números escritos en palabras en inglés.
```prolog
% Convierte un número en su representación en palabras en inglés.
number_words(Number, Words).
```

96. Comprobador de sintaxis.
```prolog
% Comprueba si una cadena es un identificador válido según una gramática dada.
identifier(String).
```

97. Sudoku.
```prolog
% Resuelve un puzzle de Sudoku.
sudoku(Puzzle, Solution).
```

98. Nonogramas.
```prolog
% Resuelve un puzzle de nonogramas.
nonogram(Rows, Cols, Solution).
```

99. Puzzle de crucigrama.
```prolog
% Resuelve un puzzle de crucigrama dado un conjunto de palabras y una cuadrícula.
crossword(Words, Grid).
```


