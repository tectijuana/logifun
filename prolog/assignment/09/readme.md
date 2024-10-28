# RESOLVER UNA BUSQUEDA  DE PALABRAS

"Hay muchas palabras escondidas en este mar de letras, pero buscamos cinco con una característica concreta: no tienen ni M, ni A, ni R."

![Screenshot 2024-10-13 at 10 37 25 p m](https://github.com/user-attachments/assets/1bcff3de-2652-4a6e-baa5-6910889ea9fe)



```prolog
% Autor: Rene Solis
% Descripción: Programa en Prolog para resolver el problema de sopa de letras.

% Definimos la sopa de letras como una lista de listas de letras.
letras(
  [[r, a, m, a, m, a, m, r],
   [f, r, e, t, n, i, e, v],
   [e, r, r, a, m, r, m, a],
   [l, r, a, b, a, a, r, m],
   [b, m, m, r, u, n, a, m],
   [r, o, m, z, o, r, m, r],
   [n, m, o, i, a, r, m, e],
   [r, n, s, m, r, a, a, m],
   [r, i, r, a, a, m, m, o],
   [r, m, r, a, a, r, m, r]]).

% Buscamos palabras sin las letras M, A, ni R.
valid_word(Word) :-
    \+ member(m, Word),
    \+ member(a, Word),
    \+ member(r, Word).

% Verificamos palabras en las filas.
find_words(Row, Word) :-
    append(_, Word, Row),
    valid_word(Word).

% Encontrar todas las palabras en la sopa de letras.
solve :-
    letras(Grid),
    member(Row, Grid),
    find_words(Row, Word),
    write(Word), nl,
    fail.
solve.
```
