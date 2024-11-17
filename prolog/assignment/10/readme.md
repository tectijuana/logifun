![Screenshot 2024-11-16 at 1 25 52 p m](https://github.com/user-attachments/assets/f1f581f5-e96d-4d3d-8bf9-a357e4a244de)

Crear un programa en Prolog para resolver un Sudoku es una excelente práctica para estudiantes que ya tienen experiencia con lógica y patrones en Prolog. A continuación, te proporciono un programa con comentarios detallados en español y una explicación paso a paso del procedimiento.

---

### Código en Prolog para resolver Sudoku

```prolog
% Resolver Sudoku en Prolog.
% Definimos un procedimiento que resuelve un Sudoku utilizando restricciones lógicas.

% Predicado principal que toma la matriz del Sudoku como entrada y la resuelve.
resolver_sudoku(Filas) :-
    % El Sudoku debe ser una cuadrícula 9x9.
    length(Filas, 9),
    maplist(validar_fila, Filas),
    
    % Aseguramos que cada fila tenga exactamente 9 elementos.
    maplist(length_(9), Filas),

    % Transponemos las filas a columnas.
    transponer(Filas, Columnas),
    
    % Aseguramos que todas las columnas sean válidas.
    maplist(validar_fila, Columnas),

    % Dividimos el Sudoku en regiones de 3x3 y validamos cada región.
    regiones_3x3(Filas, Regiones),
    maplist(validar_fila, Regiones),

    % Asignamos valores válidos a todas las casillas.
    maplist(maplist(restringir), Filas),

    % Mostramos la solución.
    maplist(portray_clause, Filas).

% Restricción: una fila, columna o región debe contener números del 1 al 9 sin repetir.
validar_fila(Fila) :- 
    Fila ins 1..9, 
    all_distinct(Fila).

% Verifica que cada lista tenga la longitud especificada.
length_(L, Lista) :- 
    length(Lista, L).

% Predicado para asignar valores del dominio 1..9.
restringir(Celda) :- 
    (var(Celda) -> Celda ins 1..9 ; true).

% Transponer una matriz (convertir filas en columnas).
transponer([], []).
transponer([Fila|Filas], Columnas) :-
    transponer(Filas, RestoColumnas),
    agregar_columnas(Fila, RestoColumnas, Columnas).

agregar_columnas([], [], []).
agregar_columnas([X|Xs], [Ys|RestoYs], [[X|Ys]|NuevasColumnas]) :-
    agregar_columnas(Xs, RestoYs, NuevasColumnas).

% Dividir las filas en bloques de 3x3 para extraer las regiones.
regiones_3x3([], []).
regiones_3x3([Fila1,Fila2,Fila3|Filas], Regiones) :-
    dividir_3x3(Fila1, Fila2, Fila3, Region1, Region2, Region3),
    regiones_3x3(Filas, RestRegiones),
    append([Region1, Region2, Region3], RestRegiones, Regiones).

dividir_3x3([], [], [], [], [], []).
dividir_3x3([A,B,C|R1], [D,E,F|R2], [G,H,I|R3],
            [A,D,G|T1], [B,E,H|T2], [C,F,I|T3]) :-
    dividir_3x3(R1, R2, R3, T1, T2, T3).

% Ejemplo de uso: Resolver un Sudoku.
% El número 0 representa una celda vacía.
ejemplo_sudoku([
    [6, _, _, 1, _, _, _, _, 7],
    [_, 1, 4, _, 6, _, 2, _, 5],
    [_, 9, _, 2, 8, _, _, _, 4],
    [6, _, 3, _, _, 7, 4, _, 1],
    [_, _, 1, _, 8, _, 5, _, 2],
    [5, _, 8, _, _, 4, _, _, 3],
    [_, _, 2, _, 7, _, 9, _, 8],
    [3, _, 8, _, _, _, 4, _, _],
    [9, _, _, 8, _, _, _, 3, 7]
]).

% Prueba el ejemplo:
prueba :-
    ejemplo_sudoku(Sudoku),
    resolver_sudoku(Sudoku).
```

---

### Explicación del procedimiento

1. **Representación del Sudoku**:
   - Representamos el Sudoku como una lista de listas en Prolog, donde cada sublista representa una fila.
   - Las celdas vacías se representan con variables (`_`).

2. **Validación de restricciones**:
   - Cada fila debe contener números únicos entre 1 y 9 (`validar_fila/1`).
   - Utilizamos el predicado `all_distinct/1` para asegurar que no haya números repetidos.

3. **Transposición**:
   - Transponemos la matriz para validar que cada columna también sea válida. Esto se hace intercambiando filas con columnas.

4. **Extracción de regiones 3x3**:
   - Dividimos las filas en bloques para formar las regiones 3x3.
   - Esto se logra agrupando los elementos en triples, fila por fila.

5. **Asignación de valores**:
   - Las celdas vacías se rellenan con números del dominio 1..9, utilizando restricciones (`ins 1..9`).

6. **Ejecución y prueba**:
   - Definimos un ejemplo inicial (`ejemplo_sudoku/1`) y lo resolvemos llamando a `prueba/0`.

---

### Sugerencias para los estudiantes

- **Modificar el ejemplo**: Cambia los valores iniciales del Sudoku para probar con diferentes tableros.
- **Depuración**: Añade predicados intermedios para mostrar las transposiciones o regiones generadas y verificar su corrección.
- **Optimización**: Explora cómo `maplist/2` y restricciones de dominio simplifican el código en comparación con un enfoque imperativo.

 Ejercicio es ideal para consolidar conocimientos en lógica declarativa y resolver problemas utilizando restricciones.
