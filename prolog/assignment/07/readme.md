
![crossword](https://github.com/user-attachments/assets/054174d0-0740-4b06-8450-fb326dd5b569)


### Práctica: Resolución de un Crucigrama en Prolog

#### Introducción

En esta práctica, se nos presenta un crucigrama parcialmente completo junto con un conjunto de palabras que deben colocarse en los espacios vacíos. El objetivo es encontrar una forma compatible de colocar todas las palabras en el marco del crucigrama. Cada secuencia de celdas vacías en dirección horizontal o vertical se llama **sitio**.

El marco del crucigrama tiene espacios donde se pueden insertar las palabras. Las celdas vacías están representadas por puntos (`.`), y algunas celdas ya contienen letras, lo que restringe las posibles soluciones.

#### Objetivo

Escribir un programa en **Prolog** que resuelva este tipo de crucigrama, colocando las palabras en los sitios disponibles, respetando las letras ya presentes en el marco.

### Análisis

- **Palabras**: Un conjunto de palabras debe ser colocado en el crucigrama.
- **Sitios**: Cada secuencia continua de celdas vacías en una fila o columna constituye un sitio.
- **Restricciones**:
  - Una palabra puede colocarse en un sitio si tiene la misma longitud.
  - Si el sitio ya tiene algunas letras predefinidas, la palabra debe coincidir con esas letras.

### Representación del Problema

1. **El marco del crucigrama**: Se representa como una lista de listas, donde cada sublista es una fila del crucigrama. Los elementos de las sublistas son puntos (`.`) para celdas vacías y letras para celdas con letras predefinidas.
2. **Las palabras**: Se representan como una lista de strings.
3. **Las soluciones**: Una solución consiste en asignar las palabras a los sitios del crucigrama de manera que coincidan con las restricciones.

### Código de Solución en Prolog

A continuación se presenta una posible solución en Prolog:

```prolog
% Predicado principal: Resolver el crucigrama
solve_crossword(Puzzle, Words, Solution) :-
    find_sites(Puzzle, Sites),      % Encuentra todos los sitios vacíos
    place_words(Sites, Words),      % Coloca las palabras en los sitios
    Solution = Puzzle.

% Encuentra los sitios vacíos en el crucigrama
find_sites(Puzzle, Sites) :-
    find_horizontal_sites(Puzzle, HorSites),   % Encuentra sitios horizontales
    transpose(Puzzle, Transposed),             % Transponer el crucigrama para procesar columnas
    find_horizontal_sites(Transposed, VerSites),  % Encuentra sitios verticales
    append(HorSites, VerSites, Sites).         % Combina los sitios horizontales y verticales

% Encuentra sitios horizontales en una lista de listas (filas del crucigrama)
find_horizontal_sites([], []).
find_horizontal_sites([Row|Rows], Sites) :-
    find_sites_in_row(Row, RowSites),
    find_horizontal_sites(Rows, RemainingSites),
    append(RowSites, RemainingSites, Sites).

% Encuentra los sitios en una fila
find_sites_in_row(Row, Sites) :-
    split_row(Row, [], Sites).

split_row([], Acc, [Acc]) :- Acc \= [].
split_row([], _, []).
split_row([H|T], Acc, Sites) :-
    (H == '.' -> split_row(T, [H|Acc], Sites) ; split_row(T, [], RemainingSites), Sites = RemainingSites).

% Transponer una matriz (de filas a columnas)
transpose([], []).
transpose([[]|_], []).
transpose(Matrix, [Row|Rows]) :-
    maplist(nth1(1), Matrix, Row),
    maplist(tail, Matrix, RestMatrix),
    transpose(RestMatrix, Rows).

tail([_|T], T).

% Coloca las palabras en los sitios
place_words([], []).
place_words([Site|Sites], Words) :-
    select(Word, Words, RemainingWords),
    same_length(Site, Word),  % Verifica que la palabra tiene la misma longitud que el sitio
    match(Site, Word),        % Verifica que la palabra coincide con las restricciones del sitio
    place_words(Sites, RemainingWords).

% Verifica si una palabra coincide con las restricciones de un sitio
match([], []).
match([S|Site], [W|Word]) :-
    (S == '.' ; S == W),  % La celda es vacía o ya contiene la letra correcta
    match(Site, Word).

% Verifica si dos listas tienen la misma longitud
same_length([], []).
same_length([_|L1], [_|L2]) :-
    same_length(L1, L2).
```

### Explicación del Código

1. **solve_crossword/3**: Este predicado principal toma como entrada el crucigrama `Puzzle` (una lista de listas que representa las filas del crucigrama) y una lista de palabras `Words`. El objetivo es encontrar una solución donde se asignen las palabras a los sitios del crucigrama. El predicado busca los sitios vacíos y luego intenta colocar las palabras en esos sitios.

2. **find_sites/2**: Encuentra todos los sitios vacíos en el crucigrama, tanto horizontal como verticalmente. Utiliza la transposición del crucigrama para tratar las columnas como filas y aplicar el mismo procedimiento.

3. **find_horizontal_sites/2**: Encuentra todos los sitios horizontales vacíos en las filas del crucigrama.

4. **split_row/3**: Separa una fila en sus sitios vacíos, que son secuencias de celdas vacías (`.`).

5. **transpose/2**: Este predicado se utiliza para transponer el crucigrama, lo que permite encontrar fácilmente los sitios verticales tratándolos como sitios horizontales después de la transposición.

6. **place_words/2**: Coloca las palabras en los sitios del crucigrama de manera que respeten las restricciones de longitud y las letras predefinidas.

7. **match/2**: Verifica si una palabra puede colocarse en un sitio. Para cada celda en el sitio, la palabra puede colocarse si la celda está vacía (`.`) o si la letra ya presente en la celda coincide con la letra de la palabra.

8. **same_length/2**: Verifica que la palabra y el sitio tengan la misma longitud.

### Ejemplo de Uso

Dado el siguiente crucigrama y conjunto de palabras:

```prolog
Puzzle = [
    ['P', 'R', 'O', 'L', 'O', 'G', '.', '.', 'E', 'M', 'A', 'C'],
    ['E', '.', '.', '.', 'N', '.', '.', '.', '.', '.', '.', 'S'],
    ['R', 'L', 'I', 'N', 'U', 'X', 'A', '.', 'M', '.', '.', '.'],
    ['L', '.', '.', 'F', '.', 'M', 'A', 'C', '.', '.', '.', '.'],
    ['.', 'W', 'E', 'B', '.', '.', '.', '.', '.', '.', '.', '.']
].

Words = ['LINUX', 'SQL', 'WEB', 'PROLOG', 'EMAC', 'MAC'].
```

Consulta para resolver el crucigrama:

```prolog
?- solve_crossword(Puzzle, Words, Solution).
```

El predicado intentará asignar las palabras a los sitios vacíos del crucigrama, respetando las letras predefinidas. Si encuentra una solución válida, devolverá el crucigrama completo con las palabras colocadas en las posiciones correctas.

### Conclusión

Este ejercicio es un buen ejemplo de cómo Prolog puede resolver problemas de búsqueda y restricción, como la colocación de palabras en un crucigrama. La potencia de Prolog radica en su capacidad para manejar fácilmente listas, recursión y backtracking, lo que lo convierte en una excelente herramienta para este tipo de tareas de lógica combinatoria.
