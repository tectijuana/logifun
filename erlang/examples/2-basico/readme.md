
![Ejercicios](https://github.com/user-attachments/assets/ce2d8f99-d5d9-4c69-9358-2238ff17990a)


### 1. Hola Mundo

**Objetivo de la práctica:** Aprender a usar la función `io:format/2` para mostrar texto en la consola y familiarizarse con la sintaxis básica de Erlang.

```erlang
% Imprime "Hola Mundo" en la consola
io:format("Hola Mundo~n").
```

### 2. Capturar números desde el teclado

**Objetivo de la práctica:** Aprender a capturar entrada del usuario desde el teclado, procesar esa entrada y realizar operaciones con los datos ingresados.

```erlang
% Captura un número desde el teclado y lo imprime
capturar_numero() ->
    io:format("Ingrese un número: "),
    {ok, [Numero]} = io:fread("", "~d"),
    io:format("El número ingresado es: ~p~n", [Numero]).
```

### 3. Suma de dos números

**Objetivo de la práctica:** Crear una función simple que toma dos argumentos y devuelve su suma, introduciendo funciones en Erlang.

```erlang
% Define una función que suma dos números
suma(X, Y) -> X + Y.
```

### 4. Factorial Recursivo

**Objetivo de la práctica:** Aprender a usar la recursión para resolver problemas matemáticos, calculando el factorial de un número.

```erlang
% Calcula el factorial de un número de forma recursiva
factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).
```

### 5. Fibonacci Recursivo

**Objetivo de la práctica:** Implementar la secuencia de Fibonacci usando recursión para entender la naturaleza de las llamadas recursivas.

```erlang
% Genera números de la secuencia de Fibonacci recursivamente
fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) -> fibonacci(N-1) + fibonacci(N-2).
```

### 6. Número Par o Impar

**Objetivo de la práctica:** Utilizar guardas en Erlang para decidir si un número es par o impar.

```erlang
% Comprueba si un número es par o impar usando guardas
par_o_impar(N) when N rem 2 == 0 -> "Par";
par_o_impar(_) -> "Impar".
```

### 7. Máximo de dos números

**Objetivo de la práctica:** Comparar dos números y devolver el mayor usando funciones con guardas.

```erlang
% Devuelve el mayor de dos números
max(X, Y) when X > Y -> X;
max(_, Y) -> Y.
```

### 8. Reverso de una lista

**Objetivo de la práctica:** Usar funciones de listas de Erlang para invertir el orden de los elementos de una lista.

```erlang
% Revierte el orden de una lista
reverso(L) -> lists:reverse(L).
```

### 9. Concatenar dos listas

**Objetivo de la práctica:** Aprender a unir dos listas usando el operador de concatenación.

```erlang
% Concatena dos listas en una sola
concatenar(L1, L2) -> L1 ++ L2.
```

### 10. Longitud de una lista

**Objetivo de la práctica:** Calcular la longitud de una lista utilizando la función estándar `length/1`.

```erlang
% Calcula la longitud de una lista
longitud(L) -> length(L).
```

### 11. Sumar elementos de una lista

**Objetivo de la práctica:** Implementar una función recursiva para sumar los elementos de una lista.

```erlang
% Suma los elementos de una lista recursivamente
sumar_lista([]) -> 0;
sumar_lista([H|T]) -> H + sumar_lista(T).
```

### 12. Filtrar elementos pares de una lista

**Objetivo de la práctica:** Usar listas de comprensión para filtrar elementos específicos de una lista.

```erlang
% Filtra los números pares de una lista
filtrar_pares(L) -> [X || X <- L, X rem 2 == 0].
```

### 13. Dividir una cadena

**Objetivo de la práctica:** Aprender a dividir una cadena en subcadenas usando un separador específico.

```erlang
% Divide una cadena en subcadenas usando un separador
dividir(Cadena, Separador) -> string:split(Cadena, Separador).
```

### 14. Buscar elemento en una lista

**Objetivo de la práctica:** Comprobar si un elemento pertenece a una lista usando funciones estándar de Erlang.

```erlang
% Devuelve true si el elemento está en la lista, false en caso contrario
buscar(Elemento, Lista) -> lists:member(Elemento, Lista).
```

### 15. Eliminar duplicados de una lista

**Objetivo de la práctica:** Usar la función `usort/1` para eliminar duplicados y ordenar una lista.

```erlang
% Elimina los elementos duplicados de una lista
eliminar_duplicados(L) -> lists:usort(L).
```

### 16. Mapear función a lista

**Objetivo de la práctica:** Aplicar una función a cada elemento de una lista, demostrando el uso de listas de comprensión.

```erlang
% Aplica una función a cada elemento de una lista
mapear(Fun, L) -> [Fun(X) || X <- L].
```

### 17. Comprobar si una lista está vacía

**Objetivo de la práctica:** Usar comparaciones simples para verificar si una lista está vacía.

```erlang
% Verifica si una lista está vacía
esta_vacia(L) -> L == [].
```

### 18. Convertir entero a binario

**Objetivo de la práctica:** Convertir un número entero a su representación binaria en forma de cadena.

```erlang
% Convierte un entero a su representación binaria
entero_a_binario(N) -> integer_to_list(N, 2).
```

### 19. Convertir binario a entero

**Objetivo de la práctica:** Convertir una cadena binaria a un número entero.

```erlang
% Convierte una cadena binaria a un entero
binario_a_entero(Bin) -> list_to_integer(Bin, 2).
```

### 20. Contar ocurrencias de un elemento

**Objetivo de la práctica:** Contar cuántas veces aparece un elemento en una lista utilizando listas de comprensión.

```erlang
% Cuenta cuántas veces aparece un elemento en una lista
contar_ocurrencias(Elemento, Lista) -> length([X || X <- Lista, X == Elemento]).
```

### 21. Generar lista de números del 1 al N

**Objetivo de la práctica:** Generar una lista de números secuenciales usando `lists:seq/2`.

```erlang
% Genera una lista de números del 1 al N
generar_lista(N) -> lists:seq(1, N).
```

### 22. Producto de elementos de una lista

**Objetivo de la práctica:** Calcular el producto de todos los elementos de una lista usando recursión.

```erlang
% Calcula el producto de todos los elementos de una lista
producto_lista([]) -> 1;
producto_lista([H|T]) -> H * producto_lista(T).
```

### 23. Ordenar lista

**Objetivo de la práctica:** Ordenar los elementos de una lista utilizando la función `lists:sort/1`.

```erlang
% Ordena una lista de elementos
ordenar(L) -> lists:sort(L).
```

### 24. Conversión de grados Celsius a Fahrenheit

**Objetivo de la práctica:** Realizar una conversión de unidades utilizando una función simple.

```erlang
% Convierte grados Celsius a Fahrenheit
celsius_a_fahrenheit(C) -> C * 9 / 5 + 32.
```

Otros ejercicios en: https://programming-idioms.org/cheatsheet/Erlang
