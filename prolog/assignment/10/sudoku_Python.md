
### Código en Python resolver Sudoku

```python
# Resolver Sudoku en Python usando Backtracking

def resolver_sudoku(tablero):
    """
    Resuelve un Sudoku dado un tablero inicial utilizando backtracking.
    :param tablero: Matriz de 9x9 con valores enteros (0 representa una celda vacía).
    :return: True si se resuelve el Sudoku, False en caso contrario.
    """
    # Buscar una celda vacía
    vacio = encontrar_celda_vacia(tablero)
    if not vacio:
        return True  # Sudoku resuelto
    fila, columna = vacio

    # Probar números del 1 al 9
    for num in range(1, 10):
        if es_valido(tablero, num, fila, columna):
            tablero[fila][columna] = num  # Colocar el número

            # Llamada recursiva
            if resolver_sudoku(tablero):
                return True

            # Backtracking: revertir el cambio si no funciona
            tablero[fila][columna] = 0

    return False  # No se puede resolver


def es_valido(tablero, num, fila, columna):
    """
    Verifica si un número es válido en una celda específica.
    :param tablero: Matriz de 9x9.
    :param num: Número a probar (1-9).
    :param fila: Fila de la celda.
    :param columna: Columna de la celda.
    :return: True si el número es válido, False en caso contrario.
    """
    # Verificar la fila
    if num in tablero[fila]:
        return False

    # Verificar la columna
    for i in range(9):
        if tablero[i][columna] == num:
            return False

    # Verificar la región 3x3
    inicio_fila = (fila // 3) * 3
    inicio_columna = (columna // 3) * 3
    for i in range(3):
        for j in range(3):
            if tablero[inicio_fila + i][inicio_columna + j] == num:
                return False

    return True  # El número es válido


def encontrar_celda_vacia(tablero):
    """
    Encuentra una celda vacía en el tablero.
    :param tablero: Matriz de 9x9.
    :return: Una tupla (fila, columna) de la celda vacía, o None si no hay celdas vacías.
    """
    for fila in range(9):
        for columna in range(9):
            if tablero[fila][columna] == 0:  # Celda vacía
                return fila, columna
    return None


def imprimir_tablero(tablero):
    """
    Imprime el tablero de Sudoku de forma legible.
    :param tablero: Matriz de 9x9.
    """
    for i in range(9):
        if i % 3 == 0 and i != 0:
            print("-" * 21)  # Línea horizontal para separar bloques 3x3
        for j in range(9):
            if j % 3 == 0 and j != 0:
                print("| ", end="")  # Separador vertical para bloques 3x3
            print(tablero[i][j] if tablero[i][j] != 0 else ".", end=" ")
        print()


# Ejemplo de uso
if __name__ == "__main__":
    # Tablero inicial (0 representa celdas vacías)
    tablero = [
        [6, 0, 0, 1, 0, 0, 0, 0, 7],
        [0, 1, 4, 0, 6, 0, 2, 0, 5],
        [0, 9, 0, 2, 8, 0, 0, 0, 4],
        [6, 0, 3, 0, 0, 7, 4, 0, 1],
        [0, 0, 1, 0, 8, 0, 5, 0, 2],
        [5, 0, 8, 0, 0, 4, 0, 0, 3],
        [0, 0, 2, 0, 7, 0, 9, 0, 8],
        [3, 0, 8, 0, 0, 0, 4, 0, 0],
        [9, 0, 0, 8, 0, 0, 0, 3, 7],
    ]

    print("Tablero inicial:")
    imprimir_tablero(tablero)

    if resolver_sudoku(tablero):
        print("\nSudoku resuelto:")
        imprimir_tablero(tablero)
    else:
        print("\nNo se puede resolver el Sudoku.")
```

---

### Explicación del procedimiento

1. **Representación del tablero**:
   - El Sudoku se representa como una lista de listas, donde cada sublista es una fila.
   - Los valores `0` representan las celdas vacías.

2. **Backtracking**:
   - El algoritmo busca una celda vacía.
   - Intenta colocar números del 1 al 9 en esa celda, verificando si cumplen las restricciones.
   - Si un número no funciona, se elimina y se prueba el siguiente (backtracking).

3. **Validación de restricciones**:
   - Se verifica si el número es válido en la fila, la columna y la región 3x3 correspondiente.

4. **Ejecución del programa**:
   - El tablero inicial se define en el bloque `if __name__ == "__main__"`.
   - Se imprime el tablero inicial, se resuelve el Sudoku y se muestra el resultado.

5. **Impresión del tablero**:
   - La función `imprimir_tablero` organiza el tablero con separadores visuales para facilitar su lectura.

---

### Sugerencias para los estudiantes

- **Modificar el tablero**:
  Cambia los valores iniciales para probar diferentes configuraciones de Sudoku.

- **Optimizar el código**:
  Implementa heurísticas como seleccionar la celda con menos opciones posibles antes de probar números.

- **Agregar validaciones**:
  Verifica si el tablero inicial es válido antes de intentar resolverlo.



Este código es claro, fácil de entender y adecuado para aprender cómo resolver problemas complejos usando Python.
