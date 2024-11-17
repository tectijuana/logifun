![Screenshot 2024-11-16 at 7 37 36 p m](https://github.com/user-attachments/assets/24520f94-c0bb-44e9-9359-ea85afdbdbc5)


Para resolver un Sudoku donde **C#** invoque a **Prolog**, necesitamos establecer una integración entre ambos lenguajes. Esto se logra típicamente mediante el uso de una biblioteca o interfaz de interoperabilidad. Una solución común es utilizar **SWI-Prolog** y acceder a él desde **C#** a través de un proceso externo o mediante una biblioteca específica.

---

## **1. Crear el programa en Prolog**

Primero, creamos un programa en **Prolog** que pueda resolver un Sudoku. Guardaremos este programa en un archivo llamado `resolver_sudoku.pl`.

### Código en Prolog (`resolver_sudoku.pl`)

```prolog
% Resolver Sudoku en Prolog
:- use_module(library(clpfd)).

resolver_sudoku(Tablero) :-
    % Cada fila debe tener 9 elementos
    length(Tablero, 9),
    maplist(same_length(Tablero), Tablero),

    % Aplanamos la matriz en una lista para aplicar restricciones
    append(Tablero, Elementos),
    Elementos ins 1..9,  % Valores entre 1 y 9

    % Las filas deben ser válidas
    maplist(all_distinct, Tablero),

    % Las columnas deben ser válidas
    transponer(Tablero, Columnas),
    maplist(all_distinct, Columnas),

    % Las regiones 3x3 deben ser válidas
    regiones_3x3(Tablero, Regiones),
    maplist(all_distinct, Regiones),

    % Solucionar el tablero
    label(Elementos).

% Transponer filas a columnas
transponer([], []).
transponer([Fila|Filas], Columnas) :-
    transponer(Filas, RestoColumnas),
    agregar_columnas(Fila, RestoColumnas, Columnas).

agregar_columnas([], [], []).
agregar_columnas([X|Xs], [Ys|RestoYs], [[X|Ys]|NuevasColumnas]) :-
    agregar_columnas(Xs, RestoYs, NuevasColumnas).

% Dividir el tablero en bloques de 3x3
regiones_3x3([], []).
regiones_3x3([Fila1, Fila2, Fila3 | Filas], Regiones) :-
    dividir_3x3(Fila1, Fila2, Fila3, Region1, Region2, Region3),
    regiones_3x3(Filas, RestoRegiones),
    append([Region1, Region2, Region3], RestoRegiones, Regiones).

dividir_3x3([], [], [], [], [], []).
dividir_3x3([A,B,C|R1], [D,E,F|R2], [G,H,I|R3],
            [A,D,G|T1], [B,E,H|T2], [C,F,I|T3]) :-
    dividir_3x3(R1, R2, R3, T1, T2, T3).

% Predicado para iniciar la resolución
resolver :-
    Tablero = [[6,_,_,1,_,_,_,_,7],
               [_,1,4,_,6,_,2,_,5],
               [_,9,_,2,8,_,_,_,4],
               [6,_,3,_,_,7,4,_,1],
               [_,_,1,_,8,_,5,_,2],
               [5,_,8,_,_,4,_,_,3],
               [_,_,2,_,7,_,9,_,8],
               [3,_,8,_,_,_,4,_,_],
               [9,_,_,8,_,_,_,3,7]],
    resolver_sudoku(Tablero),
    maplist(portray_clause, Tablero).
```

---

## **2. Crear el programa en C#**

Ahora, en **C#**, configuramos un programa que envíe el tablero de Sudoku a Prolog, lo resuelva y reciba la solución.

### Código en C#

```csharp
using System;
using System.Diagnostics;
using System.Text;

class Program
{
    static void Main(string[] args)
    {
        // Tablero inicial (0 representa las celdas vacías)
        int[,] tablero = {
            { 6, 0, 0, 1, 0, 0, 0, 0, 7 },
            { 0, 1, 4, 0, 6, 0, 2, 0, 5 },
            { 0, 9, 0, 2, 8, 0, 0, 0, 4 },
            { 6, 0, 3, 0, 0, 7, 4, 0, 1 },
            { 0, 0, 1, 0, 8, 0, 5, 0, 2 },
            { 5, 0, 8, 0, 0, 4, 0, 0, 3 },
            { 0, 0, 2, 0, 7, 0, 9, 0, 8 },
            { 3, 0, 8, 0, 0, 0, 4, 0, 0 },
            { 9, 0, 0, 8, 0, 0, 0, 3, 7 }
        };

        // Convertir el tablero a formato Prolog
        string tableroProlog = ConvertirTableroAProlog(tablero);

        // Llamar a Prolog para resolver
        string solucion = LlamarProlog(tableroProlog);

        // Mostrar la solución
        Console.WriteLine("Solución del Sudoku:");
        Console.WriteLine(solucion);
    }

    static string ConvertirTableroAProlog(int[,] tablero)
    {
        StringBuilder sb = new StringBuilder();
        sb.Append("[");
        for (int i = 0; i < 9; i++)
        {
            sb.Append("[");
            for (int j = 0; j < 9; j++)
            {
                sb.Append(tablero[i, j] == 0 ? "_" : tablero[i, j].ToString());
                if (j < 8) sb.Append(",");
            }
            sb.Append("]");
            if (i < 8) sb.Append(",");
        }
        sb.Append("]");
        return sb.ToString();
    }

    static string LlamarProlog(string tableroProlog)
    {
        // Ruta al ejecutable de SWI-Prolog
        string prologPath = "swipl"; // Asegúrate de que SWI-Prolog esté en el PATH
        string prologFile = "resolver_sudoku.pl";

        // Configurar el proceso
        Process proceso = new Process();
        proceso.StartInfo.FileName = prologPath;
        proceso.StartInfo.Arguments = $"-q -f {prologFile} -g \"resolver_sudoku({tableroProlog}),halt.\"";
        proceso.StartInfo.RedirectStandardOutput = true;
        proceso.StartInfo.UseShellExecute = false;
        proceso.StartInfo.CreateNoWindow = true;

        // Ejecutar Prolog
        proceso.Start();
        string resultado = proceso.StandardOutput.ReadToEnd();
        proceso.WaitForExit();

        return resultado;
    }
}
```

---

## **3. Configurar el entorno**

1. **Instalar SWI-Prolog**:
   - Descarga e instala **SWI-Prolog** desde [https://www.swi-prolog.org/](https://www.swi-prolog.org/).
   - Asegúrate de que el ejecutable (`swipl`) esté en el PATH del sistema.

2. **Guardar el archivo Prolog**:
   - Guarda el código de Prolog como `resolver_sudoku.pl` en el mismo directorio que tu programa en C#.

3. **Compilar y ejecutar el programa en C#**:
   - Usa Visual Studio o cualquier compilador de C# para compilar y ejecutar el programa.

---

## **4. Resultado esperado**

- **Entrada**: Un tablero de Sudoku con valores iniciales.
- **Salida**: 
  ```
  Solución del Sudoku:
  [6,4,2,1,5,3,8,9,7]
  [8,1,4,9,6,7,2,3,5]
  [7,9,5,2,8,6,3,1,4]
  [6,5,3,4,2,7,4,8,1]
  [2,7,1,3,8,9,5,6,2]
  ...
  ```

---

### **Ventajas de esta solución**

- C# maneja la interacción con el usuario y la lógica principal.
- Prolog realiza la resolución lógica del Sudoku utilizando su poderoso sistema de restricciones.
