# ERLANG, como lenguaje funcional

| Concepto                   | Sintaxis/Ejemplo                                 | Descripción                                                        |
|----------------------------|--------------------------------------------------|--------------------------------------------------------------------|
| **Comentarios**            | `% Esto es un comentario`                        | Los comentarios en Erlang comienzan con el símbolo `%`.            |
| **Variables**              | `Variable = Valor.`                              | Las variables comienzan con mayúscula y son inmutables.            |
| **Enteros**                | `Int = 42.`                                       | Definición de un entero.                                           |
| **Flotantes**              | `Float = 3.14.`                                   | Definición de un número flotante.                                  |
| **Átomos**                 | `Atom = ejemplo.`                                 | Los átomos son literales constantes. Inician con letra minúscula.  |
| **Tuplas**                 | `Tupla = {valor1, valor2, valor3}.`               | Estructuras de datos fijas.                                        |
| **Listas**                 | `Lista = [1, 2, 3, 4].`                           | Estructura de datos dinámica.                                      |
| **Concatenar Listas**      | `[1, 2] ++ [3, 4].`                               | Uso del operador `++` para concatenar listas.                      |
| **Patrón de coincidencia** | `{X, Y} = {1, 2}.`                                | Asignación por coincidencia de patrón.                             |
| **Comprobación de tipo**   | `is_atom(X).`                                     | Verifica si `X` es un átomo.                                       |
| **If-Else**                | `if Condición1 -> Acción1; Condición2 -> Acción2 end.` | Estructura condicional.                                          |
| **Case**                   | `case Expresión of patrón1 -> Acción1; _ -> Acción2 end.` | Estructura de selección basada en patrones.                |
| **Funciones**              | `nombre_función(Arg1, Arg2) -> expresión.`        | Definición de funciones con cláusulas.                             |
| **Llamada a función**      | `Modulo:Funcion(Argumentos).`                     | Llamada a una función en un módulo específico.                     |
| **Spawning Process**       | `spawn(Modulo, Función, [Argumentos]).`           | Creación de un nuevo proceso.                                      |
| **Mensaje de envío**       | `Pid ! Mensaje.`                                  | Envía un mensaje a un proceso identificado por `Pid`.              |
| **Recibir mensaje**        | `receive Mensaje -> Acción end.`                  | Recepción de mensajes dentro de un proceso.                        |
| **Definición de Módulo**   | `-module(nombre_modulo).`                         | Define un módulo.                                                  |
| **Exportar Funciones**     | `-export([función/Aridad]).`                      | Exporta funciones para que puedan ser llamadas desde otros módulos.|
| **Cargar módulo**          | `c(módulo).`                                      | Compila y carga un módulo en la shell de Erlang.                   |
| **Salir de la shell**      | `q().`                                            | Comando para salir de la shell de Erlang.                          |
| **Encabezados de función** | `nombre_función(X) when is_integer(X) -> Acción.` | Restricciones de tipo en la declaración de funciones.              |
| **Try-Catch**              | `try Expresión of ... catch Tipo:Error -> Acción end.` | Manejo de excepciones.                                      |


---

# Cheat Sheet de Erlang: Manejo de Binarios

## Creación de Binarios

| **Expresión**                  | **Descripción**                         |
|--------------------------------|-----------------------------------------|
| `<<>>`                         | Binario vacío                           |
| `<<1,2,3>>`                    | Binario de tres bytes                   |
| `<<1:4, 15:4>>`                | Binario de un byte con dos mitades      |
| `<<1,2,3>> ++ <<4,5,6>>`       | Concatenación de binarios               |

## Extraer Binarios

| **Expresión**              | **Descripción**                                     |
|----------------------------|-----------------------------------------------------|
| `<<X:8, Y:8, Z:8>> = <<1,2,3>>` | Extrae tres bytes en X, Y, Z                       |
| `<<A, Rest/binary>> = <<1,2,3>>` | Extrae el primer byte en A, resto en `Rest`         |
| `<<N:16>> = <<1,2>>`          | Extrae 2 bytes y los convierte en un entero N       |

## Tamaños y Tipos de Binarios

| **Expresión**                       | **Descripción**                                         |
|-------------------------------------|---------------------------------------------------------|
| `<<1:8/unit:1>>`                    | Binario de un byte                                      |
| `<<1024:16/unit:8>>`                | 1024 bytes (1 KB)                                       |
| `<<N:8/unit:8>>`                    | N bytes (definidos por un entero N)                     |
| `<<X:4, Y:4>> = <<15>>`             | X = 0, Y = 15 (división de byte en dos mitades)         |
| `<<12345:32/float>>`                | Convierte el número en binario de 32 bits de coma flotante |

## Comparaciones de Binarios

| **Expresión**         | **Descripción**                                   |
|-----------------------|---------------------------------------------------|
| `<<1,2,3>> == <<1,2,3>>`  | Compara dos binarios, retorna `true` si son iguales |
| `<<1,2,3>> /= <<3,2,1>>`  | Compara dos binarios, retorna `true` si son diferentes |

## Conversión de Binarios

| **Expresión**              | **Descripción**                                 |
|----------------------------|-------------------------------------------------|
| `binary_to_list(<<1,2,3>>)` | Convierte un binario a una lista de enteros     |
| `list_to_binary([1,2,3])`   | Convierte una lista de enteros a un binario     |
| `integer_to_binary(123)`    | Convierte un entero a su representación binaria |
| `binary_to_integer(<<123>>)`| Convierte un binario a un entero                |

## Ejemplos de Uso de Binarios

| **Expresión**                          | **Descripción**                                   |
|----------------------------------------|---------------------------------------------------|
| `<<X:8, Rest/binary>> = <<65, 66, 67>>`| X = 65, Rest = <<66, 67>>                         |
| `<<16#A:4, 16#B:4>> = <<16#AB>>`       | Descompone el byte hexadecimal `AB` en A = 10, B = 11 |

Este cheat sheet proporciona una guía rápida sobre cómo crear, manipular y trabajar con binarios en Erlang, así como realizar conversiones y comparaciones básicas. Es útil para programadores que necesiten trabajar con estructuras de datos binarios y optimización de datos en Erlang.

---

