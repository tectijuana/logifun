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
