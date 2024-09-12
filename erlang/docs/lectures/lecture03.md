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
# Makefile para automatización de corrida de programas.

Un **Makefile** es un archivo de texto que contiene un conjunto de reglas e instrucciones para el programa **`make`**, una herramienta de automatización de compilación. Los Makefiles se utilizan para simplificar y automatizar el proceso de compilación y construcción de programas, especialmente en proyectos que involucran múltiples archivos fuente o etapas de compilación complejas.

---

## ¿Qué es `make`?

**`make`** es una utilidad de Unix que analiza el Makefile y ejecuta comandos definidos en él para construir y gestionar proyectos de software. Utiliza las dependencias y las reglas especificadas para determinar qué partes del programa necesitan ser recompiladas y qué comandos deben ejecutarse para hacerlo.

---

## Beneficios de Usar un Makefile

- **Automatización**: Evita la necesidad de compilar manualmente cada archivo o ejecutar comandos repetitivos.
- **Eficiencia**: `make` recompila sólo los archivos que han cambiado, ahorrando tiempo.
- **Organización**: Centraliza las instrucciones de compilación y ejecución en un solo lugar.
- **Portabilidad**: Facilita la construcción del proyecto en diferentes entornos sin modificar los comandos manualmente.

---

## Componentes de un Makefile

Un Makefile está compuesto por reglas que incluyen:

1. **Objetivos (Targets)**: El nombre de la tarea que se quiere lograr, como compilar un archivo o ejecutar un programa.
2. **Dependencias**: Archivos o recursos que deben existir o actualizarse antes de ejecutar el objetivo.
3. **Comandos**: Las acciones que se ejecutarán para cumplir el objetivo.

**Ejemplo de una regla simple:**

```makefile
programa1.beam: programa1.erl
    erlc programa1.erl
```

- **Objetivo**: `programa1.beam`
- **Dependencia**: `programa1.erl`
- **Comando**: `erlc programa1.erl`

---

## Cómo Funciona el Makefile en el Contexto de Erlang

En el ejemplo anterior, el Makefile define cómo compilar y ejecutar dos programas Erlang (`programa1` y `programa2`). Esto automatiza las siguientes tareas:

- **Compilación**: Convierte los archivos fuente `.erl` en archivos bytecode `.beam` utilizables por la máquina virtual de Erlang.
- **Ejecución**: Ejecuta los programas sin necesidad de introducir manualmente los comandos en la consola de Erlang.
- **Limpieza**: Elimina los archivos compilados para mantener el directorio limpio.

**Uso del Makefile:**

- **Compilar todos los programas**:

  ```bash
  make
  ```

- **Ejecutar un programa específico**:

  ```bash
  make ejecutar_programa1
  ```

- **Limpiar los archivos compilados**:

  ```bash
  make limpiar
  ```

---

## Ventajas de Usar un Makefile en Proyectos Erlang

- **Simplifica el Flujo de Trabajo**: No es necesario recordar todos los comandos de compilación y ejecución.
- **Reduce Errores**: Al automatizar comandos, se minimiza el riesgo de cometer errores manuales.
- **Facilita la Colaboración**: Otros desarrolladores pueden entender y reproducir el proceso de compilación fácilmente.
- **Escalabilidad**: A medida que el proyecto crece, el Makefile puede actualizarse para manejar nuevas dependencias y tareas.

---

## Creación y Personalización de un Makefile

Puedes crear un Makefile con cualquier editor de texto y guardarlo con el nombre `Makefile` (sin extensión). Asegúrate de respetar la indentación (utiliza tabulaciones, no espacios) y la sintaxis correcta.

**Ejemplo básico:**

```makefile
all: programa1.beam programa2.beam

programa1.beam: programa1.erl
    erlc programa1.erl

programa2.beam: programa2.erl
    erlc programa2.erl

ejecutar_programa1:
    erl -noshell -s programa1 start -s init stop

ejecutar_programa2:
    erl -noshell -s programa2 start -s init stop

limpiar:
    rm -f *.beam
```
