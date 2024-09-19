# Cómo depurar (debugger) el programa "Hello World" en Erlang en la consola de Linux Raspbian

A continuación, te guiaré a través de los pasos necesarios para depurar un programa "Hello World" en Erlang utilizando la consola de Raspbian en una Raspberry Pi.

## Requisitos previos

- Una Raspberry Pi con Raspbian instalado.
- Acceso a la terminal de comandos.
- Conexión a Internet para instalar Erlang (si aún no está instalado).

## Pasos

### 1. Actualizar el sistema e instalar Erlang

Primero, asegúrate de que tu sistema esté actualizado y luego instala Erlang:

```bash
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install erlang
```

### 2. Crear el programa "Hello World"

Crea un nuevo archivo con extensión `.erl` utilizando un editor de texto como `nano`:

```bash
nano hello.erl
```

Escribe el siguiente código en el archivo:

```erlang
-module(hello).
-export([start/0]).

start() ->
    io:format("Hello, World!~n").
```

Guarda el archivo presionando `Ctrl + O` y luego sal con `Ctrl + X`.

### 3. Compilar el programa

Compila el archivo Erlang desde la terminal:

```bash
erlc hello.erl
```

Si todo está correcto, se generará un archivo `hello.beam`.

### 4. Iniciar la consola de Erlang

Inicia la consola interactiva de Erlang:

```bash
erl
```

### 5. Configurar el depurador

Dentro de la consola de Erlang, inicia el depurador:

```erlang
1> debugger:start().
{ok,<0.33.0>}
```

### 6. Interpretar el módulo para depuración

Para depurar el módulo, necesitas cargarlo como interpretado:

```erlang
2> int:i(hello).
{module,hello}
```

### 7. Establecer puntos de interrupción

Establece un punto de interrupción en la función `start/0`:

```erlang
3> int:break(hello, start, 0).
break
```

### 8. Ejecutar el programa

Ejecuta la función `start/0`:

```erlang
4> hello:start().
```

Al tener un punto de interrupción, la ejecución se detendrá antes de ejecutar el cuerpo de la función.

### 9. Utilizar el depurador

Ahora puedes utilizar los comandos del depurador para inspeccionar y avanzar paso a paso:

- **siguiente paso**: Presiona `n` y luego `Enter` para avanzar al siguiente paso.
- **ver variables**: Utiliza el comando `bindings` para ver las variables en el contexto actual.
- **continuar ejecución**: Presiona `c` y `Enter` para continuar hasta el siguiente punto de interrupción o hasta que termine el programa.

### 10. Salir del depurador y la consola de Erlang

Una vez que hayas terminado, puedes detener el depurador y salir de Erlang:

```erlang
5> debugger:stop().
ok
6> q().
```

También puedes salir presionando `Ctrl + C` dos veces.

## Ejemplo completo

Para mayor claridad, aquí está un ejemplo de toda la sesión:

```bash
pi@raspberrypi:~ $ erl
Erlang/OTP 23 [erts-11.1.8] [source] [64-bit] [smp:4:4] ...

Eshell V11.1.8  (abort with ^G)
1> debugger:start().
{ok,<0.33.0>}
2> int:i(hello).
{module,hello}
3> int:break(hello, start, 0).
break
4> hello:start().
(Breakpoint at hello:start/0)
5> n.
6> bindings().
% Muestra las variables actuales
7> c.
Hello, World!
ok
8> debugger:stop().
ok
9> q().
```

## Importante

Has aprendido cómo depurar un programa simple en Erlang utilizando la consola de Raspbian. Este proceso es útil para comprender el flujo de ejecución y para identificar posibles errores en programas más complejos.

## Recursos adicionales

- [Documentación oficial de Erlang](https://www.erlang.org/docs)
- [Introducción al depurador de Erlang](http://erlang.org/doc/apps/debugger/debugger_chapter.html)
