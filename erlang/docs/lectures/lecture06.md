# Cómo Depurar un Programa en Erlang desde la Consola de Linux

La depuración de programas en Erlang puede realizarse directamente desde la consola de Linux utilizando las herramientas y módulos integrados en el lenguaje. A continuación, se detallan los pasos y técnicas para depurar un programa Erlang en el shell de Linux.

---

## 1. Compilar el Código con Información de Depuración

Antes de comenzar a depurar, es esencial compilar el código con información de depuración. Esto permite que las herramientas de depuración puedan acceder a detalles como nombres de variables y líneas de código.

### Compilación desde la Consola de Linux

Utilice el compilador de Erlang `erlc` con la opción `+debug_info`:

```bash
erlc +debug_info mi_modulo.erl
```

### Compilación desde el Shell de Erlang

Dentro del shell de Erlang, puede compilar con información de depuración así:

```erlang
c(mi_modulo, [debug_info]).
```

---

## 2. Utilizar el Módulo `dbg` para Depuración

El módulo `dbg` es una herramienta poderosa para depurar código desde la consola. Permite trazar llamadas a funciones, monitorear mensajes entre procesos y establecer condiciones específicas para el trazado.

### Pasos para Utilizar `dbg`

1. **Iniciar el Trazador**

   ```erlang
   dbg:tracer().
   ```

2. **Configurar el Trazado para Procesos**

   Por ejemplo, para trazar todas las llamadas de todos los procesos:

   ```erlang
   dbg:p(all, c).
   ```

3. **Establecer Patrones de Trazado**

   Para trazar todas las funciones de `mi_modulo`:

   ```erlang
   dbg:tpl(mi_modulo, '_', []).
   ```

   O para una función específica:

   ```erlang
   dbg:tpl(mi_modulo, funcion_especifica, []).
   ```

4. **Ejecutar el Código a Depurar**

   Llame a la función que desea depurar:

   ```erlang
   mi_modulo:funcion_especifica(Argumentos).
   ```

5. **Observar la Salida**

   La consola mostrará las llamadas a las funciones, incluyendo argumentos y valores de retorno.

6. **Detener y Limpiar el Trazado**

   ```erlang
   dbg:stop_clear().
   ```

### Ejemplo Práctico

Supongamos que tiene una función `sumar/2` en `mi_modulo`:

```erlang
% mi_modulo.erl
-module(mi_modulo).
-export([sumar/2]).

sumar(A, B) ->
    Resultado = A + B,
    Resultado.
```

**Pasos para depurar `sumar/2`:**

1. **Compilar con Información de Depuración**

   ```erlang
   c(mi_modulo, [debug_info]).
   ```

2. **Iniciar el Trazador y Configurar Trazado**

   ```erlang
   dbg:tracer().
   dbg:p(all, c).
   dbg:tpl(mi_modulo, sumar, []).
   ```

3. **Ejecutar la Función**

   ```erlang
   mi_modulo:sumar(5, 7).
   ```

4. **Observar la Salida**

   La consola mostrará:

   ```
   (<0.80.0>) call mi_modulo:sumar(5,7)
   (<0.80.0>) returned from mi_modulo:sumar/2 -> 12
   ```

5. **Detener el Trazado**

   ```erlang
   dbg:stop_clear().
   ```

---

## 3. Utilizar `io:format/2` para Mensajes de Depuración

Insertar mensajes de depuración en el código puede ayudar a entender el flujo y estado interno del programa.

### Ejemplo

```erlang
sumar(A, B) ->
    io:format("A = ~p, B = ~p~n", [A, B]),
    Resultado = A + B,
    io:format("Resultado = ~p~n", [Resultado]),
    Resultado.
```

---

## 4. Obtener Rastros de Pila (Stack Traces)

Cuando ocurre una excepción, Erlang proporciona un rastro de pila que puede ayudar a identificar el origen del error.

### Capturar Excepciones

```erlang
try mi_modulo:funcion_problematica() of
    Resultado -> Resultado
catch
    Tipo:Error ->
        io:format("Error de tipo ~p: ~p~n", [Tipo, Error]),
        io:format("Stack trace: ~p~n", [erlang:get_stacktrace()])
end.
```

---

## 5. Utilizar el Módulo `runtime_tools` para Perfilado

El módulo `runtime_tools` permite inspeccionar procesos y obtener información en tiempo de ejecución.

### Ejemplo

```erlang
% Listar todos los procesos con su información
processes() ->
    [erlang:process_info(P) || P <- processes()].
```

---

## 6. Usar el Depurador en Modo Consola

Aunque el depurador gráfico es más intuitivo, también puede depurar en modo consola.

### Pasos

1. **Cargar el Módulo para Depuración**

   ```erlang
   im(mi_modulo).
   ```

2. **Establecer Puntos de Interrupción**

   ```erlang
   ib(mi_modulo, Linea).
   ```

3. **Ejecutar la Función y Controlar la Ejecución**

   ```erlang
   mi_modulo:funcion_a_depurar().
   ```

   Use comandos como:

   - `c.` para continuar
   - `s.` para siguiente paso
   - `bt.` para backtrace

---

## 7. Consejos Adicionales

- **Utilizar Pruebas Unitarias con `eunit`**

  Las pruebas unitarias ayudan a detectar errores antes de ejecutar el programa completo.

  ```erlang
  % mi_modulo_tests.erl
  -module(mi_modulo_tests).
  -include_lib("eunit/include/eunit.hrl").

  sumar_test() ->
      ?assertEqual(5, mi_modulo:sumar(2, 3)).
  ```

  Ejecutar las pruebas:

  ```erlang
  eunit:test(mi_modulo).
  ```

- **Documentar el Código**

  Mantener una buena documentación facilita la depuración y mantenimiento.

---

**Recursos Adicionales**

- **Documentación Oficial de Erlang**: [erlang.org/doc](https://www.erlang.org/doc)
- **Manual del Módulo `dbg`**: [dbg Documentation](https://www.erlang.org/doc/man/dbg.html)
- **Tutoriales de Depuración**: Busque guías y tutoriales en línea para profundizar en técnicas avanzadas.
