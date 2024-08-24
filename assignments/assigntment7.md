
### Proyecto 7: Implementación de un Almacén de Datos Clave-Valor con ETS en Erlang

#### Objetivo del Proyecto

El objetivo de este proyecto es implementar un almacén de datos simple de tipo clave-valor utilizando ETS (Erlang Term Storage). Este proyecto permitirá explorar cómo almacenar y recuperar datos de manera eficiente en un entorno concurrente utilizando las capacidades de almacenamiento en memoria de ETS.

#### Descripción del Proyecto

El proyecto consistirá en crear un sistema de almacenamiento de datos clave-valor que utilice ETS para manejar las operaciones de inserción, eliminación, y recuperación de datos. ETS es una poderosa herramienta de Erlang para el almacenamiento de datos en memoria, y es utilizada comúnmente en sistemas donde la velocidad de acceso y la concurrencia son críticas.

#### Requisitos del Proyecto

1. **Creación de un Almacén Clave-Valor**: Implementar un módulo que permita crear un almacén de datos utilizando ETS.
2. **Operaciones CRUD**: El sistema debe soportar las operaciones básicas de Crear, Leer, Actualizar y Eliminar (CRUD) para manejar los datos.
3. **Gestión de Concurrencia**: Aprovechar las características de ETS para gestionar el acceso concurrente a los datos de manera eficiente.
4. **Interfaz de Usuario Simple**: Implementar una interfaz de consola para interactuar con el almacén de datos, permitiendo agregar, actualizar, eliminar y buscar datos.

#### Implementación Detallada

1. **Inicialización del Almacén Clave-Valor**:
   - Crear un módulo que inicie un almacén utilizando ETS. Se utilizará una tabla de tipo `set` para almacenar pares clave-valor únicos.

   ```erlang
   -module(kv_store).
   -export([start_link/0, stop/0, put/2, get/1, delete/1, update/2]).

   -define(TABLE, kv_table).

   start_link() ->
       ets:new(?TABLE, [named_table, set, public, {read_concurrency, true}, {write_concurrency, true}]),
       {ok, "ETS table created"}.

   stop() ->
       ets:delete(?TABLE),
       {ok, "ETS table deleted"}.
   ```

2. **Operaciones CRUD**:
   - Implementar las funciones para manejar las operaciones de creación, lectura, actualización y eliminación de datos en el almacén ETS.

   - **Insertar un par clave-valor**:
     ```erlang
     put(Key, Value) ->
         ets:insert(?TABLE, {Key, Value}),
         {ok, "Value inserted"}.
     ```

   - **Leer un valor basado en la clave**:
     ```erlang
     get(Key) ->
         case ets:lookup(?TABLE, Key) of
             [{_, Value}] -> {ok, Value};
             [] -> {error, "Key not found"}
         end.
     ```

   - **Eliminar un par clave-valor**:
     ```erlang
     delete(Key) ->
         case ets:delete(?TABLE, Key) of
             true -> {ok, "Key deleted"};
             false -> {error, "Key not found"}
         end.
     ```

   - **Actualizar un valor existente**:
     ```erlang
     update(Key, NewValue) ->
         case ets:lookup(?TABLE, Key) of
             [{_, _}] ->
                 ets:insert(?TABLE, {Key, NewValue}),
                 {ok, "Value updated"};
             [] -> {error, "Key not found"}
         end.
     ```

3. **Interfaz de Usuario Simple**:
   - Implementar un módulo CLI para interactuar con el almacén de datos, permitiendo a los usuarios realizar operaciones CRUD.

   ```erlang
   -module(kv_cli).
   -export([start/0]).

   start() ->
       kv_store:start_link(),
       io:format("Bienvenido al almacén de datos Clave-Valor~n"),
       loop().

   loop() ->
       io:format("Seleccione una opción:~n"),
       io:format("1. Insertar clave-valor~n"),
       io:format("2. Leer valor~n"),
       io:format("3. Eliminar clave~n"),
       io:format("4. Actualizar valor~n"),
       io:format("5. Salir~n"),
       Opcion = io:get_line("Opción: "),
       case string:trim(Opcion) of
           "1" ->
               Key = io:get_line("Clave: "),
               Value = io:get_line("Valor: "),
               {ok, Msg} = kv_store:put(string:trim(Key), string:trim(Value)),
               io:format("~p~n", [Msg]),
               loop();
           "2" ->
               Key = io:get_line("Clave: "),
               case kv_store:get(string:trim(Key)) of
                   {ok, Value} -> io:format("Valor: ~p~n", [Value]);
                   {error, Msg} -> io:format("~p~n", [Msg])
               end,
               loop();
           "3" ->
               Key = io:get_line("Clave a eliminar: "),
               {ok, Msg} = kv_store:delete(string:trim(Key)),
               io:format("~p~n", [Msg]),
               loop();
           "4" ->
               Key = io:get_line("Clave: "),
               NewValue = io:get_line("Nuevo valor: "),
               {ok, Msg} = kv_store:update(string:trim(Key), string:trim(NewValue)),
               io:format("~p~n", [Msg]),
               loop();
           "5" ->
               kv_store:stop(),
               io:format("Saliendo...~n"),
               ok;
           _ ->
               io:format("Opción no válida~n"),
               loop()
       end.
   ```

#### Evaluación del Proyecto

- **Funcionalidad**: El almacén de datos debe permitir insertar, leer, actualizar y eliminar pares clave-valor de manera eficiente.
- **Uso de ETS**: Se debe demostrar una comprensión clara de cómo utilizar ETS para gestionar datos en memoria con concurrencia.
- **Gestión de Errores**: El sistema debe manejar adecuadamente los casos en los que se intenta acceder a claves que no existen.
- **Interfaz de Usuario**: La interfaz de usuario debe ser intuitiva y permitir al usuario interactuar fácilmente con el almacén de datos.

#### Entrega

- El proyecto completo debe incluir todos los archivos de código fuente (.erl) y una documentación que explique cómo iniciar y utilizar el sistema de almacenamiento clave-valor.
- Incluir un archivo README que describa los pasos necesarios para ejecutar el sistema y ejemplos de comandos para interactuar con el almacén de datos.

### Conclusión

Este proyecto proporcionando un sistema clave-valor eficiente y concurrente. Se espera que los estudiantes comprendan y apliquen conceptos clave de almacenamiento de datos y concurrencia en aplicaciones prácticas utilizando Erlang.
