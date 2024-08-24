
### Proyecto 4: Aplicación de Gestión de Tareas con GenServer en Erlang

#### Objetivo del Proyecto

El objetivo de este proyecto es desarrollar una aplicación de gestión de tareas utilizando Erlang. La aplicación permitirá agregar, eliminar y listar tareas utilizando un proceso GenServer para manejar el estado de las tareas de manera concurrente y robusta.

#### Descripción del Proyecto

Este proyecto implicará la creación de una aplicación simple de lista de tareas (Todo List) donde las tareas pueden ser agregadas, eliminadas y listadas. La aplicación utilizará un módulo GenServer para gestionar el estado de las tareas, demostrando cómo manejar el estado y la comunicación en una aplicación concurrente en Erlang.

#### Requisitos del Proyecto

1. **Implementación de un GenServer**: Se debe implementar un módulo GenServer que maneje las operaciones de agregar, eliminar y listar tareas.
2. **Funciones para Operaciones de Tareas**:
   - **Agregar Tarea**: Función para agregar una nueva tarea a la lista.
   - **Eliminar Tarea**: Función para eliminar una tarea específica por su identificador.
   - **Listar Tareas**: Función para listar todas las tareas actuales.
3. **Persistencia del Estado**: El estado de las tareas debe mantenerse a través de las operaciones utilizando las características del GenServer.
4. **Interfaz de Usuario Simple**: Implementar una interfaz de consola simple para interactuar con la lista de tareas, permitiendo a los usuarios agregar, eliminar y listar tareas.

#### Implementación Detallada

1. **Creación del Módulo GenServer**:
   - Crear un módulo en Erlang que utilice GenServer para manejar el estado de la lista de tareas.
   - El estado se representará como una lista de tareas, donde cada tarea es una tupla `{Id, Descripcion}`.

   ```erlang
   -module(todo_server).
   -behaviour(gen_server).

   %% API
   -export([start_link/0, add_task/1, remove_task/1, list_tasks/0]).

   %% Callbacks
   -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

   %% Estado inicial
   start_link() ->
       gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

   init([]) ->
       {ok, []}.
   ```

2. **Funciones de Operación de Tareas**:
   - **Agregar Tarea**: Esta función permitirá agregar una nueva tarea al estado.
     ```erlang
     add_task(Descripcion) ->
         gen_server:call(?MODULE, {add_task, Descripcion}).

     handle_call({add_task, Descripcion}, _From, Estado) ->
         Id = erlang:unique_integer([positive]),
         NuevaTarea = {Id, Descripcion},
         NuevoEstado = [NuevaTarea | Estado],
         {reply, {ok, Id}, NuevoEstado}.
     ```

   - **Eliminar Tarea**: Esta función eliminará una tarea específica usando su identificador.
     ```erlang
     remove_task(Id) ->
         gen_server:call(?MODULE, {remove_task, Id}).

     handle_call({remove_task, Id}, _From, Estado) ->
         NuevoEstado = lists:filter(fun({TareaId, _}) -> TareaId =/= Id end, Estado),
         {reply, ok, NuevoEstado}.
     ```

   - **Listar Tareas**: Esta función devolverá la lista de todas las tareas actuales.
     ```erlang
     list_tasks() ->
         gen_server:call(?MODULE, list_tasks).

     handle_call(list_tasks, _From, Estado) ->
         {reply, Estado, Estado}.
     ```

3. **Manejo de Estados y Respuestas**:
   - El GenServer debe manejar los estados de manera adecuada y responder a las llamadas de manera síncrona utilizando `handle_call/3`.
   - Las funciones `handle_cast/2` y `handle_info/2` se utilizarán para manejar mensajes asincrónicos y otros tipos de mensajes, si es necesario.

4. **Interfaz de Usuario Simple**:
   - Implementar una función de interfaz en consola para interactuar con la aplicación.
     ```erlang
     -module(todo_cli).
     -export([start/0]).

     start() ->
         io:format("Bienvenido a la aplicación de gestión de tareas~n"),
         loop().

     loop() ->
         io:format("Seleccione una opción:~n"),
         io:format("1. Agregar tarea~n"),
         io:format("2. Eliminar tarea~n"),
         io:format("3. Listar tareas~n"),
         io:format("4. Salir~n"),
         Opcion = io:get_line("Opción: "),
         case string:trim(Opcion) of
             "1" ->
                 Descripcion = io:get_line("Descripción de la tarea: "),
                 {ok, Id} = todo_server:add_task(Descripcion),
                 io:format("Tarea agregada con ID: ~p~n", [Id]),
                 loop();
             "2" ->
                 Id = io:get_line("ID de la tarea a eliminar: "),
                 case todo_server:remove_task(list_to_integer(string:trim(Id))) of
                     ok -> io:format("Tarea eliminada~n");
                     _ -> io:format("Tarea no encontrada~n")
                 end,
                 loop();
             "3" ->
                 Tareas = todo_server:list_tasks(),
                 io:format("Lista de tareas: ~p~n", [Tareas]),
                 loop();
             "4" ->
                 io:format("Saliendo...~n"),
                 ok;
             _ ->
                 io:format("Opción no válida~n"),
                 loop()
         end.
     ```

#### Evaluación del Proyecto

- **Funcionalidad**: La aplicación debe permitir agregar, eliminar y listar tareas de manera eficiente.
- **Uso de GenServer**: El manejo del estado de las tareas debe realizarse utilizando GenServer, demostrando comprensión de su ciclo de vida y manejo de estado.
- **Interfaz de Usuario**: La aplicación debe tener una interfaz de usuario simple pero funcional que permita la interacción básica con el sistema de tareas.
- **Manejo de Errores**: El sistema debe manejar adecuadamente los errores, como intentar eliminar una tarea inexistente.

#### Entrega

- El proyecto completo debe incluir todos los archivos de código fuente (.erl) y una documentación que explique cómo iniciar y utilizar la aplicación.
- Incluir un archivo README que describa los pasos necesarios para ejecutar la aplicación y ejemplos de comandos que un usuario puede utilizar para interactuar con la lista de tareas.

### Resultados

Este proyecto demostrará la capacidad de Erlang para manejar estados utilizando GenServer, así como la implementación de un sistema concurrente robusto para la gestión de tareas. Se espera que los estudiantes apliquen conceptos clave de manejo de estado y concurrencia en aplicaciones prácticas utilizando Erlang.
```
