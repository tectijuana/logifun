
### Proyecto 5: Implementación de un Balanceador de Carga con Estrategia Round-Robin en Erlang

#### Objetivo del Proyecto

El objetivo de este proyecto es implementar un balanceador de carga utilizando Erlang. El balanceador de carga distribuirá tareas entre múltiples procesos trabajadores siguiendo la estrategia round-robin. Esta implementación demostrará cómo gestionar la distribución de tareas de manera equitativa y eficiente entre múltiples procesos concurrentes.

#### Descripción del Proyecto

El proyecto consistirá en crear un sistema de balanceo de carga donde un proceso central (balanceador) distribuirá las tareas entre varios procesos trabajadores (workers) utilizando la estrategia round-robin. El balanceador de carga recibirá tareas y las asignará secuencialmente a los trabajadores, asegurando que cada trabajador reciba una cantidad equitativa de trabajo.

#### Requisitos del Proyecto

1. **Creación del Balanceador de Carga**: Implementar un proceso que actúe como balanceador de carga, distribuyendo tareas a los trabajadores utilizando la estrategia round-robin.
2. **Creación de Procesos Trabajadores**: Implementar varios procesos trabajadores que realizarán las tareas asignadas por el balanceador de carga.
3. **Gestión de Tareas**: Las tareas serán mensajes simples que los trabajadores procesarán y responderán al balanceador.
4. **Estrategia Round-Robin**: El balanceador debe distribuir las tareas de manera secuencial y cíclica entre los trabajadores.

#### Implementación Detallada

1. **Definición del Módulo de Balanceador de Carga**:
   - El balanceador de carga será un módulo que gestiona una lista de trabajadores y distribuye las tareas de manera secuencial.

   ```erlang
   -module(load_balancer).
   -behaviour(gen_server).

   %% API
   -export([start_link/1, add_task/1]).

   %% Callbacks
   -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

   %% Inicia el balanceador con una lista de procesos trabajadores
   start_link(Workers) ->
       gen_server:start_link({local, ?MODULE}, ?MODULE, Workers, []).

   init(Workers) ->
       {ok, {Workers, 0}}.
   ```

2. **Manejo de Tareas en el Balanceador de Carga**:
   - El balanceador recibirá tareas y las distribuirá entre los trabajadores usando un índice para rastrear a qué trabajador asignar la siguiente tarea.

   ```erlang
   add_task(Task) ->
       gen_server:cast(?MODULE, {add_task, Task}).

   handle_cast({add_task, Task}, {Workers, Index}) ->
       Worker = lists:nth(Index + 1, Workers),
       Worker ! {process_task, Task},
       NextIndex = (Index + 1) rem length(Workers),
       {noreply, {Workers, NextIndex}}.
   ```

3. **Implementación de los Trabajadores (Workers)**:
   - Los trabajadores serán procesos simples que procesan las tareas que reciben del balanceador de carga y envían una respuesta.

   ```erlang
   -module(worker).
   -export([start/0, loop/0]).

   start() ->
       spawn(fun loop/0).

   loop() ->
       receive
           {process_task, Task} ->
               io:format("Procesando tarea: ~p~n", [Task]),
               loop();
           _ ->
               io:format("Mensaje desconocido recibido~n"),
               loop()
       end.
   ```

4. **Inicialización del Sistema de Balanceo de Carga**:
   - Crear un script o una función para iniciar el balanceador de carga y los trabajadores, y agregar tareas para ser distribuidas.

   ```erlang
   -module(startup).
   -export([start_system/0]).

   start_system() ->
       Worker1 = worker:start(),
       Worker2 = worker:start(),
       Worker3 = worker:start(),
       Workers = [Worker1, Worker2, Worker3],
       load_balancer:start_link(Workers),

       %% Agregar tareas al balanceador
       load_balancer:add_task(task1),
       load_balancer:add_task(task2),
       load_balancer:add_task(task3),
       load_balancer:add_task(task4),
       load_balancer:add_task(task5).
   ```

#### Evaluación del Proyecto

- **Funcionalidad**: El balanceador de carga debe distribuir las tareas de manera equitativa entre los trabajadores utilizando la estrategia round-robin.
- **Uso de GenServer**: El balanceador de carga debe manejar el estado de los trabajadores y el índice de asignación de tareas utilizando GenServer.
- **Robustez**: El sistema debe manejar situaciones donde los trabajadores no están disponibles o fallan al procesar una tarea.
- **Eficiencia**: El balanceador de carga debe distribuir las tareas de manera eficiente para maximizar la utilización de los trabajadores.

#### Entrega

- El proyecto completo debe incluir todos los archivos de código fuente (.erl) y una documentación que explique cómo iniciar y utilizar el sistema de balanceo de carga.
- Incluir un archivo README que describa los pasos necesarios para ejecutar el balanceador de carga y ejemplos de cómo interactuar con el sistema.

### Resultados

Este proyecto demostrará la capacidad de Erlang para manejar la distribución de tareas de manera equitativa y eficiente utilizando procesos concurrentes. Se espera que los estudiantes comprendan y apliquen conceptos clave de manejo de tareas y balanceo de carga en aplicaciones concurrentes utilizando Erlang.
