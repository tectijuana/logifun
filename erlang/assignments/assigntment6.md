
### Proyecto 6: Sistema Tolerante a Fallos con Árboles de Supervisión en Erlang

#### Objetivo del Proyecto

El objetivo de este proyecto es desarrollar un sistema tolerante a fallos utilizando Erlang. Implementaremos un sistema de supervisión que detecte fallos en procesos y los reinicie automáticamente. Para lograr esto, utilizaremos árboles de supervisión (supervision trees), que son una característica clave en Erlang para mejorar la tolerancia a fallos y la robustez del sistema.

#### Descripción del Proyecto

El proyecto consistirá en crear un sistema de varios procesos donde cada proceso realiza una tarea específica. Se implementará un supervisor que monitorice estos procesos. Si un proceso falla, el supervisor lo detectará y lo reiniciará según las estrategias definidas. Este enfoque es fundamental para construir sistemas confiables y tolerantes a fallos, capaces de seguir operando incluso cuando ocurren errores inesperados.

#### Requisitos del Proyecto

1. **Implementación de Procesos Trabajadores**: Crear varios procesos que simulen tareas de negocio y que puedan fallar de manera controlada para probar el sistema de supervisión.
2. **Implementación de un Supervisor**: Implementar un módulo supervisor que vigile a los procesos trabajadores y los reinicie en caso de fallo.
3. **Uso de Estrategias de Reinicio**: Definir estrategias de reinicio adecuadas (one_for_one, one_for_all, rest_for_one) para gestionar cómo se deben reiniciar los procesos en caso de fallo.
4. **Simulación de Fallos**: Introducir fallos controlados en los procesos trabajadores para probar la efectividad del sistema de supervisión.

#### Implementación Detallada

1. **Definición de los Procesos Trabajadores**:
   - Los procesos trabajadores simularán tareas de negocio simples y se diseñarán para fallar en ciertos casos, permitiendo probar el sistema de supervisión.

   ```erlang
   -module(worker).
   -export([start_link/0, perform_task/0]).

   start_link() ->
       spawn_link(fun perform_task/0).

   perform_task() ->
       receive
           {do_work, N} when N rem 5 == 0 -> 
               %% Simula un fallo en ciertos casos
               exit(crash);
           {do_work, N} ->
               io:format("Worker procesando tarea: ~p~n", [N]),
               perform_task()
       end.
   ```

2. **Implementación del Supervisor**:
   - El supervisor supervisará a los procesos trabajadores y aplicará estrategias de reinicio en caso de fallo.

   ```erlang
   -module(supervisor).
   -behaviour(supervisor).

   %% API
   -export([start_link/0, init/1]).

   start_link() ->
       supervisor:start_link({local, ?MODULE}, ?MODULE, []).

   init([]) ->
       Workers = [
           {worker1, {worker, start_link, []}, permanent, brutal_kill, worker, [worker]},
           {worker2, {worker, start_link, []}, permanent, brutal_kill, worker, [worker]}
       ],
       {ok, {{one_for_one, 5, 10}, Workers}}.
   ```

   En este ejemplo, el supervisor está configurado con una estrategia `one_for_one`, lo que significa que si un proceso trabajador falla, solo ese proceso será reiniciado. También se definen límites de intensidad (`5` reinicios en `10` segundos) para evitar ciclos de reinicio infinito.

3. **Configuración del Árbol de Supervisión**:
   - El árbol de supervisión se define en la función `init/1` del supervisor. Se especifican los procesos hijos y sus configuraciones.

   ```erlang
   init([]) ->
       Workers = [
           {worker1, {worker, start_link, []}, permanent, brutal_kill, worker, [worker]},
           {worker2, {worker, start_link, []}, permanent, brutal_kill, worker, [worker]},
           {worker3, {worker, start_link, []}, permanent, brutal_kill, worker, [worker]}
       ],
       {ok, {{one_for_all, 3, 10}, Workers}}.
   ```

   Aquí, se utiliza una estrategia `one_for_all` para mostrar cómo, si un trabajador falla, todos los trabajadores serán reiniciados.

4. **Simulación de Fallos**:
   - Para probar el sistema de supervisión, se pueden enviar mensajes a los procesos trabajadores que generen un fallo intencionado.

   ```erlang
   %% Enviar tareas a los trabajadores
   Pid1 = whereis(worker1),
   Pid2 = whereis(worker2),
   
   Pid1 ! {do_work, 1},
   Pid2 ! {do_work, 5},  %% Este debería provocar un fallo y reiniciar
   ```

#### Evaluación del Proyecto

- **Tolerancia a Fallos**: El sistema debe ser capaz de detectar fallos en los procesos trabajadores y aplicar las estrategias de reinicio adecuadas.
- **Implementación de Árboles de Supervisión**: Se debe demostrar una comprensión clara de cómo configurar y utilizar árboles de supervisión para manejar la tolerancia a fallos.
- **Gestión de Errores**: El sistema debe manejar adecuadamente los errores sin entrar en ciclos de reinicio infinitos.
- **Pruebas**: Se deben realizar pruebas para demostrar que el sistema se comporta correctamente en presencia de fallos.

#### Entrega

- El proyecto completo debe incluir todos los archivos de código fuente (.erl) y una documentación que explique cómo iniciar y probar el sistema de supervisión.
- Incluir un archivo README que describa los pasos necesarios para ejecutar el sistema y ejemplos de cómo introducir fallos y observar el comportamiento del sistema de supervisión.

### Resultados

Este proyectoespera que los estudiantes comprendan y apliquen conceptos clave de tolerancia a fallos y supervisión de procesos en aplicaciones concurrentes utilizando Erlang.
