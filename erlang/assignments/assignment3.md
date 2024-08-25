
### Proyecto 3: Calculadora Distribuida en Erlang

#### Objetivo del Proyecto

El objetivo de este proyecto es desarrollar una calculadora distribuida utilizando Erlang, donde los cálculos se realizan en diferentes nodos. Esta implementación demostrará la capacidad de Erlang para manejar la distribución de tareas y la comunicación entre nodos en un sistema distribuido.

#### Descripción del Proyecto

El proyecto consistirá en configurar cuatro nodos de Erlang, cada uno responsable de una operación aritmética básica: suma, resta, multiplicación y división. Los nodos se comunicarán entre sí utilizando el paso de mensajes para realizar los cálculos. Se implementará un cliente que enviará solicitudes de cálculo a los nodos apropiados y recibirá los resultados.

#### Requisitos del Proyecto

1. **Configuración de Nodos**: Se deben configurar cuatro nodos de Erlang:
   - Nodo de Suma: `nodo_suma@nombre_host`
   - Nodo de Resta: `nodo_resta@nombre_host`
   - Nodo de Multiplicación: `nodo_multiplicacion@nombre_host`
   - Nodo de División: `nodo_division@nombre_host`

2. **Comunicación entre Nodos**: Los nodos deben poder comunicarse entre sí. Esto se puede verificar utilizando el comando `net_adm:ping/1` para asegurarse de que los nodos estén conectados.

3. **Implementación de Funciones de Calculadora**: Cada nodo debe implementar una función para realizar su operación específica. Las operaciones deben ser manejadas utilizando un proceso servidor que escuche solicitudes y responda con resultados.

4. **Cliente de Cálculo**: Se debe implementar un cliente que envíe solicitudes de cálculo a los nodos correspondientes y maneje las respuestas.

#### Implementación Detallada

1. **Configuración de los Nodos de Erlang**:
   - Iniciar los nodos con los siguientes comandos en diferentes terminales:
     ```shell
     $ erl -sname nodo_suma
     $ erl -sname nodo_resta
     $ erl -sname nodo_multiplicacion
     $ erl -sname nodo_division
     ```

   - Asegurarse de que los nodos puedan comunicarse entre sí:
     ```erlang
     % En la shell de cada nodo
     net_adm:ping('nodo_suma@nombre_host').
     net_adm:ping('nodo_resta@nombre_host').
     net_adm:ping('nodo_multiplicacion@nombre_host').
     net_adm:ping('nodo_division@nombre_host').
     ```

2. **Definición del Protocolo de Comunicación**:
   - Se utilizará un protocolo simple de paso de mensajes donde el cliente envía una tupla `{operacion, A, B, From}` a un nodo, y el nodo responde con `{resultado, Resultado}`.

3. **Implementación de los Módulos de Nodos**:
   - **Nodo de Suma** (`nodo_suma.erl`):
     ```erlang
     -module(nodo_suma).
     -export([start/0, loop/0]).

     start() ->
         spawn(fun loop/0).

     loop() ->
         receive
             {suma, A, B, From} ->
                 Resultado = A + B,
                 From ! {resultado, Resultado},
                 loop();
             _ ->
                 io:format("Mensaje desconocido~n"),
                 loop()
         end.
     ```

   - **Nodo de Resta** (`nodo_resta.erl`):
     ```erlang
     -module(nodo_resta).
     -export([start/0, loop/0]).

     start() ->
         spawn(fun loop/0).

     loop() ->
         receive
             {resta, A, B, From} ->
                 Resultado = A - B,
                 From ! {resultado, Resultado},
                 loop();
             _ ->
                 io:format("Mensaje desconocido~n"),
                 loop()
         end.
     ```

   - **Nodo de Multiplicación** (`nodo_multiplicacion.erl`):
     ```erlang
     -module(nodo_multiplicacion).
     -export([start/0, loop/0]).

     start() ->
         spawn(fun loop/0).

     loop() ->
         receive
             {multiplicacion, A, B, From} ->
                 Resultado = A * B,
                 From ! {resultado, Resultado},
                 loop();
             _ ->
                 io:format("Mensaje desconocido~n"),
                 loop()
         end.
     ```

   - **Nodo de División** (`nodo_division.erl`):
     ```erlang
     -module(nodo_division).
     -export([start/0, loop/0]).

     start() ->
         spawn(fun loop/0).

     loop() ->
         receive
             {division, A, B, From} ->
                 case B of
                     0 ->
                         From ! {error, "División por cero no permitida"},
                         loop();
                     _ ->
                         Resultado = A / B,
                         From ! {resultado, Resultado},
                         loop()
                 end;
             _ ->
                 io:format("Mensaje desconocido~n"),
                 loop()
         end.
     ```

4. **Implementación del Cliente**:
   - El cliente se conectará a los nodos de cálculo y enviará las solicitudes de cálculo en función de la operación solicitada por el usuario.
   - Ejemplo de implementación del cliente:
     ```erlang
     -module(cliente_calculadora).
     -export([enviar_solicitud/3]).

     enviar_solicitud(Operacion, A, B) ->
         case Operacion of
             suma ->
                 {nodo_suma, 'nodo_suma@nombre_host'};
             resta ->
                 {nodo_resta, 'nodo_resta@nombre_host'};
             multiplicacion ->
                 {nodo_multiplicacion, 'nodo_multiplicacion@nombre_host'};
             division ->
                 {nodo_division, 'nodo_division@nombre_host'}
         end ! {Operacion, A, B, self()},
         recibir_respuesta().

     recibir_respuesta() ->
         receive
             {resultado, Resultado} ->
                 io:format("El resultado es: ~p~n", [Resultado]);
             {error, Motivo} ->
                 io:format("Error: ~p~n", [Motivo])
         end.
     ```

#### Evaluación del Proyecto

- **Funcionalidad**: El sistema debe ser capaz de realizar correctamente las operaciones aritméticas solicitadas por el cliente.
- **Distribución**: Cada operación debe ser manejada por un nodo diferente y los nodos deben poder comunicarse entre sí.
- **Gestión de Errores**: El sistema debe manejar adecuadamente los casos de error, como la división por cero.
- **Código Limpio y Documentado**: El código debe ser claro, modular y bien documentado para facilitar la comprensión y el mantenimiento.

#### Entrega

- El proyecto completo, incluyendo todos los archivos de código fuente (.erl) y una breve documentación de cómo iniciar y probar el sistema, debe ser entregado en un repositorio compartido o plataforma de gestión de código especificada por el instructor.
- La documentación debe incluir instrucciones detalladas para configurar los nodos y ejecutar pruebas para verificar la funcionalidad del sistema.


