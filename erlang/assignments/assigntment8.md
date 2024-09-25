
### Proyecto 8: Chat Distribuido en Erlang

#### Objetivo del Proyecto

El objetivo de este proyecto es extender un sistema de chat simple para que funcione en un entorno distribuido utilizando Erlang. Los usuarios conectados a diferentes nodos podrán comunicarse entre sí, demostrando la capacidad de Erlang para manejar la comunicación en un sistema distribuido. Este proyecto destacará la gestión de la comunicación entre procesos en nodos diferentes y la coordinación para mantener un sistema de chat funcional.

#### Descripción del Proyecto

El proyecto consistirá en extender un sistema de chat ya existente para que funcione en un entorno distribuido. Cada nodo de Erlang representará un servidor de chat al cual los usuarios pueden conectarse. Los mensajes enviados por un usuario en un nodo se propagarán a todos los usuarios conectados, sin importar en qué nodo estén. La aplicación utilizará características de Erlang como la comunicación de procesos entre nodos y registro de nombres global para lograr esta distribución.

#### Requisitos del Proyecto

1. **Configuración de Nodos Distribuidos**: Configurar múltiples nodos de Erlang que actuarán como servidores de chat.
2. **Gestión de Usuarios**: Permitir que los usuarios se conecten a cualquier nodo y se registren en el sistema de chat.
3. **Propagación de Mensajes**: Los mensajes enviados por un usuario deben propagarse a todos los usuarios conectados a cualquier nodo.
4. **Registro de Usuarios**: Utilizar un registro global para manejar los usuarios conectados y su distribución entre nodos.
5. **Tolerancia a Fallos**: El sistema debe manejar adecuadamente la desconexión de usuarios y la caída de nodos.

#### Implementación Detallada

1. **Configuración de Nodos de Erlang**:
   - Para iniciar un nodo de Erlang, se utiliza el siguiente comando en la terminal. Se deben iniciar múltiples nodos para simular un entorno distribuido.

   ```shell
   erl -sname nodo1 -setcookie chatcookie
   erl -sname nodo2 -setcookie chatcookie
   ```

   - Asegúrese de que los nodos puedan comunicarse utilizando el mismo cookie (`chatcookie`) y que estén conectados a través de la red.

2. **Módulo de Servidor de Chat**:
   - Implementar un módulo que gestione la conexión de usuarios y la distribución de mensajes. Este módulo actuará como un servidor en cada nodo.

   ```erlang
   -module(chat_server).
   -export([start_link/0, register_user/2, send_message/2, handle_info/2]).

   -behaviour(gen_server).

   %% API
   start_link() ->
       gen_server:start_link({local, chat_server}, chat_server, [], []).

   %% Callbacks
   init([]) ->
       {ok, #{users => []}}.

   register_user(User, Pid) ->
       gen_server:call(chat_server, {register_user, User, Pid}).

   send_message(User, Message) ->
       gen_server:call(chat_server, {send_message, User, Message}).

   handle_call({register_user, User, Pid}, _From, State) ->
       NewState = maps:update_with(users, fun(Users) -> [{User, Pid} | Users] end, [], State),
       {reply, {ok, "User registered"}, NewState};

   handle_call({send_message, User, Message}, _From, State) ->
       Users = maps:get(users, State, []),
       lists:foreach(fun({_, Pid}) -> Pid ! {message, User, Message} end, Users),
       {reply, {ok, "Message sent"}, State}.
   ```

3. **Módulo de Usuario**:
   - Implementar un módulo para los usuarios, que se conectará al servidor de chat y manejará la recepción de mensajes.

   ```erlang
   -module(chat_user).
   -export([start_link/1, send_message/1, handle_info/2]).

   start_link(User) ->
       Pid = spawn_link(fun() -> init(User) end),
       {ok, Pid}.

   init(User) ->
       %% Register the user with the chat server
       chat_server:register_user(User, self()),
       loop(User).

   loop(User) ->
       receive
           {message, FromUser, Message} ->
               io:format("~p dice: ~s~n", [FromUser, Message]),
               loop(User);
           _ ->
               io:format("Mensaje desconocido recibido~n"),
               loop(User)
       end.

   send_message(Message) ->
       %% Send message to chat server
       chat_server:send_message(self(), Message).
   ```

4. **Propagación de Mensajes Entre Nodos**:
   - Para permitir la propagación de mensajes entre nodos, cada nodo debe conocer a los otros nodos. Esto se puede lograr utilizando `net_adm:ping/1` para establecer la conexión entre los nodos.

   ```erlang
   %% En cada nodo
   net_adm:ping('nodo2@nombre_host').
   ```

   - Los mensajes enviados desde un usuario en un nodo se propagarán a través de los servidores de chat, asegurando que todos los usuarios, sin importar el nodo al que estén conectados, reciban los mensajes.

5. **Gestión de Registro Global**:
   - Utilizar `global:register_name/2` y `global:whereis_name/1` para manejar el registro de usuarios de manera distribuida.

   ```erlang
   global:register_name({chat_server, nodo1}, Pid).
   Pid = global:whereis_name({chat_server, nodo1}).
   ```

#### Evaluación del Proyecto

- **Comunicación Distribuida**: El sistema debe permitir la comunicación fluida entre usuarios conectados a diferentes nodos.
- **Uso de Registro Global**: Demostrar la capacidad de manejar usuarios de manera distribuida utilizando un registro global.
- **Tolerancia a Fallos**: El sistema debe manejar adecuadamente la desconexión de usuarios y caídas de nodos, manteniendo la comunicación para los usuarios restantes.
- **Escalabilidad**: Evaluar cómo el sistema maneja un número creciente de usuarios y nodos.

#### Entrega

- El proyecto completo debe incluir todos los archivos de código fuente (.erl) y una documentación que explique cómo iniciar y utilizar el sistema de chat distribuido.
- Incluir un archivo README que describa los pasos necesarios para ejecutar el sistema y ejemplos de comandos para interactuar con el chat.

# REPOSITORIO

- PRACTICO
https://github.com/PaulScholl/chat_distribuido

- TEORICO


