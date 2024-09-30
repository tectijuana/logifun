# Project 2: Chat Server

En este proyecto, los estudiantes implementarán un servidor de chat simple utilizando Erlang. Este servidor permitirá que múltiples clientes se conecten y envíen mensajes entre ellos. El proyecto está diseñado para enseñar conceptos fundamentales de concurrencia, manejo de procesos y comunicación entre procesos en Erlang.

## Objetivos del Proyecto

- Aprender a crear y manejar procesos en Erlang.
- Implementar la comunicación entre procesos utilizando el paso de mensajes.
- Construir un sistema de supervisión básico para manejar errores y reiniciar procesos fallidos.
- Entender cómo gestionar múltiples conexiones y la sincronización de mensajes en un sistema concurrente.

## Estructura del Proyecto

El proyecto se estructurará en módulos que abordan diferentes componentes del servidor de chat. A continuación, se detallan los pasos modulares para implementar cada parte del servidor de chat.

### Módulo 1: Servidor de Chat (chat_server)

**Objetivo**: Crear el proceso principal del servidor que aceptará conexiones de clientes y manejará la distribución de mensajes entre ellos.

1. **Definir el Módulo y Funciones Exportadas**:
    - Crea un módulo llamado `chat_server`.
    - Exporta las funciones `start/0`, `accept/1`, y `handle_message/1`.

    ```erlang
    -module(chat_server).
    -export([start/0, accept/1, handle_message/1]).
    ```

2. **Función `start/0`**:
    - Esta función inicia el servidor, crea un socket TCP y escucha en un puerto específico (por ejemplo, 1234).

    ```erlang
    start() ->
        {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
        io:format("Chat server started on port 1234~n"),
        accept(ListenSocket).
    ```

3. **Función `accept/1`**:
    - Acepta nuevas conexiones y genera un nuevo proceso para manejar cada conexión. Luego, vuelve a llamarse a sí mismo para seguir aceptando más conexiones.

    ```erlang
    accept(ListenSocket) ->
        {ok, Socket} = gen_tcp:accept(ListenSocket),
        spawn(fun() -> handle_message(Socket) end),
        accept(ListenSocket).
    ```

4. **Función `handle_message/1`**:
    - Maneja la recepción de mensajes de un cliente conectado. Los mensajes recibidos son enviados a todos los clientes conectados.

    ```erlang
    handle_message(Socket) ->
        case gen_tcp:recv(Socket, 0) of
            {ok, Data} ->
                io:format("Received message: ~p~n", [Data]),
                broadcast(Data),
                handle_message(Socket);
            {error, closed} ->
                io:format("Client disconnected~n"),
                ok
        end.
    ```

### Módulo 2: Manejo de Clientes Conectados (chat_clients)

**Objetivo**: Gestionar la lista de clientes conectados y permitir la difusión de mensajes a todos los clientes.

1. **Definir el Módulo y Funciones Exportadas**:
    - Crea un módulo llamado `chat_clients`.
    - Exporta las funciones `add_client/1`, `remove_client/1`, `get_clients/0`, y `broadcast/1`.

    ```erlang
    -module(chat_clients).
    -export([add_client/1, remove_client/1, get_clients/0, broadcast/1]).
    ```

2. **Función `add_client/1`**:
    - Añade un cliente a la lista de clientes conectados.

    ```erlang
    add_client(Socket) ->
        register(client_manager, spawn(fun() -> client_manager([Socket]) end)).
    ```

3. **Función `remove_client/1`**:
    - Elimina un cliente de la lista cuando se desconecta.

    ```erlang
    remove_client(Socket) ->
        client_manager ! {remove, Socket}.
    ```

4. **Función `get_clients/0`**:
    - Devuelve la lista de todos los clientes conectados.

    ```erlang
    get_clients() ->
        client_manager ! {get_clients, self()},
        receive
            {clients, Clients} -> Clients
        end.
    ```

5. **Función `broadcast/1`**:
    - Envía un mensaje a todos los clientes conectados.

    ```erlang
    broadcast(Message) ->
        Clients = get_clients(),
        lists:foreach(fun(Client) -> gen_tcp:send(Client, Message) end, Clients).
    ```

### Módulo 3: Proceso de Gestión de Clientes (client_manager)

**Objetivo**: Gestionar internamente la lista de clientes conectados utilizando un proceso dedicado.

1. **Definir el Módulo y Funciones Exportadas**:
    - Crea un módulo llamado `client_manager`.
    - Exporta la función `start_link/0`.

    ```erlang
    -module(client_manager).
    -export([start_link/0]).
    ```

2. **Función `start_link/0`**:
    - Inicia el proceso del gestor de clientes.

    ```erlang
    start_link() ->
        register(client_manager, spawn(fun() -> client_manager([]) end)).
    ```

3. **Función `client_manager/1`**:
    - Mantiene y actualiza la lista de clientes conectados.

    ```erlang
    client_manager(Clients) ->
        receive
            {add, Socket} ->
                client_manager([Socket | Clients]);
            {remove, Socket} ->
                client_manager(lists:delete(Socket, Clients));
            {get_clients, Caller} ->
                Caller ! {clients, Clients},
                client_manager(Clients);
            _Other ->
                client_manager(Clients)
        end.
    ```

### Módulo 4: Cliente de Chat (chat_client)

**Objetivo**: Crear un módulo cliente para conectarse al servidor de chat y enviar/recibir mensajes.

1. **Definir el Módulo y Funciones Exportadas**:
    - Crea un módulo llamado `chat_client`.
    - Exporta las funciones `start/0` y `send_message/2`.

    ```erlang
    -module(chat_client).
    -export([start/0, send_message/2]).
    ```

2. **Función `start/0`**:
    - Conecta al cliente con el servidor de chat y lee los mensajes.

    ```erlang
    start() ->
        {ok, Socket} = gen_tcp:connect("localhost", 1234, [binary, {packet, 0}, {active, false}]),
        spawn(fun() -> receive_messages(Socket) end),
        {ok, Socket}.
    ```

3. **Función `send_message/2`**:
    - Envia un mensaje al servidor de chat.

    ```erlang
    send_message(Socket, Message) ->
        gen_tcp:send(Socket, Message).
    ```

4. **Función `receive_messages/1`**:
    - Escucha y muestra los mensajes recibidos del servidor.

    ```erlang
    receive_messages(Socket) ->
        case gen_tcp:recv(Socket, 0) of
            {ok, Message} ->
                io:format("Mensaje recibido: ~p~n", [Message]),
                receive_messages(Socket);
            {error, closed} ->
                io:format("Conexión cerrada~n"),
                ok
        end.
    ```

## Pasos Finales

1. **Compilar y Ejecutar**:
    - Compila todos los módulos utilizando el shell de Erlang.
    - Ejecuta `chat_server:start().` para iniciar el servidor de chat.
    - Usa `chat_client:start().` para conectar un cliente al servidor. Repite esto para conectar múltiples clientes.

2. **Interacción**:
    - Envía mensajes usando `chat_client:send_message(Socket, "Mensaje").` desde los diferentes clientes y observa cómo los mensajes son retransmitidos a todos los clientes conectados.

3. **Supervisión**:
    - Implementa un simple supervisor para reiniciar procesos de manejo de clientes si fallan, reforzando la tolerancia a fallos del sistema.

