


```erlang
%% Módulo de ejemplo para un juego simple de Ping Pong en Erlang
-module(ping_pong).

%% Exporta las funciones para que puedan ser llamadas desde fuera del módulo
-export([start/0, ping/1, pong/0]).

%% Función principal que inicia los procesos de ping y pong
start() ->
    %% Crea el proceso de pong y obtiene su PID
    PongPid = spawn(?MODULE, pong, []),
    %% Crea el proceso de ping y pasa el PID de pong para la comunicación
    spawn(?MODULE, ping, [PongPid]).

%% Función para el proceso de ping
ping(PongPid) ->
    %% Envía el mensaje 'ping' al proceso pong
    PongPid ! {ping, self()},
    %% Espera un mensaje de respuesta
    receive
        pong ->
            io:format("Ping recibió pong~n"),
            %% Envía 'ping' de nuevo después de recibir 'pong'
            ping(PongPid)
    end.

%% Función para el proceso de pong
pong() ->
    receive
        %% Espera un mensaje con la estructura {ping, Sender}
        {ping, Sender} ->
            io:format("Pong recibió ping~n"),
            %% Envía 'pong' de vuelta al proceso que envió el ping
            Sender ! pong,
            %% Llama recursivamente para esperar el siguiente mensaje
            pong()
    end.
