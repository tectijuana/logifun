
### Proyecto 10: Implementación de un Servidor Web Simple en Erlang

#### Objetivo del Proyecto

El objetivo de este proyecto es implementar un servidor web simple en Erlang que pueda manejar solicitudes HTTP. Este proyecto demostrará cómo construir aplicaciones web básicas en Erlang, aprovechando su capacidad para manejar concurrencia y comunicación entre procesos de manera eficiente. Se explorará cómo utilizar bibliotecas estándar de Erlang para recibir y responder a solicitudes HTTP.

#### Descripción del Proyecto

El proyecto consistirá en crear un servidor web básico que escuche en un puerto específico, procese solicitudes HTTP entrantes y envíe respuestas adecuadas. Este servidor será capaz de manejar solicitudes de tipo GET y devolverá diferentes respuestas basadas en la ruta solicitada. Se utilizará la biblioteca `inets` de Erlang para manejar el protocolo HTTP, lo que simplificará la gestión de la comunicación.

#### Requisitos del Proyecto

1. **Configuración del Servidor Web**: Implementar un módulo en Erlang que inicie un servidor HTTP y escuche en un puerto específico.
2. **Manejo de Solicitudes HTTP**: El servidor debe ser capaz de manejar solicitudes HTTP de tipo GET.
3. **Respuestas Basadas en Rutas**: El servidor debe devolver diferentes respuestas según la ruta solicitada (por ejemplo, `/`, `/hello`, `/time`).
4. **Uso de la Biblioteca `inets`**: Utilizar la biblioteca estándar `inets` para gestionar las solicitudes y respuestas HTTP.
5. **Manejo de Errores**: El servidor debe manejar adecuadamente las rutas no reconocidas y devolver respuestas de error apropiadas.

#### Implementación Detallada

1. **Configuración del Servidor HTTP**:
   - Utilizar el módulo `httpd` de la biblioteca `inets` para configurar y ejecutar el servidor HTTP. Definir un módulo que actuará como el manejador de solicitudes.

   ```erlang
   -module(simple_web_server).
   -export([start/0, stop/0, request_handler/1]).

   -define(PORT, 8080).

   start() ->
       %% Iniciar el servicio httpd de inets
       inets:start(),
       ServiceConfig = [
           {modules, [simple_web_server]},
           {port, ?PORT},
           {server_name, "Simple Erlang Web Server"},
           {server_root, "."},
           {document_root, "."},
           {bind_address, "0.0.0.0"},
           {directory_index, ["index.html"]},
           {mime_types, [{"html", "text/html"}]}
       ],
       %% Arrancar el servidor con la configuración
       inets:start(httpd, ServiceConfig),
       io:format("Servidor web iniciado en el puerto ~p~n", [?PORT]).

   stop() ->
       inets:stop(httpd, ?PORT),
       io:format("Servidor web detenido.~n").
   ```

2. **Manejo de Solicitudes HTTP**:
   - Definir el manejador de solicitudes que será llamado por el servidor HTTP para procesar cada solicitud. Este manejador determinará la respuesta basada en la ruta solicitada.

   ```erlang
   request_handler({Request, Client}) ->
       Method = proplists:get_value(method, Request),
       Path = proplists:get_value(uri, Request),
       case {Method, Path} of
           {'GET', "/"} ->
               Response = "Bienvenido al Servidor Web Simple de Erlang!",
               send_response(Client, 200, Response);
           {'GET', "/hello"} ->
               Response = "Hola, Mundo!",
               send_response(Client, 200, Response);
           {'GET', "/time"} ->
               Response = io_lib:format("Hora actual: ~p", [calendar:local_time()]),
               send_response(Client, 200, Response);
           _ ->
               Response = "Página no encontrada",
               send_response(Client, 404, Response)
       end.

   send_response(Client, Status, Body) ->
       HTTPStatus = case Status of
           200 -> "200 OK";
           404 -> "404 Not Found";
           _ -> "500 Internal Server Error"
       end,
       Response = io_lib:format("HTTP/1.1 ~s\r\nContent-Length: ~p\r\nContent-Type: text/plain\r\n\r\n~s", [HTTPStatus, byte_size(Body), Body]),
       gen_tcp:send(Client, Response),
       gen_tcp:close(Client).
   ```

3. **Inicio y Prueba del Servidor**:
   - Para iniciar el servidor, se ejecuta la función `simple_web_server:start/0` en la shell de Erlang. Esto pondrá al servidor a la escucha en el puerto 8080.

   ```erlang
   %% Iniciar el servidor web
   simple_web_server:start().
   ```

   - Una vez iniciado, se puede acceder al servidor web desde un navegador web o herramientas de línea de comandos como `curl`:

   ```bash
   curl http://localhost:8080/
   curl http://localhost:8080/hello
   curl http://localhost:8080/time
   curl http://localhost:8080/not_found
   ```

4. **Manejo de Errores**:
   - El manejador de solicitudes debe devolver un error 404 para las rutas no reconocidas, asegurando que el servidor responda adecuadamente a las solicitudes desconocidas.

#### Evaluación del Proyecto

- **Funcionalidad del Servidor**: El servidor debe ser capaz de manejar y responder a las solicitudes HTTP de tipo GET.
- **Respuestas Basadas en Rutas**: El servidor debe devolver respuestas adecuadas según la ruta solicitada y manejar rutas no reconocidas.
- **Uso de `inets`**: Se debe demostrar un uso efectivo de la biblioteca `inets` para la gestión de solicitudes HTTP.
- **Manejo de Errores**: El servidor debe manejar adecuadamente los errores y devolver las respuestas HTTP apropiadas.

#### Entrega

- El proyecto completo debe incluir todos los archivos de código fuente (.erl) y una documentación que explique cómo iniciar y utilizar el servidor web.
- Incluir un archivo README que describa los pasos necesarios para ejecutar el servidor y ejemplos de cómo interactuar con él.

### Conclusión

Este proyecto da capacidad de Erlang para construir aplicaciones web básicas utilizando  modo de manejar concurrencia y comunicación de procesos. Se espera que los estudiantes comprendan y apliquen conceptos clave de manejo de solicitudes HTTP y desarrollo de servidores web en Erlang.
