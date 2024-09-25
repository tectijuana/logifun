# Tutorial para crear una aplicación demo en Erlang con Cowboy en AWS EC2 Ubuntu

Este tutorial te guiará a través del proceso de instalación de Erlang y el framework Cowboy en una instancia Ubuntu de AWS EC2. Al final, tendrás una aplicación web básica que muestra "¡Hola Mundo desde Cowboy!" en tu navegador.

## Requisitos previos

- **Instancia Ubuntu en AWS EC2**: Asumimos que ya tienes acceso a una instancia Ubuntu limpia y que sabes cómo conectarte a ella.

## Pasos a seguir

### 1. Actualizar el sistema

Antes de comenzar, es buena práctica actualizar los paquetes del sistema.

```bash
sudo apt update
sudo apt upgrade -y
```

### 2. Instalar Erlang

Instala Erlang utilizando el gestor de paquetes `apt`.

```bash
sudo apt install erlang -y
```

Verifica la instalación ejecutando:

```bash
erl
```

Deberías ver algo como:

```
Erlang/OTP 23 [erts-11.1.8] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1]

Eshell V11.1.8  (abort with ^G)
1>
```

Escribe `q().` para salir del shell de Erlang.

### 3. Instalar rebar3

`rebar3` es una herramienta de construcción para proyectos Erlang.

```bash
wget https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/
```

Verifica la instalación:

```bash
rebar3 version
```

### 4. Crear un nuevo proyecto

Crea una nueva aplicación usando `rebar3`.

```bash
rebar3 new release my_cowboy_app
cd my_cowboy_app
```

### 5. Agregar Cowboy como dependencia

Edita el archivo `rebar.config` y agrega Cowboy.

```bash
nano rebar.config
```

Agrega `{cowboy, "2.9.0"}` a las dependencias:

```erlang
{deps, [
    {cowboy, "2.9.0"}
]}.
```

### 6. Actualizar las dependencias

Descarga las dependencias especificadas.

```bash
rebar3 get-deps
```

### 7. Crear un manejador de solicitudes

Crea un nuevo archivo para manejar las solicitudes HTTP.

```bash
nano src/my_cowboy_handler.erl
```

Agrega el siguiente código:

```erlang
%% Declaración del módulo y exportación de funciones
-module(my_cowboy_handler).
-export([init/2]).

%% Función de inicialización del manejador
init(Req, State) ->
    %% Enviar una respuesta HTTP 200 con "¡Hola Mundo desde Cowboy!"
    {ok, Resp} = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"¡Hola Mundo desde Cowboy!">>,
        Req),
    %% Terminar el proceso
    {ok, Resp, State}.
```

### 8. Configurar la aplicación para usar Cowboy

Edita el archivo `src/my_cowboy_app_app.erl`.

```bash
nano src/my_cowboy_app_app.erl
```

Modifica el código:

```erlang
-module(my_cowboy_app_app).
-behaviour(application).

%% Exportar funciones de inicio y parada
-export([start/2, stop/1]).

%% Iniciar la aplicación
start(_Type, _Args) ->
    %% Definir las rutas
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", my_cowboy_handler, []}
        ]}
    ]),
    %% Iniciar Cowboy en el puerto 8080
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}),
    my_cowboy_app_sup:start_link().

%% Detener la aplicación
stop(_State) ->
    ok.
```

### 9. Compilar el proyecto

Compila el proyecto para asegurarte de que no hay errores.

```bash
rebar3 compile
```

### 10. Ejecutar la aplicación

Inicia la aplicación en modo shell.

```bash
rebar3 shell
```

Deberías ver que la aplicación se ha iniciado correctamente.

### 11. Configurar reglas de seguridad en AWS

Asegúrate de que el puerto 8080 esté abierto en el grupo de seguridad de tu instancia EC2.

- Ve a la consola de AWS EC2.
- Selecciona tu instancia y ve a la sección de **Grupos de seguridad**.
- Edita las reglas de entrada y agrega una nueva regla:
  - **Tipo**: Personalizado TCP
  - **Puerto**: 8080
  - **Origen**: 0.0.0.0/0 (o tu IP específica por seguridad)

### 12. Probar la aplicación

En tu navegador web, visita:

```
http://[DIRECCIÓN_IP_DE_TU_INSTANCIA]:8080/
```

Deberías ver:

```
¡Hola Mundo desde Cowboy!
```

## Conclusión

Has creado con éxito una aplicación web básica en Erlang utilizando el framework Cowboy en una instancia Ubuntu de AWS EC2. A partir de aquí, puedes expandir la aplicación agregando más rutas y funcionalidades según tus necesidades.

## Notas adicionales

- **Detener la aplicación**: Para detener la aplicación, simplemente presiona `Ctrl+C` dos veces en la terminal donde se está ejecutando.
- **Ejecutar en segundo plano**: Para ejecutar la aplicación sin el shell interactivo, puedes usar `rebar3 release` y luego ejecutar el binario generado.

¡Disfruta desarrollando con Erlang y Cowboy!