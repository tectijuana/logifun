

# MANEJO DE RECURSOS PROCESOS  

###Código Erlang: `stress_test.erl`

```erlang
%% Archivo: stress_test.erl
-module(stress_test).
-export([start/0, stress/1]).

start() ->
    %% Obtener el número de núcleos de CPU disponibles
    CpuCount = erlang:system_info(logical_processors_available),
    io:format("Iniciando prueba de estrés en ~p núcleos de CPU...~n", [CpuCount]),
    
    %% Crear un proceso de estrés para cada núcleo de CPU
    [spawn(fun() -> stress(0) end) || _ <- lists:seq(1, CpuCount)],
    ok.

stress(Count) ->
    %% Realizar cálculos intensivos
    lists:foldl(fun(_, Acc) -> Acc + math:sin(Acc) end, 0, lists:seq(1, 10000)),
    
    %% Bucle indefinido para mantener el estrés
    stress(Count + 1).
```

### Cómo Ejecutar el Programa en Raspberry Pi 5

#### Prerrequisitos

- **Erlang Instalado**: Asegúrate de que Erlang esté instalado en tu Raspberry Pi 5. Puedes instalarlo usando:

  ```bash
  sudo apt-get update
  sudo apt-get install erlang
  ```

#### Pasos para Compilar y Ejecutar

1. **Guardar el Código**:

   Guarda el código Erlang anterior en un archivo llamado `stress_test.erl`:

   ```bash
   nano stress_test.erl
   ```

   Pega el código en el archivo y guarda (`Ctrl + X`, luego `Y`, y `Enter`).

2. **Compilar el Código**:

   Abre una terminal y compila el programa Erlang:

   ```bash
   erlc stress_test.erl
   ```

   Esto generará un archivo `stress_test.beam` en el mismo directorio.

3. **Ejecutar el Programa**:

   Tienes dos opciones para ejecutar el programa:

   - **Opción 1: Desde la Consola de Erlang**

     Inicia la consola de Erlang:

     ```bash
     erl
     ```

     Dentro de la consola, ejecuta:

     ```erlang
     stress_test:start().
     ```

     Para salir de la consola después de la prueba (puedes necesitar detener el programa manualmente, ya que se ejecuta indefinidamente):

     ```erlang
     q().
     ```

   - **Opción 2: Ejecución Directa**

     Ejecuta el programa sin entrar en la consola de Erlang:

     ```bash
     erl -noshell -s stress_test start
     ```

     **Nota**: Esto se ejecutará indefinidamente. Para detener el programa, presiona `Ctrl + C` dos veces.

#### Monitoreo del Rendimiento del Sistema

Mientras el programa se está ejecutando, puedes monitorear el rendimiento de tu Raspberry Pi usando:

- **htop**:

  Instala htop si no lo tienes instalado:

  ```bash
  sudo apt-get install htop
  ```

  Ejecuta htop para ver el uso de la CPU:

  ```bash
  htop
  ```

- **vcgencmd** (para temperatura):

  ```bash
  watch -n 1 vcgencmd measure_temp
  ```

### Explicación del Código

- **Función start/0**:

  - Recupera el número de procesadores lógicos (núcleos de CPU).
  - Crea un proceso de Erlang (`stress/1`) para cada núcleo de CPU.
  - Cada proceso se ejecuta indefinidamente para maximizar la utilización de la CPU.

- **Función stress/1**:

  - Realiza una tarea computacionalmente intensiva calculando la suma de valores seno.
  - Se llama a sí misma recursivamente para continuar estresando la CPU.

### Precauciones

- **Uso de Recursos**: Este programa maximizará el uso de la CPU y puede causar que tu Raspberry Pi se caliente significativamente.
- **Enfriamiento**: Asegúrate de que tu Raspberry Pi 5 tenga un enfriamiento adecuado para evitar el sobrecalentamiento.
- **Interrumpir el Programa**: Para detener la prueba de estrés, es posible que debas terminar los procesos manualmente (`Ctrl + C` o matar el proceso de Erlang).

---

![Screenshot from 2024-09-19 13-18-57](https://github.com/user-attachments/assets/9bc379c4-e7a6-4f39-9096-ea6b5d8b0f74)

## Explicación sobre el uso del comando `kill` y el operador `&` en Linux para gestionar procesos en segundo plano:

### Comando `kill`

El comando `kill` se utiliza para enviar señales a los procesos en Linux, generalmente para finalizar un proceso. Su uso básico es:

```bash
kill [opciones] PID
```

- **PID**: Es el identificador del proceso que deseas terminar.
- **Opciones**:
  - `-9`: Forzar la terminación del proceso (SIGKILL).
  - `-15`: Terminar el proceso de manera más suave (SIGTERM, esta es la señal predeterminada).

**Ejemplo**:

1. Para encontrar el PID de un proceso, puedes usar `ps` o `top`:

   ```bash
   ps aux | grep nombre_del_proceso
   ```

2. Luego, usa `kill` para terminar el proceso:

   ```bash
   kill -15 1234  # Reemplaza 1234 con el PID real
   ```

   O para forzar la terminación:

   ```bash
   kill -9 1234
   ```

### Operador `&`

El operador `&` se utiliza para ejecutar un comando en segundo plano, lo que te permite continuar usando la terminal mientras el proceso sigue ejecutándose.

**Ejemplo**:

```bash
comando & 
```

Por ejemplo, si deseas ejecutar un script llamado `mi_script.sh` en segundo plano, usarías:

```bash
./mi_script.sh &
```

### Combinando Ambos

Puedes ejecutar un proceso en segundo plano y luego utilizar `kill` para detenerlo cuando sea necesario. Por ejemplo:

1. Ejecuta un proceso en segundo plano:

   ```bash
   ./mi_script.sh &
   ```

2. Toma nota del PID que se muestra después de ejecutar el comando.

3. Cuando quieras terminar el proceso, usa:

   ```bash
   kill PID
   ```

### Listar Procesos en Segundo Plano

Para ver los procesos en segundo plano que has iniciado en tu sesión de terminal, puedes usar el comando:

```bash
jobs
```

Esto mostrará una lista de trabajos en segundo plano junto con sus estados (ejecutándose, detenido, etc.). Si deseas llevar un proceso de fondo a primer plano, puedes usar el comando `fg` seguido del número del trabajo.

### Importante recordar

- **`kill`**: Para terminar procesos, utilizando PID.
- **`&`**: Para ejecutar procesos en segundo plano, permitiendo que la terminal siga libre.

---

### Notas adicionales

Siguiendo los pasos anteriores, puedes realizar una prueba de estrés en tu Raspberry Pi 5 utilizando Erlang. Esto te ayudará a evaluar el rendimiento y la estabilidad de tu sistema bajo una carga pesada de CPU.
