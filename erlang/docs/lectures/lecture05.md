# Herramientas de Depuración para Erlang

La depuración en Erlang es esencial para desarrollar aplicaciones robustas y escalables. A continuación, se describen las herramientas y técnicas más utilizadas para depurar código en este ambiente, incluyendo instrucciones de instalación en Ubuntu Linux.

## 1. Depurador Gráfico (Debugger)

El depurador gráfico es una herramienta integrada en Erlang/OTP que permite:

- **Ejecución paso a paso**: Avanzar por el código línea por línea.
- **Puntos de interrupción**: Detener la ejecución en líneas específicas.
- **Inspección de variables**: Ver el estado de las variables en tiempo real.

### Instalación en Ubuntu:

El depurador gráfico se encuentra en el paquete `erlang-debugger`. Para instalarlo:

```bash
sudo apt update
sudo apt install erlang-debugger
```

### Cómo utilizarlo:

```erlang
% Compilar el módulo con información de depuración
c(mi_modulo, [debug_info]).

% Iniciar el depurador
debugger:start().

% Interpretar el módulo para depuración
int:i(mi_modulo).
```

## 2. Trazado con el Módulo `dbg`

El módulo `dbg` permite realizar trazas en el sistema, lo que es útil para:

- **Monitorear llamadas a funciones**.
- **Inspeccionar mensajes entre procesos**.
- **Filtrar eventos específicos**.

### Instalación en Ubuntu:

El módulo `dbg` se incluye en el paquete `erlang-tools`. Para instalarlo:

```bash
sudo apt update
sudo apt install erlang-tools
```

### Ejemplo de uso:

```erlang
% Iniciar el trazador
dbg:tracer().

% Trazar todas las llamadas a funciones en 'mi_modulo'
dbg:p(all, c).
dbg:tpl(mi_modulo, '_', []).

% Detener y limpiar el trazado
dbg:stop_clear().
```

## 3. Uso de `logger` para Registro

El módulo `logger` es fundamental para registrar eventos y errores:

- **Registrar información de depuración**.
- **Configurar niveles de registro (debug, info, warning, error)**.
- **Dirigir los registros a diferentes manejadores (consola, archivos, etc.)**.

### Instalación en Ubuntu:

El módulo `logger` viene incluido en Erlang/OTP a partir de la versión 21. Si no lo tiene, asegúrese de instalar Erlang actualizado:

```bash
sudo apt update
sudo apt install erlang-base
```

### Ejemplo:

```erlang
logger:info("Valor de X: ~p", [X]).
```

## 4. Herramienta Gráfica `observer`

`observer` es una interfaz gráfica que ofrece:

- **Monitoreo de procesos**: Ver el estado y las estadísticas de los procesos.
- **Visualización del árbol de supervisión**.
- **Inspección de la utilización del sistema (CPU, memoria, E/S)**.

### Instalación en Ubuntu:

El paquete necesario es `erlang-observer`:

```bash
sudo apt update
sudo apt install erlang-observer
```

### Cómo iniciarlo:

```erlang
observer:start().
```

## 5. Análisis con `recon`

`recon` es una biblioteca externa que proporciona funciones avanzadas para diagnosticar sistemas en producción:

- **Análisis de procesos lentos o bloqueados**.
- **Recolección de información sin afectar el rendimiento**.

### Instalación y uso básico:

`recon` no está incluido en los paquetes de Erlang y debe agregarse como dependencia en su proyecto.

#### Usando `rebar3`:

1. **Instalar `rebar3`** (si no lo tiene):

   ```bash
   sudo apt update
   sudo apt install rebar3
   ```

2. **Agregar `recon` como dependencia** en el archivo `rebar.config`:

   ```erlang
   {deps, [recon]}.
   ```

3. **Actualizar las dependencias**:

   ```bash
   rebar3 get-deps
   rebar3 compile
   ```

### Ejemplo de uso:

```erlang
% Obtener información del sistema
recon:info().
```

## 6. Pruebas Unitarias con `eunit`

Aunque no es una herramienta de depuración directa, las pruebas unitarias ayudan a:

- **Detectar errores tempranamente**.
- **Asegurar el correcto funcionamiento de funciones individuales**.

### Instalación en Ubuntu:

El paquete es `erlang-eunit`:

```bash
sudo apt update
sudo apt install erlang-eunit
```

### Ejemplo de una prueba unitaria:

```erlang
% Dentro de mi_modulo.erl
my_test() ->
    ?assertEqual(esperado, funcion_a_probar(entrada)).
```

## 7. Monitoreo con `runtime_tools`

El paquete `runtime_tools` incluye herramientas para:

- **Generar volcados de pila (stack traces)**.
- **Realizar análisis de rendimiento en tiempo de ejecución**.

### Instalación en Ubuntu:

Instalar el paquete `erlang-runtime-tools`:

```bash
sudo apt update
sudo apt install erlang-runtime-tools
```

### Ejemplo:

```erlang
% Obtener un stack trace de todos los procesos
erts_dbg:stack_trace().
```

## Conclusión

Las herramientas de depuración en Erlang son poderosas y variadas, permitiendo desde el trazado detallado de funciones hasta el monitoreo en producción sin interrupciones. Familiarizarse con estas herramientas y saber cómo instalarlas es clave para el desarrollo eficiente y la resolución de problemas en sistemas Erlang.
