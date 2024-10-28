
![Lego Teacher Microbit](https://github.com/user-attachments/assets/b480c31a-fe07-44ae-91d2-ec6fae4125de)

Este ejemplo demostrará cómo utilizar la micro:bit para recolectar datos de sensores y procesarlos con Prolog para resolver problemas lógicos, lo cual puede enriquecer el material de enseñanza sobre sistemas lógicos.

## **Requisitos Previos**

1. **Hardware:**
   - **micro:bit v2:** Asegúrate de tener una micro:bit versión 2.
   - **Cable USB:** Para conectar la micro:bit al PC.
   - **Componentes adicionales (opcional):** Sensores externos como sensores de temperatura, luminosidad, etc., dependiendo del proyecto.

2. **Software:**
   - **Prolog:** Recomiendo **SWI-Prolog**, que es gratuito y ampliamente utilizado.
     - **Descarga e instalación:** [SWI-Prolog Descargar](https://www.swi-prolog.org/Download.html)
   - **Editor de texto o IDE para Prolog:** Por ejemplo, **SWI-Prolog IDE** que viene con la instalación de SWI-Prolog.
   - **Entorno de desarrollo para micro:bit:** Puedes usar **Mu Editor**, **Microsoft MakeCode** o **Thonny** si prefieres programar en MicroPython.
     - **Descarga de Mu Editor:** [Mu Editor Descargar](https://codewith.mu/en/download)

## **Ejemplo de Proyecto: Monitor de Temperatura y Lógica de Decisión**

En este proyecto, la micro:bit medirá la temperatura ambiente y enviará los datos al PC. Prolog procesará estos datos para determinar si se debe encender un ventilador, una luz, etc., basándose en reglas lógicas definidas.

### **Paso 1: Programar la micro:bit**

Vamos a programar la micro:bit para leer la temperatura y enviar los datos a través del puerto serial.

**Ejemplo en MicroPython:**

1. Abre **Mu Editor** y selecciona el modo **MicroPython**.
2. Escribe el siguiente código:

```python
from microbit import *
import utime

uart.init(baudrate=9600)

while True:
    temperature = temperature()
    uart.write(str(temperature) + "\n")
    sleep(1000)  # Espera 1 segundo antes de la siguiente lectura
```

3. Guarda y descarga el programa a la micro:bit conectada vía USB.

**Explicación:**
- La micro:bit lee la temperatura integrada.
- Envía la temperatura al PC a través del puerto serial cada segundo.

### **Paso 2: Configurar SWI-Prolog en Windows**

1. **Instalación:**
   - Descarga e instala SWI-Prolog desde [SWI-Prolog Descargar](https://www.swi-prolog.org/Download.html).

2. **Configuración del Puerto Serial:**
   - Identifica el puerto COM al que está conectada la micro:bit. Puedes verificar esto en el **Administrador de Dispositivos** de Windows bajo "Puertos (COM & LPT)".

3. **Código Prolog para Leer Datos Seriales y Aplicar Reglas Lógicas:**

Crea un archivo Prolog, por ejemplo, `monitor_temperatura.pl`, con el siguiente contenido:

```prolog
:- use_module(library(ansi_term)).
:- use_module(library(readutil)).

% Define las reglas lógicas
accion_based_on_temp(Temp) :-
    Temp < 20,
    writeln('Temperatura baja: Encender la luz.').
accion_based_on_temp(Temp) :-
    Temp >= 20,
    Temp < 30,
    writeln('Temperatura óptima: Mantener todo como está.').
accion_based_on_temp(Temp) :-
    Temp >= 30,
    writeln('Temperatura alta: Encender el ventilador.').

% Función para leer del puerto serial
leer_serial(Puerto) :-
    setup_call_cleanup(
        open(Puerto, read, Stream, [type(text)]),
        leer_datos(Stream),
        close(Stream)
    ).

leer_datos(Stream) :-
    read_line_to_string(Stream, Line),
    (   Line \= end_of_file,
        catch(number_string(Temp, Line),
              _,
              (writeln('Error al convertir la lectura a número.'), Temp = 0)),
        format('Temperatura leída: ~w°C~n', [Temp]),
        accion_based_on_temp(Temp),
        leer_datos(Stream)
    ;
        writeln('Fin de la lectura serial.')
    ).

% Punto de entrada
:- initialization(main).

main :-
    % Reemplaza 'COM3' con el puerto correcto
    Puerto = 'COM3',
    writeln('Iniciando monitor de temperatura...'),
    leer_serial(Puerto).
```

**Explicación del Código:**

- **Importación de Módulos:**
  - `library(ansi_term)`: Para formato de texto en la consola (opcional).
  - `library(readutil)`: Para leer líneas de texto.

- **Reglas Lógicas (`accion_based_on_temp/1`):**
  - Define acciones basadas en la temperatura leída.
    - **< 20°C:** Enciende la luz.
    - **20°C - 30°C:** Mantiene todo igual.
    - **>= 30°C:** Enciende el ventilador.

- **Funciones de Lectura Serial (`leer_serial/1` y `leer_datos/1`):**
  - Abren el puerto serial y leen las líneas enviadas por la micro:bit.
  - Convierte la línea leída en un número (temperatura).
  - Aplica las reglas lógicas basadas en la temperatura.
  - Continua leyendo hasta el final del archivo (cuando se desconecta la micro:bit).

- **Punto de Entrada (`main/0`):**
  - Especifica el puerto serial a usar (debes reemplazar `'COM3'` con el puerto correcto).
  - Inicia la lectura serial.

### **Paso 3: Ejecutar el Proyecto**

1. **Conectar la micro:bit:**
   - Conecta la micro:bit al PC mediante el cable USB.
   - Asegúrate de que el programa en la micro:bit esté corriendo.

2. **Ejecutar el Código Prolog:**
   - Abre **SWI-Prolog**.
   - Navega hasta el directorio donde guardaste `monitor_temperatura.pl`.
   - Carga el archivo con el comando:
     ```prolog
     ?- [monitor_temperatura].
     ```
   - El programa debería comenzar a leer las temperaturas y aplicar las reglas lógicas, mostrando las acciones en la consola.

**Salida Esperada:**
```
Iniciando monitor de temperatura...
Temperatura leída: 18°C
Temperatura baja: Encender la luz.
Temperatura leída: 25°C
Temperatura óptima: Mantener todo como está.
Temperatura leída: 32°C
Temperatura alta: Encender el ventilador.
...
```

### **Paso 4: Extender el Proyecto (Opcional)**

Puedes ampliar este proyecto agregando más sensores a la micro:bit y definiendo reglas lógicas más complejas en Prolog. Por ejemplo:

- **Sensor de Luminosidad:** Para controlar luces basadas en la luminosidad ambiental.
- **Sensor de Movimiento:** Para activar alarmas o notificaciones.

### **Consideraciones Adicionales**

1. **Comunicación Serial:**
   - Asegúrate de que ningún otro programa esté accediendo al puerto serial que usa la micro:bit mientras Prolog está intentando leerlo.
   
2. **Manejo de Errores:**
   - El código Prolog incluye una captura de errores al convertir la lectura a número. Puedes expandir esto para manejar diferentes tipos de errores o condiciones inesperadas.

3. **Optimización:**
   - Para proyectos más avanzados, podrías implementar comunicación bidireccional, donde Prolog no solo recibe datos de la micro:bit sino que también envía comandos para controlar dispositivos conectados.

## **Conclusión**

Este ejemplo demuestra cómo integrar la micro:bit versión 2 con Prolog en un entorno Windows para crear un sistema lógico que responde a datos del mundo real. Este enfoque no solo refuerza conceptos de programación lógica con Prolog, sino que también introduce a los estudiantes al trabajo con hardware y comunicación serial, proporcionando una experiencia educativa completa y práctica.

Espero que este ejemplo te sea útil para mejorar el material de enseñanza sobre sistemas lógicos con Prolog. Si tienes alguna pregunta adicional o necesitas más detalles sobre algún paso, no dudes en preguntar.
