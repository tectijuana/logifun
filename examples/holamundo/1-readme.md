Para introducir a los estudiantes al mundo de Erlang de manera sencilla y motivadora, un buen punto de partida es el clásico "Hola Mundo". Este ejemplo muestra cómo escribir, compilar y ejecutar un programa Erlang que imprime "Hola, Mundo!" en la consola.

### Ejemplo Sencillo en Erlang: Hola Mundo

#### Código de "Hola Mundo" en Erlang

Puedes crear un archivo llamado `hola_mundo.erl` con el siguiente contenido:

```erlang
% hola_mundo.erl
-module(hola_mundo).
-export([saludar/0]).

saludar() ->
    io:format("Hola, Mundo!~n").
```

- **Explicación del código**:
  - `-module(hola_mundo).`: Define el nombre del módulo, que debe coincidir con el nombre del archivo (sin la extensión `.erl`).
  - `-export([saludar/0]).`: Exporta la función `saludar` para que pueda ser llamada desde fuera del módulo. El `/0` indica que esta función no toma ningún argumento.
  - `saludar() ->`: Define una función llamada `saludar`.
  - `io:format("Hola, Mundo!~n").`: Imprime "Hola, Mundo!" en la consola, donde `~n` es una nueva línea.

### Compilación y Ejecución

Hay dos maneras de compilar y ejecutar el código de Erlang: directamente desde la consola de Erlang o utilizando un Makefile para simplificar el proceso.
### Compilación y Ejecución de un Programa Erlang en PowerShell y Linux Terminal

A continuación, se describe cómo compilar y ejecutar un sencillo programa de Erlang ("Hola Mundo") utilizando tanto PowerShell en Windows como una terminal en Linux.

#### A. Compilación Directa en la Consola de Erlang

Para muchos principiantes, usar la consola de Erlang directamente es la forma más sencilla y comprensible de empezar. Aquí están los pasos:

1. **Abrir la Consola de Erlang**:

   - **En PowerShell (Windows)**:
     - Abre PowerShell y escribe `erl` para iniciar la consola de Erlang.
     - Asegúrate de que Erlang esté instalado y configurado en tu sistema. Si `erl` no es reconocido, verifica la instalación de Erlang y la variable de entorno `PATH`.

   - **En la Terminal de Linux**:
     - Abre una terminal y escribe `erl` para iniciar la consola de Erlang.
     - Al igual que en Windows, asegúrate de tener Erlang instalado. Puedes instalarlo con tu gestor de paquetes favorito (por ejemplo, `sudo apt-get install erlang` en Debian/Ubuntu).

2. **Compilar el Módulo**:

   - En la consola de Erlang (tanto en PowerShell como en Linux), navega hasta el directorio donde se encuentra el archivo `hola_mundo.erl`. Usa el siguiente comando para compilar el archivo:
     ```erlang
     c(hola_mundo).
     ```
   - Esto debería mostrar un mensaje indicando que el módulo ha sido compilado correctamente. Se creará un archivo de bytecode `hola_mundo.beam` en el mismo directorio.

3. **Ejecutar la Función**:

   - Después de compilar el módulo, ejecuta la función `saludar` con el siguiente comando:
     ```erlang
     hola_mundo:saludar().
     ```
   - Deberías ver el output `Hola, Mundo!` en la consola.

### Compilación y Ejecución desde la Línea de Comandos (Sin Entrar en la Consola de Erlang)

En lugar de usar la consola de Erlang, también puedes compilar y ejecutar el código directamente desde PowerShell o la terminal de Linux usando comandos. Esta opción es útil cuando quieres automatizar el proceso o ejecutarlo desde scripts.

#### En PowerShell (Windows):

1. **Compilar el Módulo**:

   - Usa el compilador de Erlang directamente desde PowerShell para compilar el archivo:
     ```shell
     erlc hola_mundo.erl
     ```
   - Esto generará el archivo `hola_mundo.beam` en el mismo directorio.

2. **Ejecutar la Función**:

   - Ejecuta el módulo compilado y la función directamente:
     ```shell
     erl -noshell -s hola_mundo saludar -s init stop
     ```
   - `-noshell` ejecuta Erlang sin abrir la shell interactiva.
   - `-s hola_mundo saludar` especifica que se debe ejecutar la función `saludar` del módulo `hola_mundo`.
   - `-s init stop` detiene la máquina virtual BEAM después de ejecutar la función.

#### En la Terminal de Linux:

1. **Compilar el Módulo**:

   - Al igual que en PowerShell, usa el compilador de Erlang para compilar el archivo:
     ```bash
     erlc hola_mundo.erl
     ```
   - Asegúrate de estar en el mismo directorio donde se encuentra `hola_mundo.erl`.

2. **Ejecutar la Función**:

   - Ejecuta el archivo compilado utilizando:
     ```bash
     erl -noshell -s hola_mundo saludar -s init stop
     ```
   - Este comando compilará y ejecutará el código, mostrando `Hola, Mundo!` en la terminal y luego saldrá de la VM de Erlang.


#### B. Uso de un Makefile en macOS, Linux

Usar un Makefile es útil en proyectos más grandes o cuando se trabaja con múltiples archivos. Para este ejemplo simple, es opcional, pero puede introducir a los estudiantes a prácticas más organizadas. Aquí tienes un ejemplo de un `Makefile` básico para Erlang:

- **Crear un archivo `Makefile`** con el siguiente contenido, recuerde que este script es muy "especial" con los espacios/tabuladores:

```makefile
# Define the Erlang compiler and interpreter commands
ERLC = erlc
ERL = erl

# Define the default task that will run both compile and run tasks
all: compile run

# Compile the Erlang source file into a .beam file
compile:
	$(ERLC) hola_mundo.erl

# Run the compiled Erlang program
run:
	$(ERL) -noshell -s hola_mundo saludar -s init stop

# Clean up: remove all .beam files
clean:
	rm -f *.beam

```


#### ¿Directo o con Makefile?

- **Para Principiantes Absolutos**: Usar la consola de Erlang directamente (`erl`) es más directo y ayuda a los estudiantes a familiarizarse con el entorno de Erlang rápidamente. Les permite ver cómo funcionan la compilación y la ejecución en tiempo real.

- **Para Estudiantes con Algo de Experiencia**: Introducir un Makefile puede ser beneficioso para aprender prácticas de desarrollo más organizadas, especialmente cuando el proyecto comienza a crecer y hay múltiples módulos.

-----


[![asciicast](https://asciinema.org/a/yyz9tEOuD9CkryizvgNserE9T.svg)](https://asciinema.org/a/yyz9tEOuD9CkryizvgNserE9T)



### Conclusión

Usar el ejemplo de "Hola Mundo" es una excelente manera de empezar a motivar a los estudiantes, ya que muestra lo simple y directo que puede ser trabajar con Erlang. A medida que avancen, comprenderán cómo estas bases se aplican a problemas más complejos y cómo las características avanzadas de Erlang, como la concurrencia y la tolerancia a fallos, les permitirán construir aplicaciones robustas y escalables.
