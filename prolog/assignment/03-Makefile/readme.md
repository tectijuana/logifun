# Proyecto de corrida de Programas en Prolog

Esta simulación de repositorio supongamos contiene 10 programas escritos en Prolog. A continuación, se proporcionan instrucciones sobre cómo clonar el repositorio, construir y ejecutar cada programa individualmente utilizando un Makefile y CMake.

## Prerrequisitos

Antes de comenzar, asegúrese de tener instalados los siguientes componentes:

- **Git**: para clonar el repositorio.
- **CMake**: para generar los archivos de construcción.
- **SWI-Prolog**: o cualquier otra distribución de Prolog para ejecutar los programas.

## Clonar el repositorio

Clona el repositorio usando el siguiente comando:

```bash
git clone https://github.com/usuario/proyecto-prolog.git
cd proyecto-prolog
```

## Construir el proyecto

Crea un directorio de construcción y genera los archivos necesarios utilizando CMake:

```bash
mkdir build
cd build
cmake ..
```

Luego, ejecuta el Makefile:

```bash
make
```

## Ejecutar los programas

Los programas están ubicados en el directorio `src` y están nombrados como `programa1.pl`, `programa2.pl`, ..., hasta `programa10.pl`.

Para ejecutar los programas uno por uno, puedes utilizar el Makefile con las siguientes reglas:

```bash
make run_programa1
make run_programa2
...
make run_programa10
```

Alternativamente, para ejecutar todos los programas secuencialmente:

```bash
make run_all
```

## Uso del Makefile

El Makefile está diseñado para simplificar la ejecución de los programas Prolog. Contiene reglas específicas para cada programa y una regla para ejecutarlos todos.

### Ejemplos de reglas en el Makefile:

```makefile
.PHONY: all run_all clean

all: run_all

run_all: run_programa1 run_programa2 run_programa3 run_programa4 run_programa5 run_programa6 run_programa7 run_programa8 run_programa9 run_programa10

run_programa1:
	swipl -s ../src/programa1.pl -g main -t halt

run_programa2:
	swipl -s ../src/programa2.pl -g main -t halt

# ... Repite para los demás programas hasta run_programa10

clean:
	@echo "Limpiando archivos generados..."
	# Añade comandos de limpieza si es necesario
```

## Sugerencias

- **Estructura del Programa**: Asegúrate de que cada programa Prolog tenga un predicado `main/0` que actúe como punto de entrada.
- **Modificaciones Personalizadas**: Si necesitas cambiar cómo se ejecuta un programa específico, puedes editar la regla correspondiente en el Makefile.
- **Mensajes de Salida**: Puedes agregar mensajes de salida en el Makefile para una mejor legibilidad durante la ejecución.

## Notas Adicionales

- **Compatibilidad**: Estos pasos asumen que estás trabajando en un entorno Unix-like. Los comandos pueden variar en Windows.
- **Ayuda**: Si necesitas ayuda adicional con Prolog o CMake, consulta la documentación oficial o busca recursos en línea.

---

¡Disfruta explorando los programas en Prolog!
