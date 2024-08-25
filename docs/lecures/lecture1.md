

![](https://github.com/gilbarbara/logos/blob/master/logos/erlang.svg)

### Erlang: Concurrencia y Programación Funcional

Erlang es conocido tanto por su paradigma de concurrencia como por su enfoque de programación funcional. Sin embargo, en la industria, suele ser más reconocido y valorado por sus capacidades de concurrencia y su modelo de programación distribuida, resiliente y tolerante a fallos. Aquí te detallo ambos aspectos y cómo se perciben en la práctica:

#### 1. Programación Funcional

Erlang pertenece a la familia de lenguajes de programación funcionales y, por lo tanto, comparte muchas características con otros lenguajes funcionales:

- **Inmutabilidad de datos**: En Erlang, los datos son inmutables por defecto, lo que significa que una vez asignado un valor a una variable, no puede ser cambiado. Esto elimina muchos errores comunes asociados con el estado mutable en otros lenguajes.

- **Funciones de primera clase**: Las funciones en Erlang son ciudadanos de primera clase, lo que significa que pueden ser pasadas como argumentos a otras funciones, devueltas como resultados y asignadas a variables.

- **Enfoque en expresiones**: Erlang se enfoca en evaluar expresiones y no en ejecutar instrucciones imperativas. Todo en Erlang retorna un valor, y se construyen programas mediante la composición de funciones.

#### 2. Concurrencia y Distribución

El aspecto más destacado de Erlang en la industria es su capacidad para manejar la concurrencia de manera efectiva. Erlang fue diseñado desde sus inicios para construir sistemas altamente concurrentes y distribuidos. Algunos de los puntos clave que resaltan este enfoque son:

- **Modelo de Actor**: Erlang utiliza el modelo de actor para la concurrencia, donde cada actor (o proceso) es completamente independiente, tiene su propio estado y se comunica con otros actores solo a través de mensajes. Esto simplifica la programación concurrente y evita los problemas comunes de sincronización de estado compartido.

- **Procesos Livianos**: Los procesos en Erlang son extremadamente livianos en comparación con los hilos de sistemas operativos convencionales. Esto permite crear cientos de miles de procesos sin que esto implique una carga significativa para el sistema.

- **Escalabilidad y Distribución**: Erlang permite construir sistemas distribuidos con facilidad. Los nodos de Erlang pueden comunicarse entre sí de manera transparente, lo que facilita la creación de aplicaciones distribuidas escalables.

- **Tolerancia a Fallos**: Uno de los lemas de Erlang es "let it crash" ("deja que falle"). Esto significa que los sistemas en Erlang están diseñados para manejar fallos de manera automática, reiniciando procesos fallidos sin afectar el sistema completo. Los supervisores y los árboles de supervisión son fundamentales en este enfoque.

### En la Industria: Concurrencia y Resiliencia

En el mundo real, Erlang se elige principalmente por su capacidad de manejar la concurrencia de manera eficiente y por su robustez en aplicaciones de telecomunicaciones, mensajería, sistemas de bases de datos distribuidas, y cualquier otra aplicación que requiera alta disponibilidad y baja latencia.

- **Telecomunicaciones**: Erlang fue originalmente desarrollado por Ericsson para sistemas de telecomunicaciones, donde es crucial manejar millones de llamadas telefónicas simultáneamente y con alta fiabilidad.

- **Mensajería y Sistemas de Chat**: Sistemas como WhatsApp utilizan Erlang precisamente porque puede manejar millones de conexiones simultáneas en tiempo real, manteniendo la fiabilidad y la baja latencia.

- **Bases de Datos Distribuidas**: Sistemas de bases de datos como Riak y CouchDB están construidos sobre Erlang debido a su capacidad para manejar la replicación, la consistencia eventual y la recuperación de fallos de manera efectiva.

