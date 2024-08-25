## Proyecto 9: Mnesia Database

En este proyecto, los estudiantes construirán una base de datos distribuida utilizando Mnesia. Este proyecto les permitirá crear, leer, actualizar y borrar registros, proporcionando una comprensión práctica de cómo funciona Mnesia y cómo puede ser utilizado en aplicaciones distribuidas. Los estudiantes aprenderán a configurar la replicación de datos entre nodos, manejar la persistencia en memoria y disco, y explorarán cómo Mnesia puede ser integrada con otros componentes de un sistema basado en Erlang.



# Uso de Mnesia en el Mundo Real

Mnesia se utiliza en aplicaciones del mundo real, aunque su adopción suele estar limitada a casos específicos donde sus características únicas proporcionan claras ventajas. A continuación, exploraremos cómo y cuándo utilizar Mnesia en aplicaciones prácticas, así como sus principales ventajas y limitaciones.

Fuente:

  https://www.erlang.org/doc/apps/mnesia/mnesia.html


## ¿Qué es Mnesia?

Mnesia es un sistema de gestión de bases de datos distribuido incluido en Erlang/OTP. Está diseñado para aplicaciones que necesitan:

- **Alta disponibilidad**: Mnesia admite replicación automática y puede operar en modo distribuido, lo que permite que los datos se almacenen en múltiples nodos. Esto asegura que el sistema continúe funcionando incluso si uno o más nodos fallan.

- **Bajo tiempo de respuesta**: Mnesia ofrece acceso rápido a los datos, esencial para aplicaciones que requieren respuestas en tiempo real.

- **Integración con Erlang**: Mnesia se integra perfectamente con Erlang, utilizando el mismo modelo de concurrencia basado en procesos y mensajes. Esto facilita el desarrollo de aplicaciones que requieren tanto procesamiento concurrente como acceso a datos.

## Casos de Uso de Mnesia en el Mundo Real

Mnesia es particularmente útil en los siguientes escenarios:

- **Datos distribuidos y replicación**: Mnesia facilita la creación de bases de datos distribuidas con replicación automática entre nodos, ideal para sistemas que necesitan alta disponibilidad y consistencia, como aplicaciones de telecomunicaciones y sistemas de mensajería.

- **Datos de configuración y estado de corta duración**: Es excelente para almacenar datos de configuración o estados temporales que deben ser compartidos entre diferentes nodos, especialmente cuando la rapidez es crucial y los datos no son extensos.

- **Sistemas embebidos**: En entornos embebidos donde se utiliza Erlang, Mnesia puede funcionar como un almacén de datos ligero y eficiente para las aplicaciones.

- **Aplicaciones de telecomunicaciones**: Originalmente diseñado por Ericsson, Mnesia sigue siendo relevante en sistemas de telecomunicaciones, como gestión de llamadas y servidores de mensajes, donde la alta disponibilidad y la respuesta rápida son esenciales.

## Ejemplos de Uso Real de Mnesia

- **Control de Llamadas y Telecomunicaciones**: Mnesia se ha empleado en centrales telefónicas para gestionar la información de control de llamadas, listas de usuarios y configuraciones de red en tiempo real. Su capacidad para replicar y mantener el estado en múltiples nodos lo hace ideal para estos escenarios.

- **Aplicaciones de Mensajería**: Algunos sistemas de mensajería utilizan Mnesia para gestionar datos de sesión de usuarios y otros metadatos en tiempo real, beneficiándose de su capacidad para operar en entornos distribuidos.

- **Almacenamiento Temporal en Sistemas Distribuidos**: En aplicaciones que necesitan almacenamiento rápido y temporal, donde los datos no requieren persistencia a largo plazo, Mnesia es una opción adecuada.

## Ventajas de Usar Mnesia

- **Integración nativa con Erlang**: Permite escribir código de acceso a bases de datos usando los mismos paradigmas y herramientas que el código de la aplicación, lo que simplifica el desarrollo.

- **Soporte para datos distribuidos y replicación**: Facilita la construcción de aplicaciones de alta disponibilidad sin necesidad de sistemas de replicación externos.

- **Capacidad de funcionar en memoria o en disco**: Mnesia puede configurarse para almacenar datos solo en memoria para velocidad, o en disco para persistencia.

## Limitaciones de Mnesia

- **Escalabilidad**: Aunque es excelente para bases de datos distribuidas de tamaño moderado, Mnesia no está diseñado para escalar a miles de nodos o manejar petabytes de datos. Su capacidad de escalabilidad horizontal es limitada comparada con sistemas modernos de bases de datos distribuidas como Cassandra o CockroachDB.

- **Capacidades de consulta limitadas**: Mnesia no soporta consultas SQL avanzadas. Si la aplicación requiere consultas complejas, Mnesia puede no ser la opción ideal.

- **Falta de herramientas de administración**: A diferencia de las bases de datos comerciales, Mnesia carece de un ecosistema amplio de herramientas de administración, monitoreo y optimización.




