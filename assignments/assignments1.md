# Erlang como programación

El curso se divide en módulos, cada uno enfocado en un tema clave de Erlang:

1. **Introducción a Erlang**
   - Historia y filosofía de Erlang
   - Instalación y configuración del entorno
   - Primer programa: "Hello, World!"

2. **Sintaxis Básica de Erlang**
   - Variables y funciones
   - Patrones de coincidencia
   - Expresiones condicionales (`if`, `case`, `cond`)

3. **Procesos en Erlang**
   - Creación y terminación de procesos
   - Enviar y recibir mensajes
   - Concurrencia y paralelismo

4. **Comunicación entre Procesos**
   - Pasaje de mensajes
   - Tipos de mensajes y patrones de recepción
   - Manejo de tiempos de espera (`timeout`)

5. **Supervisión y Tolerancia a Fallos**
   - Modelos de supervisión
   - Uso de `link` y `monitor`
   - Árboles de supervisión y su importancia

6. **Aplicaciones OTP (Open Telecom Platform)**
   - Introducción a OTP
   - GenServer y GenEvent
   - Aplicaciones y configuraciones

7. **Persistencia y Manejo de Estado**
   - ETS (Erlang Term Storage)
   - Mnesia: base de datos distribuida de Erlang
   - Persistencia de procesos

8. **Balanceo de Carga y Distribución**
   - Estrategias de balanceo de carga
   - Diseño de sistemas distribuidos en Erlang
   - Escalabilidad y replicación de procesos

9. **Erlang en el Mundo Real**
   - Casos de uso en la industria
   - Despliegue de aplicaciones Erlang
   - Integración con otros lenguajes y tecnologías

## Lista de Proyectos

A continuación se presenta una lista de proyectos propuestos para el curso, junto con una breve explicación de cada uno.

| Proyecto                        | Descripción                                                                                         |
|---------------------------------|-----------------------------------------------------------------------------------------------------|
| **Project 1: Hello, World!**    | Proyecto introductorio donde los estudiantes configuran su entorno de desarrollo y escriben su primer programa en Erlang. |
| **Project 2: Chat Server**      | Implementación de un simple servidor de chat que permite a múltiples clientes conectarse y enviar mensajes entre ellos utilizando procesos de Erlang. |
| **Project 3: Calculator**       | Crear una calculadora distribuida donde los cálculos se realizan en diferentes nodos de Erlang, mostrando la capacidad de distribución de Erlang. |
| **Project 4: Todo List**        | Aplicación de gestión de tareas que permite agregar, eliminar y listar tareas, usando GenServer para manejar el estado de las tareas. |
| **Project 5: Load Balancer**    | Implementar un balanceador de carga que distribuya las tareas entre múltiples procesos trabajadores, aplicando la estrategia round-robin. |
| **Project 6: Fault Tolerant System** | Desarrollo de un sistema de supervisión que detecte fallos en procesos y los reinicie, utilizando árboles de supervisión para mejorar la tolerancia a fallos. |
| **Project 7: Key-Value Store**  | Implementación de un almacén de datos simple de tipo clave-valor utilizando ETS y explorando cómo almacenar y recuperar datos de manera eficiente. |
| **Project 8: Distributed Chat** | Extensión del proyecto de chat para funcionar en un entorno distribuido, permitiendo que usuarios conectados a diferentes nodos se comuniquen. |
| **Project 9: Mnesia Database**  | Proyecto para construir una base de datos distribuida usando Mnesia, donde los estudiantes pueden crear, leer, actualizar y borrar registros. |
| **Project 10: Web Server**      | Implementar un servidor web simple en Erlang que maneje solicitudes HTTP, demostrando cómo construir aplicaciones web básicas en Erlang. |

