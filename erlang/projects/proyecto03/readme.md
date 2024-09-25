
![Erlang Cowboy Framework](https://github.com/user-attachments/assets/d864fc88-d823-4451-808d-67daa52b13a3)

# Tarea: Explorando el Framework Cowboy en Erlang

## **Objetivos de Aprendizaje**

- **Comprender** los fundamentos de Erlang y su modelo de concurrencia.
- **Conocer** las características y ventajas del framework Cowboy.
- **Aplicar** conceptos de programación funcional para desarrollar aplicaciones concurrentes y escalables.
- **Desarrollar** una aplicación web básica utilizando Cowboy.
- **Analizar** casos de uso industriales donde Cowboy es una solución efectiva.

---

## **Descripción de la Tarea**

En esta tarea, explorarás el framework **Cowboy** dentro del ecosistema de **Erlang**. Aprenderás sobre sus características principales, su uso en la industria y desarrollarás una aplicación web simple para aplicar los conceptos aprendidos.

---

## **Instrucciones**

### **Parte 1: Investigación Teórica**

1. **Lectura y Comprensión:**
   - Revisa los materiales proporcionados sobre Erlang y Cowboy.
   - Consulta los recursos adicionales recomendados para profundizar en el tema.

2. **Preguntas Teóricas:**
   - Responde las siguientes preguntas en un documento PDF o Word.

### **Parte 2: Desarrollo Práctico**

1. **Configuración del Entorno en AWS o Local:**
   - Instala Erlang en tu máquina si no lo tienes ya instalado. [Guía de Instalación de Erlang](https://www.erlang.org/downloads)
   - Instala Cowboy siguiendo la documentación oficial. [Documentación de Cowboy](https://ninenines.eu/docs/en/cowboy)

2. **Desarrollo de una Aplicación Web Básica:**
   - Crea un servidor HTTP simple utilizando Cowboy que responda con "¡Hola, Mundo!" a las solicitudes GET en la ruta raíz (`/`). Puede amplificarlo es bienvenida los cambios
   - Implementa una segunda ruta (`/saludo`) que reciba un parámetro de nombre y responda con un saludo personalizado. Por ejemplo, una solicitud a `/saludo?nombre=Juan` debería responder con "¡Hola, Juan!".

3. **Pruebas:**
   - Realiza pruebas utilizando un navegador web o herramientas como `curl` para verificar que las rutas funcionan correctamente.

4. **Documentación:**
   - Aciinema de la instalacio, pruebas y ejecucion
   - LOOM.com para la visualizacion (tambien puede usar CURL command en Linux)
   - Documenta el proceso de desarrollo, incluyendo fragmentos de código relevantes y capturas de pantalla de las pruebas exitosas.

---

## **Preguntas Teóricas**

1. **Fundamentos de Erlang:**
   - ¿Qué características de Erlang lo hacen adecuado para aplicaciones de alta concurrencia y escalabilidad?
   - Explica el modelo de actores en Erlang y cómo se aplica en la gestión de procesos concurrentes.

2. **Características de Cowboy:**
   - ¿Cuáles son las principales ventajas de usar Cowboy como servidor HTTP en aplicaciones Erlang?
   - Describe cómo Cowboy maneja las conexiones concurrentes de manera eficiente.

3. **Uso en la Industria:**
   - Menciona tres empresas que utilizan Erlang/Cowboy en su infraestructura y explica brevemente cómo lo emplean.
   - ¿En qué tipos de aplicaciones es menos común utilizar Cowboy y por qué?

4. **Integración con Otros Frameworks:**
   - ¿Cómo se integra Cowboy con el framework Phoenix en Elixir?
   - Explica la relación entre Phoenix Channels y Cowboy en el manejo de WebSockets.

5. **Desafíos y Consideraciones:**
   - ¿Cuáles son los principales desafíos al aprender y utilizar Cowboy para nuevos desarrolladores?
   - Discute cómo la tolerancia a fallos de Erlang beneficia a las aplicaciones desarrolladas con Cowboy.

---

## **Ejercicios Prácticos Adicionales (Opcionales)**

1. **Implementación de WebSockets:**
   - Extiende tu aplicación web básica para soportar WebSockets utilizando Cowboy.
   - Crea una funcionalidad de chat simple donde múltiples usuarios puedan enviar y recibir mensajes en tiempo real.

2. **Despliegue de la Aplicación:**
   - Investiga y describe los pasos necesarios para desplegar tu aplicación Cowboy en un servidor de producción.
   - Considera aspectos como la configuración de puertos, seguridad y supervisión de procesos.

---

## **Entrega de la Tarea**

- **Formato de Entrega:**
  - Documento con respuestas a las preguntas teóricas en repositorio personal.
  - Código fuente de la aplicación desarrollada (preferiblemente en un repositorio GitHub privado o publico).
  - Capturas de pantalla y documentación del desarrollo y pruebas.

- **Fecha de Entrega:**

<img width="284" alt="Screenshot 2024-09-24 at 2 21 04 p m" src="https://github.com/user-attachments/assets/4a3913e0-083f-462c-9940-ad223d3f595d">

  

- **Cómo Entregar:**
  - Sube enlace de repo o gist, requeridos a la plataforma Idoceo en la sección correspondiente de la tarea.
  - Asciinema de la elaboración, y posible loom.com para grabar pantalla del cliente ejecutandose, recuerde poner nombre en las animaciones, grabaciones al inicio, junto con objetivo.
  - Asegúrate de que el repositorio de código esté accesible si utilizas GitHub u otra plataforma de control de versiones.

---

## **Criterios de Evaluación**

- **Comprensión Teórica:** Claridad y profundidad en las respuestas a las preguntas teóricas.
- **Funcionalidad del Código:** La aplicación web debe funcionar según las especificaciones dadas.
- **Calidad del Código:** Código limpio, bien estructurado y comentado.
- **Documentación:** Explicación detallada del proceso de desarrollo y pruebas realizadas. Asciinema de los eventos antes mencionados.
- **Creatividad (Opcional):** Implementación de funcionalidades adicionales que demuestren un entendimiento más profundo de Cowboy y Erlang.

---

## **Recursos Adicionales**

- **Documentación Oficial de Cowboy:** [ninenines.eu/docs/en/cowboy](https://ninenines.eu/docs/en/cowboy)
- **TUTORIAL elaboracion de un dashboard Er, Cowboy IM (2024)**  https://medium.com/@william.echenim/from-concept-to-creation-building-real-time-dashboards-with-erlang-cowboy-and-mongooseim-68f8651a3449
- **"Programming Erlang" por Joe Armstrong:** Libro fundamental para entender Erlang y su ecosistema.
- **Tutoriales de Elixir y Phoenix:** Para explorar cómo Cowboy se utiliza en frameworks modernos.
- **Guía de Inicio Rápido de Cowboy:** [Quick Start Guide](https://ninenines.eu/docs/en/cowboy/2.8/guide/quickstart/)

---

## **Consejos para el Éxito**

- **Planificación:** Divide la tarea en partes manejables y establece un cronograma para completarlas.
- **Consulta Recursos:** No dudes en utilizar la documentación oficial y recursos en línea para resolver dudas.
- **Colaboración:** Si es permitido, trabaja con compañeros para discutir conceptos y resolver problemas comunes.
- **Prueba Continuamente:** Realiza pruebas frecuentes durante el desarrollo para asegurar que cada funcionalidad opere correctamente.
- **Documenta tu Trabajo:** Mantén un registro detallado de los pasos que sigues y los desafíos que enfrentas.

---

¡Buena suerte y disfruta explorando el poderoso framework Cowboy en el mundo de Erlang!
