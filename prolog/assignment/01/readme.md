





Para realizar prácticas de laboratorio en **Prolog** en un entorno de **Ubuntu LTS** en **AWS Academy**, aquí tienes un conjunto de recomendaciones que te ayudarán a configurar y optimizar tu entorno de desarrollo:

### 1. **Lanzar una instancia de Ubuntu LTS**
   - En **AWS Academy**, accede al servicio **EC2** para lanzar una nueva instancia.
   - Selecciona una imagen de máquina (AMI) de **Ubuntu LTS** (por ejemplo, **Ubuntu 22.04 LTS** o **Ubuntu 20.04 LTS**).
   - Configura las especificaciones de la instancia según los requisitos de tu laboratorio (una t2.micro es suficiente para trabajos ligeros).
   - Asegúrate de habilitar la conexión por SSH al abrir el puerto 22 en el **grupo de seguridad**.

### 2. **Conectar por SSH**
   Desde tu terminal local o utilizando un cliente SSH (como PuTTY en Windows), conéctate a la instancia:

   ```bash
   ssh -i ruta/a/tu/clave.pem ubuntu@<tu_ip_publica>
   ```

### 3. **Instalar SWI-Prolog**
   Una vez conectado a la instancia, instala **SWI-Prolog**, que es una de las implementaciones más populares de Prolog:

   ```bash
   sudo apt update
   sudo apt install swi-prolog
   ```

   Verifica la instalación comprobando la versión de Prolog:

   ```bash
   swipl --version
   ```

   Esto debería mostrarte la versión instalada de SWI-Prolog.

### 4. **Configurar el entorno**
   Asegúrate de tener configurado tu entorno de desarrollo. Algunas configuraciones opcionales que podrían ser útiles:

   - **Editor de texto**: Puedes instalar un editor como **Vim** o **Nano** para trabajar en la terminal. Si prefieres un entorno gráfico como **VS Code**, tendrás que configurar un puerto adicional o un túnel SSH para conectarte desde tu máquina local.

     - Instalar **Vim**:
       ```bash
       sudo apt install vim
       ```
     - Instalar **Nano**:
       ```bash
       sudo apt install nano
       ```

   - **Extensión de Prolog para VS Code**: Si decides usar VS Code en tu máquina local, puedes instalar la extensión de SWI-Prolog para mejorar tu experiencia de codificación con resaltado de sintaxis y otros beneficios.

### 5. **Escribir y ejecutar código Prolog**
   Puedes crear archivos `.pl` y ejecutarlos directamente en tu instancia. Por ejemplo:

   1. Crear un archivo **`mi_programa.pl`**:

      ```bash
      nano mi_programa.pl
      ```

      Luego, escribe un código básico como:

      ```prolog
      % mi_programa.pl
      saludo :- write('Hola, Prolog en AWS!'), nl.
      ```

   2. Ejecutar el archivo en SWI-Prolog:

      ```bash
      swipl mi_programa.pl
      ```

      Luego, en el shell de Prolog, puedes ejecutar la consulta:

      ```prolog
      ?- saludo.
      ```

### 6. **Guardar y recuperar trabajos en AWS**
   Asegúrate de que cualquier código o proyecto que crees esté respaldado. Puedes usar herramientas como **Git** para mantener el control de versiones o simplemente descargar archivos a tu máquina local.

   - Instalar **Git**:
     ```bash
     sudo apt install git
     ```

   - Clonar un repositorio:
     ```bash
     git clone https://github.com/tu_usuario/tu_repositorio.git
     ```

   - Subir tus cambios:
     ```bash
     git add .
     git commit -m "Subiendo cambios"
     git push origin main
     ```

### 7. **Recursos adicionales**
   - **Documentación de SWI-Prolog**: Para aprender más sobre Prolog y SWI-Prolog, consulta la documentación oficial en [SWI-Prolog Documentation](https://www.swi-prolog.org/documentation.html).
   - **Tareas automatizadas**: Si planeas ejecutar scripts de Prolog como parte de un flujo de trabajo automatizado, puedes usar **crontab** para programar tareas o crear scripts bash que ejecuten tus programas.

### 8. **Terminar la instancia EC2**
   Una vez que termines tu laboratorio, recuerda detener o finalizar la instancia EC2 para evitar costos adicionales.

   ```bash
   sudo shutdown now
   ```

Con estas recomendaciones podrás configurar un entorno de laboratorio funcional en Prolog dentro de una instancia de Ubuntu LTS en AWS. ¡Buena suerte con tus prácticas!
