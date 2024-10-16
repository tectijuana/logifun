
![cooltext468231870622420](https://github.com/user-attachments/assets/b46909b8-eca3-4bcf-9403-c590150c4530)





Para realizar prácticas de laboratorio en **Prolog** en un entorno de **Ubuntu LTS** en **AWS Academy**, aquí tienes un conjunto de recomendaciones que te ayudarán a configurar y optimizar tu entorno de desarrollo:

### 1. **Lanzar una instancia de Ubuntu LTS**
   - En **AWS Academy**, accede al servicio **EC2** para lanzar una nueva instancia.
   - Selecciona una imagen de máquina (AMI) de **Ubuntu LTS** (por ejemplo, **Ubuntu 22.04 LTS** o **Ubuntu 20.04 LTS**).
   - Configura las especificaciones de la instancia según los requisitos de tu laboratorio (una t2.micro es suficiente para trabajos ligeros).
   - Asegúrate de habilitar la conexión por SSH al abrir el puerto 22 en el **grupo de seguridad**.

### 2. **Conectar por SSH**
   - Desde tu terminal local o utilizando un cliente SSH (como PuTTY en Windows), conéctate a la instancia:

   ```bash
   ssh -i ruta/a/tu/clave.pem ubuntu@<tu_ip_publica>
   ```
   - O desde el panel de control de la instancia, dar botón derecho y "conectar" para el SSH webview.
     
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
--- 

#### 7. Un Ejemplo Práctico

Ahora que hemos revisado los conceptos básicos de Prolog, es momento de poner manos a la obra y trabajar con un archivo de Prolog. Para este ejercicio, vamos a crear un archivo llamado `family.pl`, que contendrá una serie de hechos y reglas relacionadas con las relaciones familiares.

##### 8. Creación del archivo `family.pl`

Usa el editor **Emacs** para crear un archivo llamado `family.pl`. A continuación, escribe el siguiente código en el archivo. Es importante que respetes la sintaxis de Prolog, prestando atención a los puntos (.) y las comas (,).

```prolog
male(harry).
female(liz).

parent(phil, chas).
parent(liz, chas).
parent(chas, harry).
parent(chas, wills).

grandmother(GM, C):-  
    mother(GM, P),  
    parent(P, C).

mother(M,C):-  
    female(M),  
    parent(M, C).
```

##### 9. Guardar el archivo

Antes de realizar cualquier consulta en Prolog, es necesario **guardar** el archivo. En Emacs, puedes hacer esto seleccionando `Save` en el menú de `File` o usando el atajo `Ctrl-x Ctrl-s`.

##### 10. Consultar el archivo en Prolog

Una vez que hayas guardado el archivo, es momento de decirle a Prolog que lo consulte. Existen dos formas principales de hacer esto:

1. **Desde la ventana de SWI-Prolog**, escribe alguna de las siguientes instrucciones:
   ```prolog
   | ?- consult(family).
   ```
   O bien:
   ```prolog
   | ?- [family].
   ```

   **Nota**: No es necesario incluir la extensión `.pl` en el nombre del archivo al consultarlo.

2. **Desde Emacs**, coloca el cursor en la ventana del archivo `family.pl` y usa el atajo `Ctrl-c Ctrl-b` para "consultar el buffer" en Prolog.

Si no hay errores en el archivo, Prolog te indicará que el archivo ha sido consultado correctamente y estará listo para realizar consultas.

---

#### 11. Poner Objetivos en Prolog

Para hacer que Prolog realice alguna operación, debes emitir una **consulta**. Por ejemplo, puedes pedirle a Prolog que encuentre quién es nieto de `liz` con la siguiente consulta:

```prolog
| ?- grandmother(liz, Who).
```

Prolog te responderá:

```prolog
Who = harry
```

Luego, puedes escribir un punto y coma `;` para continuar con la siguiente solución:

```prolog
Who = wills
```

Si escribes otro `;`, Prolog responderá `no`, indicando que no hay más soluciones.

---
![Cool Text - Depuracin con trace 468402331397233](https://github.com/user-attachments/assets/f5198957-af1e-49ad-921e-756244e01ff0)



#### 12. Depuración con `trace`

Cuando estés depurando tu código o quieras ver paso a paso cómo Prolog está evaluando las consultas, puedes usar el comando `trace`. Esto es útil para encontrar errores o entender el flujo de ejecución en tus programas.

1. Activa el modo de traza escribiendo:

   ```prolog
   | ?- trace.
   ```

2. Luego, realiza la consulta:

   ```prolog
   | ?- grandmother(liz, Who).
   ```

Prolog mostrará cada uno de los pasos que sigue para resolver la consulta, incluyendo llamadas, salidas y éxitos:

```prolog
Call: (6) grandmother(liz, _G396) ? creep  
Call: (7) mother(liz, _G450) ? creep  
...
```

Para desactivar la traza, usa el comando:

```prolog
| ?- nodebug.
```

---

#### 13. Notas sobre Nombres de Archivos y Directorios en Prolog

Si organizas tus archivos de Prolog en subdirectorios, debes especificar el **ruta completa** al consultar archivos.

Por ejemplo, si tienes un archivo `family.pl` en un directorio llamado `myprolog`, debes consultar el archivo de la siguiente manera:

```prolog
| ?- consult('myprolog/family.pl').
```

Recuerda incluir comillas simples y la extensión `.pl` al trabajar con rutas.

---

### 14. **Recursos adicionales**
   - **Documentación de SWI-Prolog**: Para aprender más sobre Prolog y SWI-Prolog, consulta la documentación oficial en [SWI-Prolog Documentation](https://www.swi-prolog.org/documentation.html).
   - **Tareas automatizadas**: Si planeas ejecutar scripts de Prolog como parte de un flujo de trabajo automatizado, puedes usar **crontab** para programar tareas o crear scripts bash que ejecuten tus programas.

### 15. **Terminar la instancia EC2**
   Una vez que termines tu laboratorio, recuerda detener o finalizar la instancia EC2 para evitar costos adicionales.

   ```bash
   sudo shutdown now
   ```

Con estas recomendaciones podrás configurar un entorno de laboratorio funcional en Prolog dentro de una instancia de Ubuntu LTS en AWS. ¡Buena suerte con tus prácticas!
