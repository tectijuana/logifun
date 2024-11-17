


### **SISTEMA EXPERTO: Reparacion de Laptops**

```prolog
% --- ASCII Art ---
ascii_art :-
    writeln("==============================================="),
    writeln("   __      __       _   _               "),
    writeln("   \\ \\    / /      | | (_)              "),
    writeln("    \\ \\  / /__  ___| |_ _ _ __   __ _   "),
    writeln("     \\ \\/ / _ \\/ __| __| | '_ \\ / _` |  "),
    writeln("      \\  /  __/\\__ \\ |_| | | | | (_| |  "),
    writeln("       \\/ \\___||___/\\__|_|_| |_|\\__, |  "),
    writeln("                                __/ |  "),
    writeln("                               |___/   "),
    writeln("==============================================="),
    writeln("Bienvenido al Sistema Experto de Reparación de Laptops").
    
% --- Menú principal ---
menu :-
    ascii_art,
    writeln("Seleccione una opción:"),
    writeln("1. Diagnosticar problemas de programas."),
    writeln("2. Diagnosticar problemas del disco."),
    writeln("3. Mostrar tareas de mantenimiento sugeridas."),
    writeln("4. Salir."),
    writeln("==============================================="),
    read(Opcion),
    manejar_opcion(Opcion).

% --- Manejo de opciones ---
manejar_opcion(1) :-
    diagnosticar_programa_interactivo,
    continuar.
manejar_opcion(2) :-
    diagnosticar_disco_interactivo,
    continuar.
manejar_opcion(3) :-
    mostrar_mantenimiento,
    continuar.
manejar_opcion(4) :-
    writeln("Gracias por usar el sistema experto. ¡Hasta pronto!").
manejar_opcion(_) :-
    writeln("Opción inválida. Por favor, seleccione una opción válida."),
    menu.

% --- Continuar después de una opción ---
continuar :-
    writeln("==============================================="),
    writeln("¿Desea regresar al menú principal? (si/no)"),
    read(Respuesta),
    (   Respuesta = si -> menu
    ;   writeln("Gracias por usar el sistema experto. ¡Hasta pronto!")
    ).

% --- Diagnóstico interactivo de programas ---
diagnosticar_programa_interactivo :-
    writeln("Ingrese el sistema operativo (windows, linux, mac_os): "),
    read(SistemaOperativo),
    writeln("Describa el problema relacionado con programas (por ejemplo, 'Programas no responden'): "),
    read(Problema),
    (   diagnosticar_programa(SistemaOperativo, Problema, Solucion)
    ->  format("Diagnóstico: ~w. Solución sugerida: ~w~n", [Problema, Solucion])
    ;   writeln("No se encontró una solución específica para este problema.")
    ).

% --- Diagnóstico interactivo de disco ---
diagnosticar_disco_interactivo :-
    writeln("Ingrese el sistema operativo (windows, linux, mac_os): "),
    read(SistemaOperativo),
    writeln("Describa el problema relacionado con el disco (por ejemplo, 'Errores en el disco duro'): "),
    read(Problema),
    (   diagnosticar_disco(SistemaOperativo, Problema, Solucion)
    ->  format("Diagnóstico: ~w. Solución sugerida: ~w~n", [Problema, Solucion])
    ;   writeln("No se encontró una solución específica para este problema.")
    ).

% --- Mostrar mantenimiento preventivo ---
mostrar_mantenimiento :-
    writeln("==============================================="),
    writeln("Tareas de mantenimiento sugeridas:"),
    forall(tarea_mantenimiento(Tarea), writeln(Tarea)),
    writeln("===============================================").

% --- Diagnósticos y reglas de conocimiento ---
diagnosticar_programa(SistemaOperativo, Problema, Solucion) :-
    problema(SistemaOperativo, _, Problema, Solucion).

diagnosticar_disco(SistemaOperativo, Problema, Solucion) :-
    problema(SistemaOperativo, _, Problema, Solucion).

% --- Tareas y problemas definidos ---
problema(windows, _, "Programas no responden", "Desinstalar programas no utilizados o problemáticos").
problema(linux, _, "Errores en programas instalados", "Revisar paquetes instalados y reinstalar los necesarios").
problema(mac_os, _, "Aplicaciones consumen demasiada memoria", "Cerrar aplicaciones en segundo plano o desinstalarlas si no son necesarias").
problema(windows, _, "Errores en el disco duro", "Ejecutar un Scan-disk para verificar y reparar errores").
problema(linux, _, "Sectores dañados en el disco", "Usar herramientas como fsck para reparar el sistema de archivos").
problema(mac_os, _, "Advertencias en el almacenamiento", "Usar la Utilidad de Discos para verificar y reparar errores").

tarea_mantenimiento("Realizar un escaneo de disco regularmente para prevenir errores de almacenamiento").
tarea_mantenimiento("Desinstalar programas que ya no usa para liberar espacio").
tarea_mantenimiento("Actualizar el sistema operativo y controladores periódicamente").
tarea_mantenimiento("Ejecutar herramientas de limpieza de disco para eliminar archivos temporales y basura").
```

---

### **Ejemplo de Ejecución**

#### **Inicio del Menú**
```prolog
?- menu.
===============================================
   __      __       _   _               
   \ \    / /      | | (_)              
    \ \  / /__  ___| |_ _ _ __   __ _   
     \ \/ / _ \/ __| __| | '_ \ / _` |  
      \  /  __/\__ \ |_| | | | | (_| |  
       \/ \___||___/\__|_|_| |_|\__, |  
                                __/ |  
                               |___/   
===============================================
Bienvenido al Sistema Experto de Reparación de Laptops
Seleccione una opción:
1. Diagnosticar problemas de programas.
2. Diagnosticar problemas del disco.
3. Mostrar tareas de mantenimiento sugeridas.
4. Salir.
===============================================
|: 1.
Ingrese el sistema operativo (windows, linux, mac_os): 
|: windows.
Describa el problema relacionado con programas (por ejemplo, 'Programas no responden'): 
|: "Programas no responden".
Diagnóstico: Programas no responden. Solución sugerida: Desinstalar programas no utilizados o problemáticos.
===============================================
¿Desea regresar al menú principal? (si/no)
|: si.
```

#### **Mostrar Tareas de Mantenimiento**
```prolog
?- menu.
...
|: 3.
===============================================
Tareas de mantenimiento sugeridas:
Realizar un escaneo de disco regularmente para prevenir errores de almacenamiento
Desinstalar programas que ya no usa para liberar espacio
Actualizar el sistema operativo y controladores periódicamente
Ejecutar herramientas de limpieza de disco para eliminar archivos temporales y basura
===============================================
¿Desea regresar al menú principal? (si/no)
|: no.
Gracias por usar el sistema experto. ¡Hasta pronto!
```

---

### **Características Destacadas**

1. **ASCII Art**:
   - Hace que el sistema sea más atractivo y memorable para los estudiantes.
   
2. **Menú Interactivo**:
   - Permite a los usuarios navegar fácilmente entre las opciones de diagnóstico y mantenimiento.

3. **Ampliación y Personalización**:
   - Los estudiantes pueden agregar más problemas, soluciones, y tareas de mantenimiento para extender el sistema.

