
![Screenshot 2024-11-16 at 11 08 49 p m](https://github.com/user-attachments/assets/4f3cd07b-0f3b-4fae-844a-23c82c3514d5)

---

###  "Lado Oscuro vs. Lado Luminoso, de StarWars"**

#### **Descripción**
Creamos un sistema en **Prolog** para modelar el universo de *Star Wars*. Los estudiantes programarán relaciones entre personajes, habilidades, facciones, y decisiones que afecten si un personaje se une al Lado Oscuro o al Lado Luminoso de la Fuerza.

El objetivo será:
1. Modelar personajes con atributos como poder en la Fuerza, habilidades, y lealtades.
2. Simular interacciones y conflictos entre facciones (Jedi vs. Sith).
3. Evaluar si un personaje cambia de facción basado en sus decisiones y eventos.

---

### **Práctica en Prolog**

#### **1. Base de Conocimiento: Personajes y Facciones**

Definimos los personajes, sus habilidades y sus facciones iniciales.

```prolog
% --- Base de Conocimiento ---

% Personajes: nombre, facción inicial, poder en la Fuerza.
personaje(luke, jedi, 85).
personaje(darth_vader, sith, 95).
personaje(yoda, jedi, 100).
personaje(obi_wan, jedi, 90).
personaje(darth_maul, sith, 80).
personaje(anakin, jedi, 88).

% Habilidades: personaje, lista de habilidades.
habilidades(luke, [espada_laser, meditar, pilotar]).
habilidades(darth_vader, [estrategia, espada_laser, estrangular]).
habilidades(yoda, [sabiduria, meditar, telequinesis]).
habilidades(obi_wan, [espada_laser, diplomacia, pilotar]).
habilidades(darth_maul, [espada_laser, agilidad, combate]).
habilidades(anakin, [pilotaje, espada_laser, reparar]).

% Eventos clave que pueden cambiar la facción de un personaje.
evento(anakin, "traiciona a los Jedi", sith).
evento(luke, "resiste al Emperador", jedi).
evento(darth_vader, "redime por amor", jedi).
```

---

#### **2. Reglas: Cambios de Facción**

Modelamos las reglas que determinan si un personaje cambia de facción.

```prolog
% Cambio de facción basado en eventos clave
cambiar_faccion(Personaje, NuevaFaccion) :-
    evento(Personaje, Evento, NuevaFaccion),
    format("~w experimenta el evento: '~w' y ahora pertenece al lado ~w.~n", 
           [Personaje, Evento, NuevaFaccion]).
```

---

#### **3. Regla de Compatibilidad en Combate**

Definimos qué tan fuerte es un personaje en combate comparado con otro.

```prolog
% Determinar compatibilidad en combate basada en el poder y habilidades.
combate(Personaje1, Personaje2, Ganador) :-
    personaje(Personaje1, _, Poder1),
    personaje(Personaje2, _, Poder2),
    habilidades(Personaje1, Habs1),
    habilidades(Personaje2, Habs2),
    interseccion(Habs1, Habs2, HabsComunes),
    length(HabsComunes, PuntosExtra),
    Total1 is Poder1 + PuntosExtra,
    Total2 is Poder2 + PuntosExtra,
    (   Total1 > Total2
    ->  Ganador = Personaje1
    ;   Ganador = Personaje2 ),
    format("En el combate entre ~w y ~w, gana ~w.~n", [Personaje1, Personaje2, Ganador]).

% Obtener la intersección de habilidades.
interseccion([], _, []).
interseccion([H|T], Lista, [H|Interseccion]) :-
    member(H, Lista),
    interseccion(T, Lista, Interseccion).
interseccion([_|T], Lista, Interseccion) :-
    interseccion(T, Lista, Interseccion).
```

---

#### **4. Regla: Simulación de Conversión al Lado Oscuro**

Simulamos si un personaje es tentado al Lado Oscuro.

```prolog
% Tentación al Lado Oscuro basada en poder y eventos clave.
tentacion_lado_oscuro(Personaje) :-
    personaje(Personaje, jedi, Poder),
    Poder > 80, % Los más poderosos son tentados
    evento(Personaje, Evento, sith),
    format("~w es tentado por el Lado Oscuro debido al evento: ~w.~n", [Personaje, Evento]).
```

---

#### **5. Predicados Interactivos**

Estos predicados permiten a los estudiantes probar el sistema.

```prolog
% Simular un combate entre dos personajes.
simular_combate(Personaje1, Personaje2) :-
    combate(Personaje1, Personaje2, Ganador),
    format("Resultado: ~w gana el combate.~n", [Ganador]).

% Simular el destino de un personaje.
simular_destino(Personaje) :-
    (   tentacion_lado_oscuro(Personaje)
    ->  cambiar_faccion(Personaje, sith)
    ;   format("~w se mantiene en el lado Luminoso.~n", [Personaje])
    ).
```

---

### **Ejemplo de Ejecución**

#### **Consulta: ¿Quién gana en un combate?**
```prolog
?- simular_combate(luke, darth_vader).
En el combate entre luke y darth_vader, gana darth_vader.
Resultado: darth_vader gana el combate.
```

#### **Consulta: ¿Anakin será tentado al Lado Oscuro?**
```prolog
?- simular_destino(anakin).
anakin es tentado por el Lado Oscuro debido al evento: traiciona a los Jedi.
anakin experimenta el evento: 'traiciona a los Jedi' y ahora pertenece al lado sith.
```

#### **Consulta: Mostrar todas las tentaciones al Lado Oscuro**
```prolog
?- tentacion_lado_oscuro(Personaje).
anakin es tentado por el Lado Oscuro debido al evento: traiciona a los Jedi.
Personaje = anakin ;
false.
```

---

### **Sugerencias de Extensión**

1. **Ampliar la base de datos**:
   - Agrega más personajes del universo de Star Wars, como Rey, Kylo Ren, o el Emperador Palpatine.
   - Introduce más habilidades y eventos clave.

2. **Añadir reglas complejas**:
   - Modelar alianzas entre personajes (por ejemplo, Maestros Jedi y sus Padawan).
   - Agregar habilidades especiales que influyan en combates.

3. **Construir misiones**:
   - Diseña predicados para simular aventuras, como atacar la Estrella de la Muerte o infiltrarse en una base Sith.

4. **Integración visual**:
   - Usa otro lenguaje como **Python** o **C#** para construir una interfaz gráfica que interactúe con el modelo de Prolog.

---

Este enfoque introduce a los estudiantes al modelado lógico mientras exploran el universo de Star Wars, fomentando la creatividad y la diversión mientras trabajan con conceptos avanzados como reglas, bases de conocimiento y simulaciones en Prolog. ¡Que la Fuerza los acompañe! 😊
