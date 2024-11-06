
![Base de Datos](https://github.com/user-attachments/assets/65cf07e5-906f-4fc9-a30b-737a5fba38ae)


### Películas Famosas para ejercios de Prolog

**Autor**: Rene Solis  
**Título**: Base de datos de películas (prolog) y su conversion SQL gestor básico.

---

#### Descripción

Este programa en Prolog permite consultar una base de datos de películas famosas, proporcionando información sobre el título, año de estreno, director, actores y actrices. Además, se incluyen reglas para realizar consultas más complejas sobre los datos, como la búsqueda de películas estrenadas antes o después de un año en específico o la identificación de actores que han trabajado juntos.

#### Instrucciones de Consulta

Para realizar consultas en esta base de datos, puedes utilizar los siguientes ejemplos como guía:

1. **Consultar el año de lanzamiento de una película:**
   ```prolog
   ?- movie(american_beauty, Y).
   ```
   Resultado esperado: `Y = 1999`.

2. **Encontrar todas las películas lanzadas en un año específico:**
   ```prolog
   ?- movie(M, 2000).
   ```

3. **Listar las películas estrenadas antes del año 2000:**
   ```prolog
   ?- movie(M, Y), Y < 2000.
   ```

4. **Buscar películas estrenadas después de 1990:**
   ```prolog
   ?- movie(M, Y), Y > 1990.
   ```

5. **Encontrar un actor que haya aparecido en más de una película:**
   ```prolog
   ?- actor(M1, A, _), actor(M2, A, _), M1 \= M2.
   ```

6. **Encontrar un director de una película en la que haya aparecido Scarlett Johansson:**
   ```prolog
   ?- actress(M, scarlett_johansson, _), director(M, D).
   ```

---

### Base de Datos

La base de datos se estructura de la siguiente manera:

- **movie(M, Y)**: `M` es el título de la película y `Y` es el año de lanzamiento.
- **director(M, D)**: `M` es la película y `D` es el director.
- **actor(M, A, R)**: `M` es la película, `A` es el nombre del actor y `R` es el personaje que interpreta.
- **actress(M, A, R)**: Similar a `actor/3` pero para actrices.

---

### Ejemplo de Consultas

```prolog
% Película: American Beauty (1999)
movie(american_beauty, 1999).
director(american_beauty, sam_mendes).
actor(american_beauty, kevin_spacey, lester_burnham).
actress(american_beauty, annette_bening, carolyn_burnham).
% Más actores y actrices de American Beauty...

% Película: Anna (1987)
movie(anna, 1987).
director(anna, yurek_bogayevicz).
actress(anna, sally_kirkland, anna).
actor(anna, robert_fields, daniel).
% Más actores y actrices de Anna...
```

---

### Ejercicios de Consultas

1. **¿En qué año se estrenó la película *American Beauty*?**
   ```prolog
   ?- movie(american_beauty, Y).
   ```

2. **Encuentra todas las películas estrenadas en el año 2000.**
   ```prolog
   ?- movie(M, 2000).
   ```

3. **Encuentra películas lanzadas antes del año 2000.**
   ```prolog
   ?- movie(M, Y), Y < 2000.
   ```

4. **Encuentra películas lanzadas después de 1990.**
   ```prolog
   ?- movie(M, Y), Y > 1990.
   ```

5. **Encuentra un actor que haya trabajado en más de una película.**
   ```prolog
   ?- actor(M1, A, _), actor(M2, A, _), M1 @> M2.
   ```

6. **Encuentra un director de una película en la que haya trabajado Scarlett Johansson.**
   ```prolog
   ?- actress(M, scarlett_johansson, _), director(M, D).
   ```

---

### Reglas Adicionales

Las siguientes reglas se pueden agregar para consultas más avanzadas:

- **Películas lanzadas después de un año específico:**
   ```prolog
   released_after(M, Y) :- movie(M, Y1), Y1 > Y.
   ```

- **Películas lanzadas antes de un año específico:**
   ```prolog
   released_before(M, Y) :- movie(M, Y1), Y1 < Y.
   ```

- **Películas lanzadas el mismo año:**
   ```prolog
   same_year(M1, M2) :- movie(M1, Y), movie(M2, Y).
   ```

- **Actores que co-protagonizaron una película:**
   ```prolog
   co_star(A1, A2) :- actor(M, A1, _), actor(M, A2, _).
   ```
-----

GTP de apoyo para SQL

https://chatgpt.com/g/g-OCdqpOW8Z-sql-generator

--- 

EVIDENCIA DEL LA PRACTICA:
1. Accer a https://sqlfiddle.com/ o AWSAcademy como se sea productivo.
2. Recrear la base de datos de Peliculas, en un gestor favorito, fácil para Ud.
3. Elabore por lo menos 5 consultas de la consola SQL que ofrece ese portal
4. Evidencia mixta puede ser Asciinema, Loom, pero no pantallas pues hay interactividad.
5. Para peliculas o tablas "generativas" use http://www.mockaroo.com

   Pin en 66115 de IDoceo, tiempo 1 hora.


