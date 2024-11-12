<img width="997" alt="385100406-80318fd5-2197-497e-8832-36644ea2f40a" src="https://github.com/user-attachments/assets/bfc05dc1-bb94-48e3-85db-e296b07f84e3">

---
<iframe width="560" height="315" src="https://www.youtube.com/embed/YFUv4_lCBLY?si=dAnNyrzAOvfjzMO8" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>
---

### **Propuesta Práctica para el Proyecto Cognify**

#### **Título:** 
**Simulación de Decisiones Éticas y Rehabilitación en Cognify usando Prolog**

---

### **Objetivo de la Práctica**
Crear un sistema básico en Prolog que simule las decisiones éticas y los resultados de los procesos de rehabilitación criminal basados en el modelo Cognify, utilizando datos simulados. Los estudiantes explorarán cómo la lógica y los datos estáticos pueden modelar interacciones complejas.

---

### **Elementos de la Actividad**

1. **Narrativa Central:**
   Los estudiantes serán "ingenieros de rehabilitación" que deben configurar y analizar el sistema Cognify para diferentes casos simulados de criminales. Esto introduce un aspecto narrativo para hacer la práctica más inmersiva y relevante.

2. **Uso de Datos Simulados:**
   Los estudiantes trabajarán con un conjunto de datos generados usando portales de mockdata (por ejemplo, Mockaroo, JSON Generator). Los datos incluirán:
   - Perfil psicológico del criminal.
   - Tipo de crimen cometido.
   - Rehabilitación sugerida.
   - Respuesta simulada del sistema Cognify.

3. **Problema a Resolver:**
   Los estudiantes usarán Prolog para:
   - Determinar la adecuación de ciertos recuerdos artificiales basados en el perfil del criminal.
   - Simular las decisiones del sistema Cognify según reglas predefinidas.
   - Analizar el impacto de las decisiones utilizando predicados de Prolog.

---

### **Estructura de la Práctica**

#### **Paso 1: Introducción al Caso**
Presentar a los estudiantes el modelo narrativo de Cognify. Explicar cómo el sistema selecciona recuerdos artificiales basándose en un conjunto de reglas éticas y psicológicas. Por ejemplo:
   - "Si el crimen es violento y el criminal muestra baja empatía, implantar recuerdos de sufrimiento de la víctima."
   - "Si el criminal es un estafador, mostrar las consecuencias sociales de su acto."

#### **Paso 2: Generación de Datos**
Los estudiantes trabajarán con un dataset generado previamente, que podría incluir:
   - **Ejemplo de datos:**
     ```json
     {
       "id": 1,
       "crimen": "robo",
       "nivel_empatia": "bajo",
       "recuerdos_disponibles": ["víctima", "familia", "arrepentimiento"],
       "resultado_esperado": "rehabilitación exitosa"
     }
     ```

#### **Paso 3: Implementación en Prolog**
Los estudiantes programarán en Prolog reglas y predicados como:
   - `recuerdo_apropiado(Crimen, NivelEmpatia, Recuerdo).`
   - `evaluar_resultado(Crimen, Recuerdo, ResultadoEsperado).`
   - `recomendar_rehabilitacion(Crimen, NivelEmpatia, RecuerdosRecomendados).`

#### **Paso 4: Simulación y Pruebas**
Usar los datos simulados para:
   - Consultar en Prolog cuál es el mejor recuerdo a implantar.
   - Validar si las reglas funcionan correctamente.
   - Ajustar las reglas y los datos según los resultados obtenidos.

#### **Paso 5: Reflexión Ética**
Guiar una discusión sobre las implicaciones éticas del uso de este tipo de tecnología, simulando un debate donde los estudiantes defiendan o cuestionen el uso de Cognify.

---

### **Entrega Final**
1. **Código:** Archivo Prolog con las reglas implementadas.
2. **Reporte:** Análisis de los resultados y reflexión ética.
3. **Presentación:** Cada grupo presenta un caso específico, explica las decisiones tomadas y defiende los resultados obtenidos.

---

### **Materiales de Apoyo**
1. **Tutorial Prolog:** Mini-guía para construir predicados.
2. **Dataset:** Archivo JSON o CSV con datos generados de mockdata.
3. **Plantillas:** Ejemplos iniciales de reglas en Prolog.

---



