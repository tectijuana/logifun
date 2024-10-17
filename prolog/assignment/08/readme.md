# CASOS NO PRACTICOS
## de Diagrama Entidad-Reacion de Base de datos a Prolog

Para modelar el esquema de la tienda de celulares en Prolog, es necesario representar las entidades y sus relaciones como hechos y reglas. Cada entidad (clientes, celulares, ventas, reparaciones, técnicos) se modela con predicados que describen sus atributos. Las relaciones entre las entidades se pueden modelar mediante predicados adicionales.

A continuación se muestra el código Prolog basado en el diagrama ER proporcionado:

```prolog
% Definición de clientes: cliente(ClienteID, Nombre, Apellido, Email, Telefono, Direccion).
cliente(1, 'Juan', 'Pérez', 'juan.perez@mail.com', '555-1234', 'Calle Falsa 123').
cliente(2, 'Ana', 'Gómez', 'ana.gomez@mail.com', '555-5678', 'Av. Siempreviva 742').

% Definición de celulares: celular(ModeloID, Marca, Modelo, PrecioVenta, StockDisponible).
celular(1, 'Apple', 'iPhone 13', 999.99, 10).
celular(2, 'Samsung', 'Galaxy S21', 799.99, 15).

% Definición de ventas: venta(VentaID, FechaVenta, Cantidad, Total, ClienteID, ModeloID).
venta(1, '2023-10-01', 1, 999.99, 1, 1).
venta(2, '2023-10-05', 2, 1599.98, 2, 2).

% Definición de reparaciones: reparacion(ReparacionID, FechaReparacion, DescripcionProblema, CostoReparacion, ClienteID, ModeloID).
reparacion(1, '2023-09-15', 'Pantalla rota', 199.99, 1, 1).
reparacion(2, '2023-10-07', 'Batería defectuosa', 99.99, 2, 2).

% Definición de técnicos: tecnico(TecnicoID, Nombre, Especialidad, Telefono).
tecnico(1, 'Carlos', 'Pantallas', '555-9876').
tecnico(2, 'Laura', 'Baterías', '555-5432').

% Relación de reparaciones y técnicos: reparacion_tecnico(ReparacionID, TecnicoID).
reparacion_tecnico(1, 1).
reparacion_tecnico(2, 2).

% Reglas para consultar datos.

% Regla para obtener las ventas realizadas por un cliente
ventas_cliente(ClienteID, VentaID, FechaVenta, Cantidad, Total, ModeloID) :-
    venta(VentaID, FechaVenta, Cantidad, Total, ClienteID, ModeloID).

% Regla para obtener las reparaciones solicitadas por un cliente
reparaciones_cliente(ClienteID, ReparacionID, FechaReparacion, DescripcionProblema, CostoReparacion, ModeloID) :-
    reparacion(ReparacionID, FechaReparacion, DescripcionProblema, CostoReparacion, ClienteID, ModeloID).

% Regla para obtener las reparaciones asignadas a un técnico
reparaciones_tecnico(TecnicoID, ReparacionID, FechaReparacion, DescripcionProblema, CostoReparacion, ClienteID, ModeloID) :-
    reparacion_tecnico(ReparacionID, TecnicoID),
    reparacion(ReparacionID, FechaReparacion, DescripcionProblema, CostoReparacion, ClienteID, ModeloID).
```

### Explicación:

1. **Clientes**: Se define un predicado `cliente/6` para representar a los clientes y sus atributos.
2. **Celulares**: Se define un predicado `celular/5` para representar los modelos de celulares disponibles.
3. **Ventas**: Se define el predicado `venta/6` para representar las ventas, asociando clientes con celulares.
4. **Reparaciones**: El predicado `reparacion/6` representa las reparaciones realizadas, asociando clientes con celulares.
5. **Técnicos**: El predicado `tecnico/4` representa a los técnicos disponibles.
6. **Reparaciones_Tecnicos**: El predicado `reparacion_tecnico/2` establece la relación entre reparaciones y técnicos asignados.
   
### Consultas:

- **Ventas de un cliente**: Puedes consultar las ventas de un cliente usando `ventas_cliente/6`.
  ```prolog
  ?- ventas_cliente(1, VentaID, Fecha, Cantidad, Total, ModeloID).
  ```

- **Reparaciones de un cliente**: Puedes consultar las reparaciones solicitadas por un cliente usando `reparaciones_cliente/6`.
  ```prolog
  ?- reparaciones_cliente(1, ReparacionID, Fecha, Descripcion, Costo, ModeloID).
  ```

- **Reparaciones asignadas a un técnico**: Para saber qué reparaciones tiene asignado un técnico, usa `reparaciones_tecnico/7`.
  ```prolog
  ?- reparaciones_tecnico(1, ReparacionID, Fecha, Descripcion, Costo, ClienteID, ModeloID).
  ```

Este código puede extenderse para cubrir más funcionalidades, como agregar, eliminar o modificar datos de las entidades, como lo conocemos el CRUD en base de datos.
