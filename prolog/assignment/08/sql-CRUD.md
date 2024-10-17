Este es un diagrama muy funcional para modelar una tienda de celulares que maneja tanto ventas como reparaciones.

```mermaid
erDiagram
    CLIENTES {
        int ClienteID PK
        string Nombre
        string Apellido
        string Email
        string Telefono
        string Direccion
    }
    
    CELULARES {
        int ModeloID PK
        string Marca
        string Modelo
        decimal PrecioVenta
        int StockDisponible
    }
    
    VENTAS {
        int VentaID PK
        date FechaVenta
        int Cantidad
        decimal Total
        int ClienteID FK
        int ModeloID FK
    }
    
    REPARACIONES {
        int ReparacionID PK
        date FechaReparacion
        string DescripcionProblema
        decimal CostoReparacion
        int ClienteID FK
        int ModeloID FK
    }

    TECNICOS {
        int TecnicoID PK
        string Nombre
        string Especialidad
        string Telefono
    }

    REPARACIONES_TECNICOS {
        int ReparacionID FK
        int TecnicoID FK
    }

    CLIENTES ||--o{ VENTAS : "realiza"
    CELULARES ||--o{ VENTAS : "incluye"
    
    CLIENTES ||--o{ REPARACIONES : "solicita"
    CELULARES ||--o{ REPARACIONES : "es para"
    
    REPARACIONES ||--o{ REPARACIONES_TECNICOS : "asignada a"
    TECNICOS ||--o{ REPARACIONES_TECNICOS : "realiza"
```

### Explicación:
- **Clientes** tiene una relación uno-a-muchos con **Ventas** (un cliente puede realizar múltiples ventas).
- **Celulares** tiene una relación uno-a-muchos con **Ventas** (un celular puede estar en múltiples ventas).
- **Clientes** tiene una relación uno-a-muchos con **Reparaciones** (un cliente puede solicitar múltiples reparaciones).
- **Celulares** tiene una relación uno-a-muchos con **Reparaciones** (un celular puede estar en múltiples reparaciones).
- **Reparaciones** tiene una relación muchos-a-muchos con **Técnicos**, y esta relación se modela a través de la tabla intermedia **Reparaciones_Tecnicos**.

