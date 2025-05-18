|a |

a := Asiento crearAsientoFila: '1' Columna: '2'.
a nuevoEstadoReserva: 'superman'.
a nuevoEstadoReserva: 'superman2'.
a nuevoEstadoReserva: 'superman3'.
a nuevoEstadoReserva: 'superman4'.
a reservar: 'superman3'.
a reservar: 'superman4'.
a liberarReserva: 'superman3'.
(a comprobarReserva: 'superman4' )inspect.
a eliminarEstadoReserva: 'superman'.
a inspect.