|res cli a1 a2 a3 asientos asientos2|

asientos:= OrderedCollection new.
a1 := #(1,1).
a2 := #(3,53).
a3 := #(2,32).
asientos add: a1.
asientos add: a2.
asientos add: a3.

cli := Cliente crearClienteId: 1 nombre: 'juan' apellido: 'perez' dni: 123456 telefono: 12345322 direccion: 'calle123' email: 'bnsgsd@dfdfh.com' noUduario: 'juancito05' contrasenia: '1234sdf'.

asientos2:= OrderedCollection new.
a1 := #(3,5).
a2 := #(56,53).
a3 := #(5,32).
asientos2 add: a1.
asientos2 add: a2.
asientos2 add: a3.

res := Reserva crearReserva: 1234 idFuncionPelicula: 'Suerman' asientos: asientos costo: 952.6.

cli verCuenta agregarReserva: res.
res := Reserva crearReserva: 1235 idFuncionPelicula: 'Suerman2' asientos: asientos2 costo: 94.6.
cli verCuenta agregarReserva: res.
res := Reserva crearReserva: 13 idFuncionPelicula: 'Sue34n2' asientos: asientos2 costo: 94.6.
cli verCuenta agregarReserva: res.
cli verCuenta agregarPuntos: 350.
cli verCuenta quitarPuntos: 50.
(cli verCuenta eliminarReserva: 1235 )inspect.
cli verCuenta verReservas inspect.
cli inspect.