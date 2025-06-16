| package |
package := Package name: 'Cine'.
package paxVersion: 1;
	basicComment: ''.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAzIEYPDQAEAAAASW1hZ2VTdHJpcHBlcgAAAABSAAAABAAAAENpbmVSAAAAWwAAAEM6XFVz
ZXJzXGF0cmVlXERlc2t0b3BcaW5nZW5pZXJpYSBpbmZvcm1hdGljYVxNZXRvZG9sb2dpYXMgZGUg
UHJvZ3JhbWFjaW9uIElJXENpbmVcQ2luZS5leGWaAAAAUgAAAAQAAABDaW5lUgAAAAkAAABDaW5l
U2hlbGzvvyUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA==').

package classNames
	add: #Asiento;
	add: #AsientoReservaModel;
	add: #Cine;
	add: #CineModel;
	add: #CineNuevaFuncionPelicula;
	add: #CineNuevaPelicula;
	add: #CineNuevaReserva;
	add: #CineNuevaSala;
	add: #CineShell;
	add: #Cliente;
	add: #CuentaCliente;
	add: #FuncionPelicula;
	add: #FuncionPeliculaModel;
	add: #Horario;
	add: #InfoCliente;
	add: #InfoFuncionPelicula;
	add: #InfoPelicula;
	add: #InfoReserva;
	add: #InfoSala;
	add: #MenuSalas;
	add: #Pelicula;
	add: #PeliculaModel;
	add: #Persona;
	add: #Reserva;
	add: #ReservaModel;
	add: #Sala;
	add: #SalaModel;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Boolean\Dolphin Boolean Presenter';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Choice\Dolphin Choice Presenter';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Image\Dolphin Image Presenter';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Number\Dolphin Number Presenter';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Views\Styled Views\Dolphin Styled Views';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	yourself).

package!

"Class Definitions"!

Object subclass: #Asiento
	instanceVariableNames: 'fila columna estaReservado'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Cine
	instanceVariableNames: 'nombre salas cartelera candy empleados clientes cuit direccion ultimoIdReserva ultimoIdCliente'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #CuentaCliente
	instanceVariableNames: 'thisNombreUsuario thisContrasenia thisPuntos thisReservas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #FuncionPelicula
	instanceVariableNames: 'idFuncionPelicula nombreSala nombrePelicula horario idioma dia'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Horario
	instanceVariableNames: 'inicio fin peliculaEnEmision dia idFuncionPelicula'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Pelicula
	instanceVariableNames: 'titulo duracion formato funciones precio'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Persona
	instanceVariableNames: 'nombre apellido dni telefono direccion email'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Reserva
	instanceVariableNames: 'thisIdReserva thisIdFuncionPelicula thisAsientos thisCosto idCliente'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Sala
	instanceVariableNames: 'nombreSala asientos horarios tipo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #AsientoReservaModel
	instanceVariableNames: 'fila columna estado'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #CineModel
	instanceVariableNames: 'cine fechaActual cantidadSalas cantidadPeliculas cantidadClientes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #FuncionPeliculaModel
	instanceVariableNames: 'nombreSala nombrePelicula formato dias horas idiomas minutos idiomaElegido diaElegido salas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #PeliculaModel
	instanceVariableNames: 'titulo duracion formato precio formatos'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #ReservaModel
	instanceVariableNames: 'idFuncionPelicula asientos idCliente'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #SalaModel
	instanceVariableNames: 'nombre tipo cantidadFilas cantidadColumnas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Persona subclass: #Cliente
	instanceVariableNames: 'thisIdCliente thisCuenta'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #CineShell
	instanceVariableNames: 'nombreCine btnNuevaPelicula cuit direccion fechaActual peliculas funciones salas lunes martes miercoles jueves viernes sabado domingo f2D f3D f4D fIMAX idiomas cantidadSalas cantidadPeliculasTotales cantidadPeliculasMostradas cantidadClientes cantidadFunciones infoPeliculaAbierta menuSala'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #InfoCliente
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #InfoFuncionPelicula
	instanceVariableNames: 'idFuncionPelicula nombreSala nombrePelicula horario idioma dia'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #InfoPelicula
	instanceVariableNames: 'titulo duracion formato funciones precio'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #InfoReserva
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #InfoSala
	instanceVariableNames: 'nombreSala asientos horarios tipo asientosFiltrados btnToggleAsientos textoAsientos'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #MenuSalas
	instanceVariableNames: 'salas infoSalaAbierta s2D s3D s4D sIMAX cantSalas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Dialog subclass: #CineNuevaFuncionPelicula
	instanceVariableNames: 'salas nombreSala nombrePelicula horas minutos idiomas idiomaElegido dias diaElegido formato'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Dialog subclass: #CineNuevaPelicula
	instanceVariableNames: 'tituloPresenter duracionPresenter precioPresenter formatoPresenter formatoElegido'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Dialog subclass: #CineNuevaReserva
	instanceVariableNames: 'clientes buscador idCliente nombreCompleto dni telefono titulo sala horario idioma okCancel asientos asientosPresenter dia formato precioUnitario precioTotal'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Dialog subclass: #CineNuevaSala
	instanceVariableNames: 'nombre tipo cantidadFilas cantidadColumnas tipoElegido'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Asiento guid: (GUID fromString: '{F48C493D-7156-46ED-AB73-26B17DAFD69A}')!
Asiento comment: ''!
!Asiento categoriesForClass!TPI-Cine! !
!Asiento methodsFor!

comprobarReserva: idFuncionPelicula
	^estaReservado at: idFuncionPelicula ifAbsent: [^nil]!

eliminarEstadoReserva: idFuncionPelicula
	estaReservado removeKey: idFuncionPelicula ifAbsent: [^nil]!

iniAsientoFila: unaFila Columna: unaColumna
	fila := unaFila.
	columna := unaColumna.
	estaReservado := Dictionary new!

liberarReserva: idFuncionPelicula
	(estaReservado includesKey: idFuncionPelicula)
		ifTrue: [estaReservado at: idFuncionPelicula put: false]
		ifFalse: [^nil]!

nuevoEstadoReserva: idFuncionPelicula
	(estaReservado includesKey: idFuncionPelicula)
		ifTrue: [^nil]
		ifFalse: [estaReservado at: idFuncionPelicula put: false]!

printOn: aStream
	aStream
		nextPutAll: 'Fila: ' , self verFila printString , ' - Columna: ' , self verColumna printString!

reservar: idFuncionPelicula
	(estaReservado includesKey: idFuncionPelicula)
		ifTrue: [estaReservado at: idFuncionPelicula put: true]
		ifFalse: [^nil]!

verColumna
	^columna!

verFila
          ^fila. !

verReservas
	^estaReservado! !
!Asiento categoriesFor: #comprobarReserva:!public! !
!Asiento categoriesFor: #eliminarEstadoReserva:!public! !
!Asiento categoriesFor: #iniAsientoFila:Columna:!private! !
!Asiento categoriesFor: #liberarReserva:!public! !
!Asiento categoriesFor: #nuevoEstadoReserva:!public! !
!Asiento categoriesFor: #printOn:!public! !
!Asiento categoriesFor: #reservar:!public! !
!Asiento categoriesFor: #verColumna!public! !
!Asiento categoriesFor: #verFila!public! !
!Asiento categoriesFor: #verReservas!public! !

!Asiento class methodsFor!

crearAsientoFila: unaFila Columna: unaColumna
	^self new iniAsientoFila: unaFila Columna: unaColumna! !
!Asiento class categoriesFor: #crearAsientoFila:Columna:!public! !

Cine guid: (GUID fromString: '{66982B7D-A579-4656-BFD8-F742AB78A63D}')!
Cine comment: ''!
!Cine categoriesForClass!TPI-Cine! !
!Cine methodsFor!

agregarAsiento: unAsiento Sala: unaSala
	"Private - Intenta agregar un asiento a una sala. Si lo agrega retorna true.
	si la sala no existe retorna nil. Si la sala existe y el asiento tambien retonra false."

	(self seleccionarSala: unaSala verNombreSala)
		ifNil: [^nil]
		ifNotNil: [^(self seleccionarSala: unaSala verNombreSala) agregarAsiento: unAsiento]!

agregarAsientosSala: unaSala cantFilas: filas cantColumnas: columnas
	"Private - Agrega (filas x columnas) asientos a una Sala"

	1 to: filas
		do: 
			[:fila |
			1 to: columnas
				do: 
					[:columna |
					(self seleccionarSala: unaSala verNombreSala)
						agregarAsiento: (Asiento crearAsientoFila: fila Columna: columna)]]!

agregarCliente: unCliente
	"Private - Intenta agregar un cliente. Si lo agrega retorna true.
	Si el cliente con ese id o un cliente con ese nombre apellido y dni existe retorna false."

	(self
		existeClienteId: unCliente verIdCliente
		nombre: unCliente verNombre
		apellido: unCliente verApellido
		dni: unCliente verDni)
			ifTrue: [^false]
			ifFalse: 
				[clientes add: unCliente.
				^true]!

agregarFuncionPelicula: unaFuncionPelicula pelicula: unaPelicula
	"Private - Intenta agregar una funcion a una pelicula, si la agrega retorna true.
	si la pelicula no existe retorna nil, si la pelicula existe y la funcion ya existe retorna false."

	(cartelera includesKey: unaPelicula verTitulo , unaPelicula verFormato)
		ifFalse: [^nil]
		ifTrue: 
			[^(self seleccionarPelicula: unaPelicula verTitulo formato: unaPelicula verFormato)
				agregarFuncionPelicula: unaFuncionPelicula]!

agregarHorario: unHorario Sala: unaSala
	"Private - Intenta agregar una instancia de Horario a una sala. Si lo agrega retorna true.
	si la sala no existe retorna nil. Si la sala existe y el Horario tambien retonra false."

	(self seleccionarSala: unaSala verNombreSala)
		ifNil: [^false]
		ifNotNil: [^((self seleccionarSala: unaSala verNombreSala) agregarHorario: unHorario)]!

agregarPelicula: unaPelicula
	(cartelera includesKey: unaPelicula verTitulo , unaPelicula verFormato)
		ifFalse: 
			[cartelera at: unaPelicula verTitulo , unaPelicula verFormato put: unaPelicula.
			^true]
		ifTrue: [^false]!

agregarReserva: unaReserva Cliente: unCliente
	"Private - Agrega una reserva a un cliente"

	(self
		existeClienteId: unCliente verIdCliente
		nombre: unCliente verNombre
		apellido: unCliente verApellido
		dni: unCliente verDni)
			ifTrue: 
				[^((self seleccionarCliente: unCliente verIdCliente) verCuenta agregarReserva: unaReserva)
					ifTrue: 
						[ultimoIdReserva := ultimoIdReserva + 1.
						^true]
					ifFalse: [^false]]
			ifFalse: [^nil]!

agregarSala: unaSala
	(self seleccionarSala: unaSala verNombreSala)
		ifNil: 
			[salas add: unaSala.
			^true]
		ifNotNil: [^false]!

eliminarCliente: unCliente
	(self
		existeClienteId: unCliente verIdCliente
		nombre: unCliente verNombre
		apellido: unCliente verApellido
		dni: unCliente verDni)
			ifTrue: 
				[clientes remove: unCliente.
				^true]
			ifFalse: [^false]!

eliminarFuncion: unaFuncion
	| reservasEliminar |
	reservasEliminar := OrderedCollection new.

	"####eliminar las reservas####"
	1 to: clientes size
		do: 
			[:i |
			reservasEliminar := (clientes at: i) verCuenta verReservas
						select: [:res | res verIdFuncionPelicula = unaFuncion verIdFuncionPelicula].
			1 to: reservasEliminar size
				do: [:x | self eliminarReserva: (reservasEliminar at: x) cliente: (clientes at: i)]].


	"####eliminar el horario####"
	(self seleccionarSala: unaFuncion verNombreSalaAsignada)
		eliminarHorarioIdFuncionPelicula: unaFuncion verIdFuncionPelicula.

	"####elimina las reservas en los asientos de la sala en la que esta registrada la pelicula####"
	1 to: (self seleccionarSala: unaFuncion verNombreSalaAsignada) verAsientos size
		do: 
			[:i |
			((self seleccionarSala: unaFuncion verNombreSalaAsignada) verAsientos at: i)
				eliminarEstadoReserva: unaFuncion verIdFuncionPelicula].

	"####elimina la funcion de la clase pelicula####"
	(self seleccionarPelicula: unaFuncion verNombrePelicula
		formato: (self seleccionarSala: unaFuncion verNombreSalaAsignada) verTipo)
			eliminarFuncionPelicula: unaFuncion verIdFuncionPelicula!

eliminarPelicula: unaPelicula
	| funcionesRemove |
	funcionesRemove := unaPelicula verFunciones.
	funcionesRemove keysDo: [:key | self eliminarFuncion: (funcionesRemove at: key)].
	cartelera removeKey: unaPelicula verTitulo , unaPelicula verFormato ifAbsent: [^nil]!

eliminarReserva: unaReserva cliente: unCliente
	"Si el cliente no existe retorna nil.
	si el cliente existe y no tiene esa reserva retorna valse.
	si el cliente existe y tiene esa reserva, la elimina y retorna true."

	| asientos idFuncionPelicula pelicula sala |
	asientos := unaReserva verAsientos.
	idFuncionPelicula := unaReserva verIdFuncionPelicula.
	pelicula := cartelera detect: [:peli | (peli seleccionarFuncionPelicula: idFuncionPelicula) notNil].
	sala := self
				seleccionarSala: (pelicula seleccionarFuncionPelicula: idFuncionPelicula) verNombreSalaAsignada.
	(self
		existeClienteId: unCliente verIdCliente
		nombre: unCliente verNombre
		apellido: unCliente verApellido
		dni: unCliente verDni)
			ifTrue: 
				[((self seleccionarCliente: unCliente verIdCliente) verCuenta
					eliminarReserva: unaReserva verIdReserva)
						ifTrue: 
							[1 to: asientos size
								do: 
									[:asi |
									(self seleccionarSala: sala verNombreSala)
										cancelarReserva: idFuncionPelicula
										fila: ((asientos at: asi) at: 1)
										columna: ((asientos at: asi) at: 2)].
							^true]]
			ifFalse: [^nil]!

existeClienteId: id nombre: unNombre apellido: unApellido dni: unDni
	| clienteEncontrado |
	clienteEncontrado := true.
	clientes detect: 
			[:cli |
			cli verIdCliente = id
				or: [cli verNombre = unNombre and: [cli verApellido = unApellido and: [cli verDni = unDni]]]]
		ifNone: [clienteEncontrado := false].
	^clienteEncontrado!

iniCine: unNombre
	nombre := unNombre.
	salas := OrderedCollection new.
	cartelera := Dictionary new.
	candy := 'No implementado'.
	empleados := 'No implementado'.
	clientes := OrderedCollection new.
	ultimoIdReserva := 0.
	ultimoIdCliente := 0!

iniCine: unNombre cuit: unCuit direccion: unaDireccion
	nombre := unNombre.
	cuit := unCuit.
	direccion := unaDireccion.
	salas := OrderedCollection new.
	cartelera := Dictionary new.
	candy := 'No implementado'.
	empleados := 'No implementado'.
	clientes := OrderedCollection new.
	ultimoIdReserva := 0.
	ultimoIdCliente := 0!

nuevaFuncionPeliculaHorario: unaHora idioma: unIdioma formato: unFormato dia: unDia pelicula: unaPelicula sala: unaSala
	| funcion horario horaFin horas minutos |
	minutos := unaHora \\ 100 + unaPelicula verDuracion.
	horas := unaHora // 100 + (minutos // 60).
	horaFin := horas * 100 + (minutos \\ 60).
	horario := Horario
				crearHorarioDia: unDia
				Inicio: unaHora
				horaFin: horaFin
				nombrePelicula: unaPelicula verTitulo.
	(self agregarHorario: horario Sala: unaSala)
		ifTrue: 
			[funcion := FuncionPelicula
						crearFuncionPeliculaNombreSala: unaSala verNombreSala
						nombrePelicula: unaPelicula verTitulo
						horario: unaHora
						idioma: unIdioma
						formato: unFormato
						dia: unDia.
			horario setIdFuncionPelicula: funcion verIdFuncionPelicula.
			1 to: (self seleccionarSala: unaSala verNombreSala) verAsientos size
				do: 
					[:i |
					((self seleccionarSala: unaSala verNombreSala) verAsientos at: i)
						nuevoEstadoReserva: funcion verIdFuncionPelicula].
			^self agregarFuncionPelicula: funcion pelicula: unaPelicula]
		ifFalse: [^false]!

nuevaPeliculaTitulo: unTitulo duracion: minutos formato: unFormato precio: unPrecio
	| unaPelicula |
	unaPelicula := Pelicula
				crearPeliculaTitulo: unTitulo
				duracion: minutos
				formato: unFormato
				precio: unPrecio.
	^self agregarPelicula: unaPelicula!

nuevaReservaIdFuncionPelicula: idFuncionPelicula asientos: asientos cliente: unCliente
	"Recolecta la pelicula que contiene la funcion con el IdFuncionPelicula"
	| pelicula nombreSala flag costo |
	pelicula := cartelera detect: [:peli | (peli seleccionarFuncionPelicula: idFuncionPelicula) notNil]
				ifNone: [nil].
	flag := true.
	pelicula
		ifNotNil: 
			["nombreSala := (self
						seleccionarSala: (pelicula seleccionarFuncionPelicula: idFuncionPelicula) verNombreSalaAsignada)
							verNombreSala."
			nombreSala := (pelicula seleccionarFuncionPelicula: idFuncionPelicula) verNombreSalaAsignada.
			"Calcula el precio total de la reserva"
			costo := pelicula verPrecio * asientos size.
			"Comprueba que los asientos no esten reservados"
			1 to: asientos size
				do: 
					[:i |
					| asiento |
					asiento := asientos at: i.
					(((self seleccionarSala: nombreSala) seleccionarAsientoFila: (asiento at: 1)
						columna: (asiento at: 2)) comprobarReserva: idFuncionPelicula)
						ifNotNil: [:res | res ifTrue: [flag := false]]
						ifNil: [flag := false]].
			"Comprueba que existe el cliente"
			(self
				existeClienteId: unCliente verIdCliente
				nombre: unCliente verNombre
				apellido: unCliente verApellido
				dni: unCliente verDni) ifFalse: [flag := false].
			flag
				ifTrue: 
					["Si el cliente existe"

					"Crea la reserva"
					| reserva |
					reserva := Reserva
								crearReserva: ultimoIdReserva + 1
								idFuncionPelicula: idFuncionPelicula
								asientos: asientos
								costo: costo.
					"le agrega el id del cliente que realizo la reserva"
					reserva idCliente: unCliente verIdCliente.
					"setea como Reservados los asientos"
					1 to: asientos size
						do: 
							[:i |
							| asientoAReservar |
							asientoAReservar := asientos at: i.
							(self seleccionarSala: nombreSala)
								reservarAsiento: idFuncionPelicula
								fila: (asientoAReservar at: 1)
								columna: (asientoAReservar at: 2)].
					"Agrega la reserva al cliente"
					flag := self agregarReserva: reserva Cliente: unCliente]].
	^flag!

nuevaSalaNombre: unNombre tipoSala: unTipo
	| unaSala |
	unaSala := Sala crearSalaNombre: unNombre Tipo: unTipo.
	^self agregarSala: unaSala!

nuevoClienteNombre: unNombre apellido: unApellido dni: unDni telefono: unTelefono direccion: unaDireccion email: unEmail nomUsuario: nomUsuario contrasenia: contrasenia
	"Intenta agregar un cliente. Si lo agrega retorna true.
	Si el cliente con ese id o un cliente con ese nombre apellido y dni existe retorna false."

	(self
		existeClienteId: ultimoIdCliente + 1
		nombre: unNombre
		apellido: unApellido
		dni: unDni)
			ifTrue: [^false]
			ifFalse: 
				[self agregarCliente: (Cliente
							crearClienteId: ultimoIdCliente + 1
							nombre: unNombre
							apellido: unApellido
							dni: unDni
							telefono: unTelefono
							direccion: unaDireccion
							email: unEmail
							noUsuario: nomUsuario
							contrasenia: contrasenia).
				ultimoIdCliente := ultimoIdCliente + 1.
				^true]!

seleccionarCliente: idCliente
	^clientes detect: [:cli | cli verIdCliente = idCliente] ifNone: [^nil]!

seleccionarPelicula: titulo formato: formato
	^cartelera at: (titulo , formato) ifAbsent: [^nil]!

seleccionarSala: nombreSala
	^salas detect: [:sal | sal verNombreSala = nombreSala] ifNone: [^nil]!

verClientes
	^clientes!

verCuit
	^cuit!

verDireccion
	^direccion!

verFuncionesDePelicula: unaPelicula
	^(self seleccionarPelicula: unaPelicula verTitulo formato: unaPelicula verFormato) verFunciones!

verHorariosSala: unaSala
	^(self seleccionarSala: unaSala verNombreSala) verHorarios!

verNombre
	^nombre!

verPeliculas
	^cartelera!

verPeliculasOrdenadas
	"mostrar en pantalla la lista de peliculas
	y retorna otra lista ordenada por nombre y luego por formato con strings (titulo+formato)
	indice titulo formato precio
	ej:
	1 Batman 3D $850
	2 Batman 4D $1000
	3 Spiderman2 2D $500
	4 Spiderman2 3D $850
	...
	"

	| listaPeliculasOrdenada lista p |
	lista := OrderedCollection new.
	listaPeliculasOrdenada := cartelera asSortedCollection: 
					[:x :y |
					x verTitulo < y verTitulo or: [x verTitulo = y verTitulo and: [x verFormato < y verFormato]]].
	1 to: listaPeliculasOrdenada size
		do: 
			[:i |
			p := OrderedCollection new.
			p add: (listaPeliculasOrdenada at: i) verTitulo.
			p add: (listaPeliculasOrdenada at: i) verFormato.
			lista add: p].
	^lista!

verReservas
	"implementar"

	"recorrer todos los clientes"

	"para cada cliente ver todas las reservas e ir agregandolas a una lista ordenada por el id"

	"retornar una lista con todas las reservas y algun dato del cliente (id , nombre o algo para poder identificarlo)"

	| reservas |
	reservas := OrderedCollection new.
	1 to: clientes size
		do: 
			[:i |
			(clientes at: i verCuenta verReservas isEmpty)
				ifFalse: [reservas addAll: (clientes at: i verCuenta verReservas)]].
	^reservas!

verReservasCliente: unCliente
	(self
		existeClienteId: unCliente verIdCliente
		nombre: unCliente verNombre
		apellido: unCliente verApellido
		dni: unCliente verDni) notNil
		ifTrue: [^(self seleccionarCliente: unCliente verIdCliente) verCuenta verReservas]
		ifFalse: [^nil]!

verSalas
	^salas! !
!Cine categoriesFor: #agregarAsiento:Sala:!private! !
!Cine categoriesFor: #agregarAsientosSala:cantFilas:cantColumnas:!private! !
!Cine categoriesFor: #agregarCliente:!private! !
!Cine categoriesFor: #agregarFuncionPelicula:pelicula:!private! !
!Cine categoriesFor: #agregarHorario:Sala:!private! !
!Cine categoriesFor: #agregarPelicula:!private! !
!Cine categoriesFor: #agregarReserva:Cliente:!private! !
!Cine categoriesFor: #agregarSala:!private! !
!Cine categoriesFor: #eliminarCliente:!public! !
!Cine categoriesFor: #eliminarFuncion:!public! !
!Cine categoriesFor: #eliminarPelicula:!public! !
!Cine categoriesFor: #eliminarReserva:cliente:!public! !
!Cine categoriesFor: #existeClienteId:nombre:apellido:dni:!public! !
!Cine categoriesFor: #iniCine:!private! !
!Cine categoriesFor: #iniCine:cuit:direccion:!private! !
!Cine categoriesFor: #nuevaFuncionPeliculaHorario:idioma:formato:dia:pelicula:sala:!public! !
!Cine categoriesFor: #nuevaPeliculaTitulo:duracion:formato:precio:!public! !
!Cine categoriesFor: #nuevaReservaIdFuncionPelicula:asientos:cliente:!public! !
!Cine categoriesFor: #nuevaSalaNombre:tipoSala:!public! !
!Cine categoriesFor: #nuevoClienteNombre:apellido:dni:telefono:direccion:email:nomUsuario:contrasenia:!public! !
!Cine categoriesFor: #seleccionarCliente:!public! !
!Cine categoriesFor: #seleccionarPelicula:formato:!public! !
!Cine categoriesFor: #seleccionarSala:!public! !
!Cine categoriesFor: #verClientes!public! !
!Cine categoriesFor: #verCuit!public! !
!Cine categoriesFor: #verDireccion!public! !
!Cine categoriesFor: #verFuncionesDePelicula:!public! !
!Cine categoriesFor: #verHorariosSala:!public! !
!Cine categoriesFor: #verNombre!public! !
!Cine categoriesFor: #verPeliculas!public! !
!Cine categoriesFor: #verPeliculasOrdenadas!public! !
!Cine categoriesFor: #verReservas!public! !
!Cine categoriesFor: #verReservasCliente:!public! !
!Cine categoriesFor: #verSalas!public! !

!Cine class methodsFor!

crearCine: unNombre

	^self new iniCine: unNombre.!

crearCine: unNombre cuit: unCuit direccion: unaDireccion
	^self new
		iniCine: unNombre
		cuit: unCuit
		direccion: unaDireccion! !
!Cine class categoriesFor: #crearCine:!public! !
!Cine class categoriesFor: #crearCine:cuit:direccion:!public! !

CuentaCliente guid: (GUID fromString: '{7514C972-5771-4862-BC3B-B3E9EAA40340}')!
CuentaCliente comment: ''!
!CuentaCliente categoriesForClass!TPI-Cine! !
!CuentaCliente methodsFor!

agregarPuntos: cantPuntos
	thisPuntos := thisPuntos + cantPuntos!

agregarReserva: unaReserva
	(self seleccionarReserva: unaReserva verIdReserva)
		ifNil: 
			[thisReservas add: unaReserva.
			^true]
		ifNotNil: [^false]!

eliminarReserva: idReserva
	| reservaRemove |
	reservaRemove := thisReservas detect: [:res | res verIdReserva = idReserva] ifNone: [^false].
	reservaRemove
		ifNotNil: 
			[thisReservas remove: reservaRemove.
			^true]!

iniCuentaCliente: nombreUsuario contrasenia: contrasenia
	thisNombreUsuario := nombreUsuario.
	thisContrasenia := contrasenia.
	thisPuntos := 0.
	thisReservas := OrderedCollection new!

modiContrasenia: contrasenia
	thisContrasenia := contrasenia!

printOn: aStream
	aStream nextPutAll: 'Usuario: ' , self verNombreUsuario!

quitarPuntos:cantPuntos
	(thisPuntos<cantPuntos) ifTrue: [^nil] 
					     ifFalse: [thisPuntos:=thisPuntos-cantPuntos].!

seleccionarReserva: idReserva
	| flag |
	flag := false.
	1 to: thisReservas size
		do: 
			[:i |
			(thisReservas at: i) verIdReserva = idReserva
				ifTrue: 
					[flag := true.
					^thisReservas at: i]].
	flag ifFalse: [^nil]

	"Otra opcion posible
	^thisReservas detect:[:res| res verIdReserva = idReserva] ifNone:[^nil]."!

verContrasenia
	^thisContrasenia!

verNombreUsuario
	^thisNombreUsuario!

verPuntos
	^thisPuntos!

verReservas
	^thisReservas! !
!CuentaCliente categoriesFor: #agregarPuntos:!public! !
!CuentaCliente categoriesFor: #agregarReserva:!public! !
!CuentaCliente categoriesFor: #eliminarReserva:!public! !
!CuentaCliente categoriesFor: #iniCuentaCliente:contrasenia:!private! !
!CuentaCliente categoriesFor: #modiContrasenia:!public! !
!CuentaCliente categoriesFor: #printOn:!public! !
!CuentaCliente categoriesFor: #quitarPuntos:!public! !
!CuentaCliente categoriesFor: #seleccionarReserva:!public! !
!CuentaCliente categoriesFor: #verContrasenia!public! !
!CuentaCliente categoriesFor: #verNombreUsuario!public! !
!CuentaCliente categoriesFor: #verPuntos!public! !
!CuentaCliente categoriesFor: #verReservas!public! !

!CuentaCliente class methodsFor!

crearCuentaCliente: nombreUsuario contrasenia: contrasenia
	^self new iniCuentaCliente: nombreUsuario contrasenia: contrasenia! !
!CuentaCliente class categoriesFor: #crearCuentaCliente:contrasenia:!public! !

FuncionPelicula guid: (GUID fromString: '{4204F44D-F7E9-4846-8644-03C0EF2C3B26}')!
FuncionPelicula comment: ''!
!FuncionPelicula categoriesForClass!TPI-Cine! !
!FuncionPelicula methodsFor!

<= unaFuncionPelicula
	^self verIdFuncionPelicula <= unaFuncionPelicula verIdFuncionPelicula!

iniFuncionPeliculaNombreSala: nomSala nombrePelicula: nomPelicula horario: unahora idioma: unIdioma formato: formato dia: unDia
	| hora |
	hora := unahora digitLength < 4 ifTrue: ['0' , unahora printString] ifFalse: [unahora printString].
	idFuncionPelicula := ((nomPelicula copyFrom: 1 to: 3) , (nomPelicula select: [:char | char isDigit])
				, (unDia copyFrom: 1 to: 2) , hora
				, '-' , (unIdioma copyFrom: 1 to: 3)
				, formato , '-('
				, nomSala , ')')
				asUppercase.
	nombreSala := nomSala.
	nombrePelicula := nomPelicula.
	horario := unahora.
	idioma := unIdioma.
	dia := unDia!

mayorDia: unaFuncionPelicula
	| dias |
	dias := Dictionary new.
	dias at: 'Lunes' put: 1.
	dias at: 'Martes' put: 2.
	dias at: 'Miércoles' put: 3.
	dias at: 'Jueves' put: 4.
	dias at: 'Viernes' put: 5.
	dias at: 'Sábado' put: 6.
	dias at: 'Domingo' put: 7.
	^(dias at: self verDia) < (dias at: unaFuncionPelicula verDia)!

mayorHorario: unaFuncionPelicula
	^self verHorario < unaFuncionPelicula verHorario!

mayorIdioma: unaFuncionPelicula
	^self verIdioma < unaFuncionPelicula verIdioma!

mayorSala: unaFuncionPelicula
	^self verNombreSalaAsignada < unaFuncionPelicula verNombreSalaAsignada!

mayorTitulo: unaFuncionPelicula
	^self verNombrePelicula < unaFuncionPelicula verNombrePelicula!

printOn: aStream
	aStream
		nextPutAll: 'Dia: ' , self verDia , '  Horario: ' , self verHorarioConFormato;
		nextPut: Character lf;
		nextPutAll: 'Sala: ' , self verNombreSalaAsignada , '  Idioma: ' , self verIdioma!

printString
	| str |
	str := String streamContents: 
					[:s |
					s
						nextPut: Character lf;
						nextPutAll: 'Dia: ';
						nextPutAll: self verDia;
						nextPutAll: '  Horario: ';
						nextPutAll: self verHorarioConFormato;
						nextPutAll: 'Sala: ';
						nextPutAll: self verNombreSalaAsignada;
						nextPutAll: '  Idioma: ';
						nextPutAll: self verIdioma].
	^str!

verDia
	^dia!

verHorario
	^horario!

verHorarioConFormato
	| hs min horarioConFormato |
	(self verHorario // 100 < 24 and: [self verHorario // 100 > 9])
		ifTrue: [hs := (self verHorario // 100) printString].
	self verHorario // 100 < 10 ifTrue: [hs := '0' , (self verHorario // 100) printString].
	self verHorario // 100 > 24 ifTrue: [hs := '0' , (self verHorario // 100 - 24) printString].
	self verHorario \\ 100 < 10
		ifTrue: [min := '0' , (self verHorario \\ 100) printString]
		ifFalse: [min := (self verHorario \\ 100) printString].
	horarioConFormato := hs , ':' , min , ' hs.  '.
	^horarioConFormato!

verIdFuncionPelicula
	^idFuncionPelicula!

verIdioma
	^idioma!

verNombrePelicula
	^nombrePelicula!

verNombreSalaAsignada
	^nombreSala! !
!FuncionPelicula categoriesFor: #<=!public! !
!FuncionPelicula categoriesFor: #iniFuncionPeliculaNombreSala:nombrePelicula:horario:idioma:formato:dia:!private! !
!FuncionPelicula categoriesFor: #mayorDia:!public! !
!FuncionPelicula categoriesFor: #mayorHorario:!public! !
!FuncionPelicula categoriesFor: #mayorIdioma:!public! !
!FuncionPelicula categoriesFor: #mayorSala:!public! !
!FuncionPelicula categoriesFor: #mayorTitulo:!public! !
!FuncionPelicula categoriesFor: #printOn:!public! !
!FuncionPelicula categoriesFor: #printString!public! !
!FuncionPelicula categoriesFor: #verDia!public! !
!FuncionPelicula categoriesFor: #verHorario!public! !
!FuncionPelicula categoriesFor: #verHorarioConFormato!public! !
!FuncionPelicula categoriesFor: #verIdFuncionPelicula!public! !
!FuncionPelicula categoriesFor: #verIdioma!public! !
!FuncionPelicula categoriesFor: #verNombrePelicula!public! !
!FuncionPelicula categoriesFor: #verNombreSalaAsignada!public! !

!FuncionPelicula class methodsFor!

crearFuncionPeliculaNombreSala: nombreSala nombrePelicula: nombrePelicula horario: horario idioma: idioma formato: formato dia: dia
	^self new
		iniFuncionPeliculaNombreSala: nombreSala
		nombrePelicula: nombrePelicula
		horario: horario
		idioma: idioma
		formato: formato
		dia: dia! !
!FuncionPelicula class categoriesFor: #crearFuncionPeliculaNombreSala:nombrePelicula:horario:idioma:formato:dia:!public! !

Horario guid: (GUID fromString: '{CA24DA3B-D18F-4325-883B-240969AAF974}')!
Horario comment: ''!
!Horario categoriesForClass!TPI-Cine! !
!Horario methodsFor!

comprobarSupHoraria: unHorario
	^unHorario verDiaHorario = dia and: 
			[(unHorario verHoraInicio between: inicio and: fin)
				or: [unHorario verHoraFin between: inicio and: fin]]!

iniHorarioDia: unDia Inicio: horaInicio horaFin: horaFin nombrePelicula: nombrePelicula
	dia := unDia.
	inicio := horaInicio.
	fin := horaFin.
	peliculaEnEmision := nombrePelicula!

iniHorarioDia: unDia Inicio: horaInicio horaFin: horaFin nombrePelicula: nombrePelicula idFuncionPelicula: unaIdFuncionPelicula
	dia := unDia.
	inicio := horaInicio.
	fin := horaFin.
	peliculaEnEmision := nombrePelicula.
	idFuncionPelicula := unaIdFuncionPelicula!

printOn: aStream
	| horaInicio horaFin minsInicio minsFin |
	horaInicio := self verHoraInicio // 100 >= 24
				ifTrue: ['0' , (self verHoraInicio // 100 - 24) printString]
				ifFalse: 
					[self verHoraInicio // 100 < 10
						ifTrue: ['0' , (self verHoraInicio // 100) printString]
						ifFalse: [(self verHoraInicio // 100) printString]].
	horaFin := self verHoraFin // 100 >= 24
				ifTrue: ['0' , (self verHoraFin // 100 - 24) printString]
				ifFalse: 
					[self verHoraFin // 100 < 10
						ifTrue: ['0' , (self verHoraFin // 100) printString]
						ifFalse: [(self verHoraFin // 100) printString]].
	minsInicio := self verHoraInicio \\ 100 < 10
				ifTrue: ['0' , (self verHoraInicio \\ 100) printString]
				ifFalse: [(self verHoraInicio \\ 100) printString].
	minsFin := self verHoraFin \\ 100 < 10
				ifTrue: ['0' , (self verHoraFin \\ 100) printString]
				ifFalse: [(self verHoraFin \\ 100) printString].
	aStream
		nextPutAll: 'Pelicula: ' , self verPeliculaEnEmision;
		nextPut: Character lf;
		nextPutAll: ' | Dia: ' , dia , ' | Hora de inicio: ' , horaInicio , ':' , minsInicio
					, ' hs. - Hasta: ' , horaFin
					, ':' , minsFin
					, ' hs.'!

setIdFuncionPelicula: unIdFuncionPelicula
	idFuncionPelicula := unIdFuncionPelicula!

verDiaHorario
	^dia!

verHoraFin
	^fin!

verHoraInicio
	^inicio!

verIdFuncionPelicula
	^idFuncionPelicula!

verPeliculaEnEmision
	^peliculaEnEmision! !
!Horario categoriesFor: #comprobarSupHoraria:!public! !
!Horario categoriesFor: #iniHorarioDia:Inicio:horaFin:nombrePelicula:!private! !
!Horario categoriesFor: #iniHorarioDia:Inicio:horaFin:nombrePelicula:idFuncionPelicula:!private! !
!Horario categoriesFor: #printOn:!public! !
!Horario categoriesFor: #setIdFuncionPelicula:!public! !
!Horario categoriesFor: #verDiaHorario!public! !
!Horario categoriesFor: #verHoraFin!public! !
!Horario categoriesFor: #verHoraInicio!public! !
!Horario categoriesFor: #verIdFuncionPelicula!public! !
!Horario categoriesFor: #verPeliculaEnEmision!public! !

!Horario class methodsFor!

crearHorarioDia: unDia Inicio: horaInicio horaFin: horaFin nombrePelicula: nombrePelicula
	^self new
		iniHorarioDia: unDia
		Inicio: horaInicio
		horaFin: horaFin
		nombrePelicula: nombrePelicula
		idFuncionPelicula: ''! !
!Horario class categoriesFor: #crearHorarioDia:Inicio:horaFin:nombrePelicula:!public! !

Pelicula guid: (GUID fromString: '{1053C213-B7AE-43D6-83F4-329D428DEFF7}')!
Pelicula comment: ''!
!Pelicula categoriesForClass!TPI-Cine! !
!Pelicula methodsFor!

agregarFuncionPelicula: unaFuncionPelicula
	(funciones includesKey: unaFuncionPelicula verIdFuncionPelicula)
		ifFalse: 
			[funciones at: unaFuncionPelicula verIdFuncionPelicula put: unaFuncionPelicula.
			^true]
		ifTrue: [^false]!

eliminarFuncionPelicula: idFuncionPelicula
	funciones removeKey: idFuncionPelicula ifAbsent: [^nil]!

iniPeliculaTitulo: unTitulo duracion: minutos formato: unFormato precio: unPrecio
	titulo := unTitulo.
	duracion := minutos.
	formato := unFormato.
	funciones := Dictionary new.
	precio := unPrecio!

modiPrecio: unPrecio
	precio := unPrecio!

printOn: aStream
	aStream nextPutAll: self verTitulo , '  -  ' , self verFormato!

seleccionarFuncionPelicula: idFuncionPelicula
	^funciones at: idFuncionPelicula ifAbsent: [^nil]!

verDuracion
	^duracion!

verFormato
	^formato!

verFunciones
	^funciones!

verPrecio
	^precio!

verTitulo
	^titulo! !
!Pelicula categoriesFor: #agregarFuncionPelicula:!public! !
!Pelicula categoriesFor: #eliminarFuncionPelicula:!public! !
!Pelicula categoriesFor: #iniPeliculaTitulo:duracion:formato:precio:!private! !
!Pelicula categoriesFor: #modiPrecio:!public! !
!Pelicula categoriesFor: #printOn:!public! !
!Pelicula categoriesFor: #seleccionarFuncionPelicula:!public! !
!Pelicula categoriesFor: #verDuracion!public! !
!Pelicula categoriesFor: #verFormato!public! !
!Pelicula categoriesFor: #verFunciones!public! !
!Pelicula categoriesFor: #verPrecio!public! !
!Pelicula categoriesFor: #verTitulo!public! !

!Pelicula class methodsFor!

crearPeliculaTitulo: unTitulo duracion: minutos formato: unFormato precio: unPrecio
	^self new
		iniPeliculaTitulo: unTitulo
		duracion: minutos
		formato: unFormato
		precio: unPrecio! !
!Pelicula class categoriesFor: #crearPeliculaTitulo:duracion:formato:precio:!public! !

Persona guid: (GUID fromString: '{67D6CC25-2B73-4D58-A3EE-D97573BB68AF}')!
Persona comment: ''!
!Persona categoriesForClass!TPI-Cine! !
!Persona methodsFor!

iniPersonaNombre: unNombre apellido: unApellido dni: unDni telefono: unTelefono direccion: unaDireccion email: unEmail
	nombre := unNombre.
	apellido := unApellido.
	dni := unDni.
	telefono := unTelefono.
	direccion := unaDireccion.
	email := unEmail!

modDireccion: unaDireccion
	direccion := unaDireccion!

modEmail: unEmail
	email := unEmail!

modTelefono: unTelefono
	telefono := unTelefono!

verApellido
	^apellido!

verDireccion
	^direccion!

verDni
	^dni!

verEmail
	^email!

verNombre
	^nombre!

verTelefono
	^telefono! !
!Persona categoriesFor: #iniPersonaNombre:apellido:dni:telefono:direccion:email:!private! !
!Persona categoriesFor: #modDireccion:!accessing!public! !
!Persona categoriesFor: #modEmail:!accessing!public! !
!Persona categoriesFor: #modTelefono:!accessing!public! !
!Persona categoriesFor: #verApellido!accessing!public! !
!Persona categoriesFor: #verDireccion!accessing!public! !
!Persona categoriesFor: #verDni!accessing!public! !
!Persona categoriesFor: #verEmail!accessing!public! !
!Persona categoriesFor: #verNombre!accessing!public! !
!Persona categoriesFor: #verTelefono!accessing!public! !

Reserva guid: (GUID fromString: '{550FEF76-40EE-4125-8FDF-55792D9F5664}')!
Reserva comment: ''!
!Reserva categoriesForClass!TPI-Cine! !
!Reserva methodsFor!

idCliente
	^idCliente!

idCliente: unIdCliente
	idCliente := unIdCliente!

iniciarReserva: idReserva idFuncionPelicula: idFuncionPelicula asientos: asientos costo: costo
	thisIdReserva := idReserva.
	thisIdFuncionPelicula := idFuncionPelicula.
	thisAsientos := asientos.
	thisCosto := costo.
	idCliente := -1
	"use el this pq me daba error si tenian el mismo nombre"!

printOn: aStream
	aStream nextPutAll: 'Reserva N°: ' , self verIdReserva printString , '  -  Codigo de función:'
				, self verIdFuncionPelicula!

verAsientos
	^thisAsientos!

verCosto
	^thisCosto!

verIdFuncionPelicula
	^thisIdFuncionPelicula!

verIdReserva
	^thisIdReserva! !
!Reserva categoriesFor: #idCliente!public! !
!Reserva categoriesFor: #idCliente:!public! !
!Reserva categoriesFor: #iniciarReserva:idFuncionPelicula:asientos:costo:!private! !
!Reserva categoriesFor: #printOn:!public! !
!Reserva categoriesFor: #verAsientos!public! !
!Reserva categoriesFor: #verCosto!public! !
!Reserva categoriesFor: #verIdFuncionPelicula!public! !
!Reserva categoriesFor: #verIdReserva!public! !

!Reserva class methodsFor!

crearReserva: idReserva idFuncionPelicula:idFuncionPelicula asientos:asientos costo:costo
	^self new iniciarReserva: idReserva idFuncionPelicula: idFuncionPelicula asientos: asientos costo: costo.
! !
!Reserva class categoriesFor: #crearReserva:idFuncionPelicula:asientos:costo:!public! !

Sala guid: (GUID fromString: '{6EFA1A17-5CE3-4E8A-9BA9-CADE78918F62}')!
Sala comment: ''!
!Sala categoriesForClass!TPI-Cine! !
!Sala methodsFor!

agregarAsiento: unAsiento
	(self seleccionarAsientoFila: unAsiento verFila columna: unAsiento verColumna)
		ifNil: 
			[asientos add: unAsiento.
			^true]
		ifNotNil: [^false]!

agregarHorario: unHorario
	"Si agrega el elemento retorna correcto si no lo agrega retorna el elemento que encontro"

	| h |
	h := false.
	horarios detect: [:hor | (hor comprobarSupHoraria: unHorario) = true]
		ifNone: 
			[horarios add: unHorario.
			h := true].
	^h!

cancelarReserva: idFuncionPelicula fila: unaFila columna: unaColumna
	"si cancela la reserva retorna true si no encontro o el asiento o no existe la reserva retorna nil"

	| asiento flag |
	asiento := asientos detect: [:a | a verFila = unaFila and: [a verColumna = unaColumna]]
				ifNone: [^nil].
	((asiento comprobarReserva: idFuncionPelicula) and: [asiento ~= nil])
		ifTrue: 
			[asiento liberarReserva: idFuncionPelicula.
			flag := true]
		ifFalse: [^nil].
	^flag!

eliminarAsientoFila: unaFila Columna: unaColumna
	| asiento |
	asiento := self seleccionarAsientoFila: unaFila columna: unaColumna.
	asiento
		ifNotNil: 
			[asientos remove: asiento.
			^true]
		ifNil: [^nil]!

eliminarHorarioDia: unDia HoraInicio: unaHoraInicio
	"Elimina el horario que coincida con la hora de inicio y fin y retorna true si lo elimino, si no encuentra ninguno retorna nil"

	| h eliminado |
	eliminado := false.
	h := horarios detect: [:hor | hor verDia = unDia and: [hor verHoraInicio = unaHoraInicio]]
				ifNone: [nil].
	h
		ifNil: [eliminado := nil]
		ifNotNil: 
			[horarios remove: h.
			eliminado := true].
	^eliminado!

eliminarHorarioDia: unDia HoraInicio: unaHoraInicio horaFin: unaHoraFin
	"Elimina el horario que coincida con la hora de inicio y fin y retorna true si lo elimino, si no encuentra ninguno retorna nil"

	| h eliminado |
	eliminado := false.
	h := horarios detect: 
					[:hor |
					hor verDia = unDia and: [hor verHoraInicio = unaHoraInicio and: [hor verHoraFin = unaHoraFin]]]
				ifNone: [nil].
	h
		ifNil: [eliminado := nil]
		ifNotNil: 
			[horarios remove: h.
			eliminado := true].
	^eliminado!

eliminarHorarioDia: unDia HoraInicio: unaHoraInicio Pelicula: nombrePelicula
	"Elimina el horario que coincida con la hora de inicio y fin y retorna true si lo elimino, si no encuentra ninguno retorna nil"

	| h eliminado |
	eliminado := false.
	h := horarios detect: 
					[:hor |
					hor verDia = unDia
						and: [hor verHoraInicio = unaHoraInicio and: [hor verPeliculaEnEmision = nombrePelicula]]]
				ifNone: [nil].
	h
		ifNil: [eliminado := nil]
		ifNotNil: 
			[horarios remove: h.
			eliminado := true].
	^eliminado!

eliminarHorarioIdFuncionPelicula: unIdFuncionPelicula
	"Elimina el horario que coincida con la hora de inicio y fin y retorna true si lo elimino, si no encuentra ninguno retorna nil"

	| h eliminado |
	eliminado := false.
	h := horarios detect: [:hor | hor verIdFuncionPelicula = unIdFuncionPelicula] ifNone: [nil].
	h
		ifNil: [eliminado := nil]
		ifNotNil: 
			[horarios remove: h.
			eliminado := true].
	^eliminado!

iniSalaNombre: unNombre Tipo: unTipo
	nombreSala := unNombre.
	tipo := unTipo.
	asientos := OrderedCollection new.
	horarios := OrderedCollection new!

printOn: aStream
	aStream nextPutAll: 'Sala: ' , self verNombreSala , '  -  Tipo de Sala:' , self verTipo!

reservarAsiento: idFuncionPelicula fila: unaFila columna: unaColumna
	"si se realiza la reserva retorna true, si no se reservo retorna nil"

	| asiento flag |
	flag := false.
	asiento := asientos detect: [:a | a verFila = unaFila and: [a verColumna = unaColumna]]
				ifNone: [^nil].
	asiento notNil
		ifTrue: 
			[(asiento comprobarReserva: idFuncionPelicula)
				ifFalse: 
					[asiento reservar: idFuncionPelicula.
					flag := true]].
	^flag!

seleccionarAsientoFila: unaFila columna: unaColumna
	^asientos detect: [:asi | asi verFila = unaFila and: [asi verColumna = unaColumna]] ifNone: [nil]!

verAsientos
	^asientos!

verAsientosDisp: idPelicula
	^asientos select: [:a | (a comprobarReserva: idPelicula) = false]!

verAsientosRes: idPelicula
	^asientos reject: [:a | (a comprobarReserva: idPelicula) = false]!

verHorarios
	^horarios!

verNombreSala
	^nombreSala!

verTipo
	^tipo! !
!Sala categoriesFor: #agregarAsiento:!public! !
!Sala categoriesFor: #agregarHorario:!public! !
!Sala categoriesFor: #cancelarReserva:fila:columna:!public! !
!Sala categoriesFor: #eliminarAsientoFila:Columna:!public! !
!Sala categoriesFor: #eliminarHorarioDia:HoraInicio:!public! !
!Sala categoriesFor: #eliminarHorarioDia:HoraInicio:horaFin:!public! !
!Sala categoriesFor: #eliminarHorarioDia:HoraInicio:Pelicula:!public! !
!Sala categoriesFor: #eliminarHorarioIdFuncionPelicula:!public! !
!Sala categoriesFor: #iniSalaNombre:Tipo:!public! !
!Sala categoriesFor: #printOn:!public! !
!Sala categoriesFor: #reservarAsiento:fila:columna:!public! !
!Sala categoriesFor: #seleccionarAsientoFila:columna:!public! !
!Sala categoriesFor: #verAsientos!public! !
!Sala categoriesFor: #verAsientosDisp:!public! !
!Sala categoriesFor: #verAsientosRes:!public! !
!Sala categoriesFor: #verHorarios!public! !
!Sala categoriesFor: #verNombreSala!public! !
!Sala categoriesFor: #verTipo!public! !

!Sala class methodsFor!

crearSalaNombre: unNombre Tipo: unTipo
	^self new iniSalaNombre: unNombre Tipo: unTipo! !
!Sala class categoriesFor: #crearSalaNombre:Tipo:!public! !

AsientoReservaModel guid: (GUID fromString: '{43DAEF71-5405-4EF7-8BAD-CB7FDA03E2F6}')!
AsientoReservaModel comment: ''!
!AsientoReservaModel categoriesForClass!MVP-Models! !
!AsientoReservaModel methodsFor!

columna
	^columna!

columna: unaColumna
	columna := unaColumna!

estado
	^estado!

estado: unEstado
	estado := unEstado!

fila
	^fila!

fila: unaFila
	fila := unaFila!

initialize
	fila := 0.
	columna := 0.
	estado := false.! !
!AsientoReservaModel categoriesFor: #columna!public! !
!AsientoReservaModel categoriesFor: #columna:!public! !
!AsientoReservaModel categoriesFor: #estado!public! !
!AsientoReservaModel categoriesFor: #estado:!public! !
!AsientoReservaModel categoriesFor: #fila!public! !
!AsientoReservaModel categoriesFor: #fila:!public! !
!AsientoReservaModel categoriesFor: #initialize!public! !

CineModel guid: (GUID fromString: '{7E7FFA5E-604D-4992-BB62-C0223109D664}')!
CineModel comment: ''!
!CineModel categoriesForClass!MVP-Models! !
!CineModel methodsFor!

actualizarCantidadClientes
	cantidadClientes := cine verClientes size.
	self trigger: #clientesChanged!

actualizarCantidadPeliculas
	cantidadPeliculas := cine verPeliculas size.
	self trigger: #peliculasChanged!

actualizarCantidadSalas
	cantidadSalas := cine verSalas size.
	self trigger: #salasChanged!

actualizarFunciones
	self trigger: #funcionesChanged!

buscarFuncionPelicula: unIdFuncionPelicula
	| pelicula |
	pelicula := cine verPeliculas
				detect: [:peli | (peli seleccionarFuncionPelicula: unIdFuncionPelicula) notNil]
				ifNone: [nil].
	pelicula notNil ifTrue: [^pelicula seleccionarFuncionPelicula: unIdFuncionPelicula] ifFalse: [^nil]!

buscarPelicula: unString
	^cine verPeliculas detect: [:peli | (peli verTitulo findString: unString) > 0] ifNone: [nil]!

buscarPeliculaDeLaFuncion: unIdFuncionPelicula
	^cine verPeliculas detect: [:peli | (peli seleccionarFuncionPelicula: unIdFuncionPelicula) notNil]
		ifNone: [nil]!

buscarPeliculas: unString
	^cine verPeliculas select: [:peli | (peli verTitulo findString: unString) > 0]!

buscarSala: nombreSala
	^cine seleccionarSala: nombreSala!

buscarUnCliente: unString
	^cine verClientes detect: 
			[:cli |
			(cli verIdCliente printString findString: unString) > 0 or: 
					[(cli verDni printString findString: unString) > 0 or: 
							[(cli verNombre asLowercase findString: unString asLowercase) > 0
								or: [(cli verApellido asLowercase findString: unString asLowercase) > 0]]]]
		ifNone: [^nil]!

buscarVariosClientes: unString
	^cine verClientes select: 
			[:cli |
			(cli verIdCliente printString findString: unString) > 0 or: 
					[(cli verDni printString findString: unString) > 0 or: 
							[(cli verNombre asLowercase findString: unString asLowercase) > 0
								or: [(cli verApellido asLowercase findString: unString asLowercase) > 0]]]]!

cantidadClientes
	^cantidadClientes!

cantidadPeliculas
	^cantidadPeliculas!

cantidadSalas
	^cantidadSalas!

crearCine: unNombre
	cine := Cine crearCine: unNombre!

crearCine: unNombre cuit: unCuit direccion: unaDireccion
	cine := Cine
				crearCine: unNombre
				cuit: unCuit
				direccion: unaDireccion!

eliminarFuncionPelicula: unaFuncionPelicula
	cine eliminarFuncion: unaFuncionPelicula.
	self actualizarFunciones!

eliminarPelicula: unaPelicula
	cine eliminarPelicula: unaPelicula.
	self actualizarCantidadPeliculas!

initialize
	cine := Cine
				crearCine: 'Cine'
				cuit: '11-11111111-1'
				direccion: 'Calle 123, Localidad'.
	fechaActual := Date today printString!

nuevaFuncionPelicula: unaFuncionPeliculaModel
	| result |
	result := cine
				nuevaFuncionPeliculaHorario: unaFuncionPeliculaModel horas * 100 + unaFuncionPeliculaModel minutos
				idioma: unaFuncionPeliculaModel idioma
				formato: unaFuncionPeliculaModel formato
				dia: unaFuncionPeliculaModel dia
				pelicula: (cine seleccionarPelicula: unaFuncionPeliculaModel nombrePelicula
						formato: unaFuncionPeliculaModel formato)
				sala: (cine seleccionarSala: unaFuncionPeliculaModel nombreSala).
	self actualizarFunciones.
	^result!

nuevaPelicula: unaPeliculaModel
	| result |
	result := cine
				nuevaPeliculaTitulo: unaPeliculaModel titulo
				duracion: unaPeliculaModel duracion
				formato: unaPeliculaModel formato
				precio: unaPeliculaModel precio.
	self actualizarCantidadPeliculas.
	^result!

nuevaReserva: unaReservaModel
	| result listaAsientos |
	listaAsientos := OrderedCollection new.
	1 to: unaReservaModel asientos size
		do: 
			[:i |
			| asi |
			asi := OrderedCollection new.
			asi add: (unaReservaModel asientos at: i) fila.
			asi add: (unaReservaModel asientos at: i) columna.
			listaAsientos add: asi].
	result := cine
				nuevaReservaIdFuncionPelicula: unaReservaModel idFuncionPelicula
				asientos: listaAsientos
				cliente: (cine seleccionarCliente: unaReservaModel idCliente).
	^result!

nuevaSala: unaSalaModel
	| result |
	result := cine nuevaSalaNombre: unaSalaModel nombre tipoSala: unaSalaModel tipo.
	result
		ifTrue: 
			[cine
				agregarAsientosSala: (cine seleccionarSala: unaSalaModel nombre)
				cantFilas: unaSalaModel cantidadFilas
				cantColumnas: unaSalaModel cantidadColumnas].
	self actualizarCantidadSalas.
	^result!

preset
	"Crear Clientes"

	cine
		nuevoClienteNombre: 'juan'
		apellido: 'perez'
		dni: 25123456
		telefono: '15-1234-1234'
		direccion: 'CalleFalsa123'
		email: 'Juancito@gmail.com'
		nomUsuario: 'juampi'
		contrasenia: 'juanpi123'.
	cine
		nuevoClienteNombre: 'Marcelo'
		apellido: 'Lopez'
		dni: 30521452
		telefono: '15-1234-1234'
		direccion: 'CalleFalsa123'
		email: 'marcelito@gmail.com'
		nomUsuario: 'elmarce'
		contrasenia: 'elmarce123'.
	cine
		nuevoClienteNombre: 'Lionel'
		apellido: 'Messi'
		dni: 18236846
		telefono: '15-1234-1234'
		direccion: 'CalleFalsa123'
		email: 'liomessi@gmail.com'
		nomUsuario: 'messi10'
		contrasenia: 'liomessi123'.



	"Crear salas"
	cine nuevaSalaNombre: 'S1' tipoSala: '2D'.
	cine nuevaSalaNombre: 'S2' tipoSala: '2D'.
	cine nuevaSalaNombre: 'S3' tipoSala: '2D'.
	cine nuevaSalaNombre: 'S4' tipoSala: '2D'.
	cine nuevaSalaNombre: 'S5' tipoSala: '3D'.
	cine nuevaSalaNombre: 'S6' tipoSala: '3D'.
	cine nuevaSalaNombre: 'S7' tipoSala: '3D'.
	cine nuevaSalaNombre: 'S8' tipoSala: '4D'.
	cine nuevaSalaNombre: 'S9' tipoSala: '4D'.
	cine nuevaSalaNombre: 'S10' tipoSala: 'IMAX'.
	cine
		agregarAsientosSala: (cine seleccionarSala: 'S1')
		cantFilas: 12
		cantColumnas: 10.
	cine
		agregarAsientosSala: (cine seleccionarSala: 'S2')
		cantFilas: 12
		cantColumnas: 10.
	cine
		agregarAsientosSala: (cine seleccionarSala: 'S3')
		cantFilas: 12
		cantColumnas: 10.
	cine
		agregarAsientosSala: (cine seleccionarSala: 'S4')
		cantFilas: 12
		cantColumnas: 10.
	cine
		agregarAsientosSala: (cine seleccionarSala: 'S5')
		cantFilas: 12
		cantColumnas: 12.
	cine
		agregarAsientosSala: (cine seleccionarSala: 'S6')
		cantFilas: 12
		cantColumnas: 12.
	cine
		agregarAsientosSala: (cine seleccionarSala: 'S7')
		cantFilas: 12
		cantColumnas: 12.
	cine
		agregarAsientosSala: (cine seleccionarSala: 'S8')
		cantFilas: 8
		cantColumnas: 10.
	cine
		agregarAsientosSala: (cine seleccionarSala: 'S9')
		cantFilas: 8
		cantColumnas: 10.
	cine
		agregarAsientosSala: (cine seleccionarSala: 'S10')
		cantFilas: 15
		cantColumnas: 15.


	"Crear Peliculas"
	cine
		nuevaPeliculaTitulo: 'Superman'
		duracion: 120
		formato: '2D'
		precio: 550.00.
	cine
		nuevaPeliculaTitulo: 'Superman'
		duracion: 120
		formato: '3D'
		precio: 600.00.
	cine
		nuevaPeliculaTitulo: 'Superman'
		duracion: 120
		formato: '4D'
		precio: 800.00.
	cine
		nuevaPeliculaTitulo: 'Superman'
		duracion: 120
		formato: 'IMAX'
		precio: 720.00.
	cine
		nuevaPeliculaTitulo: 'Batman'
		duracion: 135
		formato: '2D'
		precio: 550.00.
	cine
		nuevaPeliculaTitulo: 'The Matrix'
		duracion: 136
		formato: 'IMAX'
		precio: 720.00.
	cine
		nuevaPeliculaTitulo: 'ToyStory'
		duracion: 100
		formato: '2D'
		precio: 550.00.
	cine
		nuevaPeliculaTitulo: 'ToyStory'
		duracion: 100
		formato: '3D'
		precio: 600.00.
	cine
		nuevaPeliculaTitulo: 'Star Wars: A New Hope'
		duracion: 121
		formato: '2D'
		precio: 550.00.
	cine
		nuevaPeliculaTitulo: 'Superman'
		duracion: 121
		formato: '2D'
		precio: 550.00.
	cine
		nuevaPeliculaTitulo: 'Inception'
		duracion: 148
		formato: 'IMAX'
		precio: 750.00.
	cine
		nuevaPeliculaTitulo: 'Titanic'
		duracion: 195
		formato: '2D'
		precio: 550.00.
	cine
		nuevaPeliculaTitulo: 'The Dark Knight'
		duracion: 152
		formato: 'IMAX'
		precio: 720.00.
	cine
		nuevaPeliculaTitulo: 'Avengers: Endgame'
		duracion: 181
		formato: '3D'
		precio: 700.00.
	cine
		nuevaPeliculaTitulo: 'Jurassic Park'
		duracion: 127
		formato: '2D'
		precio: 550.00.
	cine
		nuevaPeliculaTitulo: 'Pulp Fiction'
		duracion: 154
		formato: '2D'
		precio: 550.00.
	cine
		nuevaPeliculaTitulo: 'The Lion King'
		duracion: 88
		formato: '2D'
		precio: 500.00.
	cine
		nuevaPeliculaTitulo: 'Frozen'
		duracion: 102
		formato: '3D'
		precio: 600.00.
	cine
		nuevaPeliculaTitulo: 'Dune'
		duracion: 155
		formato: 'IMAX'
		precio: 750.00.
	cine
		nuevaPeliculaTitulo: 'Avatar'
		duracion: 162
		formato: '3D'
		precio: 650.00.
	cine
		nuevaPeliculaTitulo: 'Interstellar'
		duracion: 169
		formato: '2D'
		precio: 550.00.
	cine
		nuevaPeliculaTitulo: 'John Wick'
		duracion: 130
		formato: 'IMAX'
		precio: 700.00.



	"Crear Funciones"
	cine
		nuevaFuncionPeliculaHorario: 2230
		idioma: 'Español'
		formato: '2D'
		dia: 'Lunes'
		pelicula: (cine seleccionarPelicula: 'Superman' formato: '2D')
		sala: (cine seleccionarSala: 'S1').
	cine
		nuevaFuncionPeliculaHorario: 2230
		idioma: 'Español'
		formato: '3D'
		dia: 'Martes'
		pelicula: (cine seleccionarPelicula: 'Superman' formato: '3D')
		sala: (cine seleccionarSala: 'S5').
	cine
		nuevaFuncionPeliculaHorario: 1800
		idioma: 'Subtitulado (Eng)'
		formato: 'IMAX'
		dia: 'Miércoles'
		pelicula: (cine seleccionarPelicula: 'Inception' formato: 'IMAX')
		sala: (cine seleccionarSala: 'S10').
	cine
		nuevaFuncionPeliculaHorario: 2100
		idioma: 'Español'
		formato: '3D'
		dia: 'Jueves'
		pelicula: (cine seleccionarPelicula: 'Frozen' formato: '3D')
		sala: (cine seleccionarSala: 'S6').
	cine
		nuevaFuncionPeliculaHorario: 1900
		idioma: 'Subtitulado (Eng)'
		formato: '2D'
		dia: 'Viernes'
		pelicula: (cine seleccionarPelicula: 'Jurassic Park' formato: '2D')
		sala: (cine seleccionarSala: 'S4').
	cine
		nuevaFuncionPeliculaHorario: 2000
		idioma: 'Español'
		formato: 'IMAX'
		dia: 'Sábado'
		pelicula: (cine seleccionarPelicula: 'The Dark Knight' formato: 'IMAX')
		sala: (cine seleccionarSala: 'S10').
	cine
		nuevaFuncionPeliculaHorario: 2000
		idioma: 'Subtitulado (Eng)'
		formato: 'IMAX'
		dia: 'Lunes'
		pelicula: (cine seleccionarPelicula: 'Dune' formato: 'IMAX')
		sala: (cine seleccionarSala: 'S10').
	cine
		nuevaFuncionPeliculaHorario: 2200
		idioma: 'Español'
		formato: '3D'
		dia: 'Sábado'
		pelicula: (cine seleccionarPelicula: 'Avatar' formato: '3D')
		sala: (cine seleccionarSala: 'S6').
	cine
		nuevaFuncionPeliculaHorario: 1500
		idioma: 'Subtitulado (Eng)'
		formato: 'IMAX'
		dia: 'Lunes'
		pelicula: (cine seleccionarPelicula: 'Inception' formato: 'IMAX')
		sala: (cine seleccionarSala: 'S10').
	cine
		nuevaFuncionPeliculaHorario: 1800
		idioma: 'Español'
		formato: 'IMAX'
		dia: 'Miércoles'
		pelicula: (cine seleccionarPelicula: 'Inception' formato: 'IMAX')
		sala: (cine seleccionarSala: 'S10').
	cine
		nuevaFuncionPeliculaHorario: 2100
		idioma: 'Subtitulado (Eng)'
		formato: 'IMAX'
		dia: 'Viernes'
		pelicula: (cine seleccionarPelicula: 'Inception' formato: 'IMAX')
		sala: (cine seleccionarSala: 'S10').
	cine
		nuevaFuncionPeliculaHorario: 1400
		idioma: 'Español'
		formato: '2D'
		dia: 'Martes'
		pelicula: (cine seleccionarPelicula: 'Titanic' formato: '2D')
		sala: (cine seleccionarSala: 'S4').
	cine
		nuevaFuncionPeliculaHorario: 1900
		idioma: 'Español'
		formato: '2D'
		dia: 'Jueves'
		pelicula: (cine seleccionarPelicula: 'Titanic' formato: '2D')
		sala: (cine seleccionarSala: 'S4').
	cine
		nuevaFuncionPeliculaHorario: 2200
		idioma: 'Español'
		formato: '2D'
		dia: 'Sábado'
		pelicula: (cine seleccionarPelicula: 'Titanic' formato: '2D')
		sala: (cine seleccionarSala: 'S4').
	cine
		nuevaFuncionPeliculaHorario: 1700
		idioma: 'Subtitulado (Eng)'
		formato: 'IMAX'
		dia: 'Domingo'
		pelicula: (cine seleccionarPelicula: 'The Dark Knight' formato: 'IMAX')
		sala: (cine seleccionarSala: 'S10').
	cine
		nuevaFuncionPeliculaHorario: 2000
		idioma: 'Español'
		formato: 'IMAX'
		dia: 'Viernes'
		pelicula: (cine seleccionarPelicula: 'The Dark Knight' formato: 'IMAX')
		sala: (cine seleccionarSala: 'S10').
	cine
		nuevaFuncionPeliculaHorario: 2300
		idioma: 'Subtitulado (Eng)'
		formato: 'IMAX'
		dia: 'Domingo'
		pelicula: (cine seleccionarPelicula: 'The Dark Knight' formato: 'IMAX')
		sala: (cine seleccionarSala: 'S10').
	cine
		nuevaFuncionPeliculaHorario: 1600
		idioma: 'Español'
		formato: '3D'
		dia: 'Miércoles'
		pelicula: (cine seleccionarPelicula: 'Frozen' formato: '3D')
		sala: (cine seleccionarSala: 'S6').
	cine
		nuevaFuncionPeliculaHorario: 1900
		idioma: 'Español'
		formato: '3D'
		dia: 'Viernes'
		pelicula: (cine seleccionarPelicula: 'Frozen' formato: '3D')
		sala: (cine seleccionarSala: 'S6').
	cine
		nuevaFuncionPeliculaHorario: 2100
		idioma: 'Español'
		formato: '3D'
		dia: 'Domingo'
		pelicula: (cine seleccionarPelicula: 'Frozen' formato: '3D')
		sala: (cine seleccionarSala: 'S6').
	self actualizarCantidadClientes.
	self actualizarCantidadPeliculas.
	self actualizarCantidadSalas!

verAsientosDisponibles: unIdFuncionPelicula sala: nombreSala
	| asientosDisp lista |
	lista := OrderedCollection new.
	asientosDisp := (cine seleccionarSala: nombreSala) verAsientosDisp: unIdFuncionPelicula.
	1 to: asientosDisp size
		do: 
			[:i |
			| asi |
			asi := OrderedCollection new.
			asi add: (asientosDisp at: i) verFila.
			asi add: (asientosDisp at: i) verColumna.
			asi add: false.
			lista add: asi].
	^lista!

verAsientosReservados: unIdFuncionPelicula sala: nombreSala
	| asientosRes lista |
	lista := OrderedCollection new.
	asientosRes := (cine seleccionarSala: nombreSala) verAsientosRes: unIdFuncionPelicula.
	1 to: asientosRes size
		do: 
			[:i |
			| asi |
			asi := OrderedCollection new.
			asi add: (asientosRes at: i) verFila.
			asi add: (asientosRes at: i) verColumna.
			asi add: false.
			lista add: asi].
	^lista!

verClientes
	^cine verClientes.!

verCuit
	^cine verCuit!

verDireccion
	^cine verDireccion!

verFechaActual
	^fechaActual!

verFunciones: unaPelicula
	| funciones dias |
	dias := Dictionary new.
	dias at: 'Lunes' put: 1.
	dias at: 'Martes' put: 2.
	dias at: 'Miércoles' put: 3.
	dias at: 'Jueves' put: 4.
	dias at: 'Viernes' put: 5.
	dias at: 'Sábado' put: 6.
	dias at: 'Domingo' put: 7.
	funciones := (cine verFuncionesDePelicula: unaPelicula) asSortedCollection: 
					[:x :y |
					(x mayorTitulo: y) or: [(x mayorDia: y) or: [(x mayorHorario: y) or: [x mayorSala: y]]]].
	^funciones!

verFuncionesFiltradasPelicula: unaPelicula idioma: unIdioma boolLunes: unBoolLunes boolMartes: unBoolMartes boolMiercoles: unBoolMiercoles boolJueves: unBoolJueves boolViernes: unBoolViernes boolSabado: unBoolSabado boolDomingo: unBoolDomingo
	"Segun el idioma"
	| funcionesAMostrar |
	unIdioma = 'Todos'
		ifTrue: [funcionesAMostrar := self verFunciones: unaPelicula]
		ifFalse: 
			[funcionesAMostrar := (self verFunciones: unaPelicula) select: [:fun | fun verIdioma = unIdioma]].
	"Segun los dias"
	unBoolLunes ifFalse: [funcionesAMostrar := funcionesAMostrar reject: [:fun | fun verDia = 'Lunes']].
	unBoolMartes
		ifFalse: [funcionesAMostrar := funcionesAMostrar reject: [:fun | fun verDia = 'Martes']].
	unBoolMiercoles
		ifFalse: [funcionesAMostrar := funcionesAMostrar reject: [:fun | fun verDia = 'Miércoles']].
	unBoolJueves
		ifFalse: [funcionesAMostrar := funcionesAMostrar reject: [:fun | fun verDia = 'Jueves']].
	unBoolViernes
		ifFalse: [funcionesAMostrar := funcionesAMostrar reject: [:fun | fun verDia = 'Viernes']].
	unBoolSabado
		ifFalse: [funcionesAMostrar := funcionesAMostrar reject: [:fun | fun verDia = 'Sábado']].
	unBoolDomingo
		ifFalse: [funcionesAMostrar := funcionesAMostrar reject: [:fun | fun verDia = 'Domingo']].
	^funcionesAMostrar!

verNombre
	^cine verNombre!

verNombreSalas
	^cine verSalas collect: [:sala | sala verNombreSala]!

verNombreSalasFormato: unFormato
	| nombreSalas |
	nombreSalas := cine verSalas select: [:sala | sala verTipo = unFormato].
	^nombreSalas collect: [:sala | sala verNombreSala]!

verPeliculas
	| peliculas |
	peliculas := cine verPeliculas asSortedCollection: 
					[:x :y |
					x verTitulo < y verTitulo or: [x verTitulo = y verTitulo and: [x verFormato < y verFormato]]].
	^peliculas!

verPeliculasFiltradasBool2D: unBool2D bool3D: unBool3D bool4D: unBool4D boolIMAX: unBoolIMAX
	| cartelera |
	cartelera := unBool2D
				ifTrue: [self verPeliculas]
				ifFalse: [self verPeliculas reject: [:peli | peli verFormato = '2D']].
	cartelera := unBool3D
				ifTrue: [cartelera]
				ifFalse: [cartelera reject: [:peli | peli verFormato = '3D']].
	cartelera := unBool4D
				ifTrue: [cartelera]
				ifFalse: [cartelera reject: [:peli | peli verFormato = '4D']].
	cartelera := unBoolIMAX
				ifTrue: [cartelera]
				ifFalse: [cartelera reject: [:peli | peli verFormato = 'IMAX']].
	^cartelera!

verSalas
	^cine verSalas.!

verSalasFiltradasBool2D: unBool2D bool3D: unBool3D bool4D: unBool4D boolIMAX: unBoolImax
	| salasAMostrar |
	salasAMostrar := unBool2D
				ifTrue: [self verSalas]
				ifFalse: [self verSalas reject: [:sala | sala verTipo = '2D']].
	salasAMostrar := unBool3D
				ifTrue: [salasAMostrar]
				ifFalse: [salasAMostrar reject: [:sala | sala verTipo = '3D']].
	salasAMostrar := unBool4D
				ifTrue: [salasAMostrar]
				ifFalse: [salasAMostrar reject: [:sala | sala verTipo = '4D']].
	salasAMostrar := unBoolImax
				ifTrue: [salasAMostrar]
				ifFalse: [salasAMostrar reject: [:sala | sala verTipo = 'IMAX']].
	^salasAMostrar! !
!CineModel categoriesFor: #actualizarCantidadClientes!public! !
!CineModel categoriesFor: #actualizarCantidadPeliculas!public! !
!CineModel categoriesFor: #actualizarCantidadSalas!public! !
!CineModel categoriesFor: #actualizarFunciones!public! !
!CineModel categoriesFor: #buscarFuncionPelicula:!public! !
!CineModel categoriesFor: #buscarPelicula:!public! !
!CineModel categoriesFor: #buscarPeliculaDeLaFuncion:!public! !
!CineModel categoriesFor: #buscarPeliculas:!public! !
!CineModel categoriesFor: #buscarSala:!public! !
!CineModel categoriesFor: #buscarUnCliente:!public! !
!CineModel categoriesFor: #buscarVariosClientes:!public! !
!CineModel categoriesFor: #cantidadClientes!public! !
!CineModel categoriesFor: #cantidadPeliculas!public! !
!CineModel categoriesFor: #cantidadSalas!public! !
!CineModel categoriesFor: #crearCine:!public! !
!CineModel categoriesFor: #crearCine:cuit:direccion:!public! !
!CineModel categoriesFor: #eliminarFuncionPelicula:!public! !
!CineModel categoriesFor: #eliminarPelicula:!public! !
!CineModel categoriesFor: #initialize!public! !
!CineModel categoriesFor: #nuevaFuncionPelicula:!public! !
!CineModel categoriesFor: #nuevaPelicula:!public! !
!CineModel categoriesFor: #nuevaReserva:!public! !
!CineModel categoriesFor: #nuevaSala:!public! !
!CineModel categoriesFor: #preset!public! !
!CineModel categoriesFor: #verAsientosDisponibles:sala:!public! !
!CineModel categoriesFor: #verAsientosReservados:sala:!public! !
!CineModel categoriesFor: #verClientes!public! !
!CineModel categoriesFor: #verCuit!public! !
!CineModel categoriesFor: #verDireccion!public! !
!CineModel categoriesFor: #verFechaActual!public! !
!CineModel categoriesFor: #verFunciones:!public! !
!CineModel categoriesFor: #verFuncionesFiltradasPelicula:idioma:boolLunes:boolMartes:boolMiercoles:boolJueves:boolViernes:boolSabado:boolDomingo:!public! !
!CineModel categoriesFor: #verNombre!public! !
!CineModel categoriesFor: #verNombreSalas!public! !
!CineModel categoriesFor: #verNombreSalasFormato:!public! !
!CineModel categoriesFor: #verPeliculas!public! !
!CineModel categoriesFor: #verPeliculasFiltradasBool2D:bool3D:bool4D:boolIMAX:!public! !
!CineModel categoriesFor: #verSalas!public! !
!CineModel categoriesFor: #verSalasFiltradasBool2D:bool3D:bool4D:boolIMAX:!public! !

!CineModel class methodsFor!

publishedEventsOfInstances
	^(super publishedEventsOfInstances)
		add: #peliculasChanged;
		add: #salasChanged;
		add: #clientesChanged;
		add: #funcionesChanged;
		add: #reservasChanged;
		yourself! !
!CineModel class categoriesFor: #publishedEventsOfInstances!public! !

FuncionPeliculaModel guid: (GUID fromString: '{0154C478-94A6-4C64-88CF-0C40042B2FE0}')!
FuncionPeliculaModel comment: ''!
!FuncionPeliculaModel categoriesForClass!MVP-Models! !
!FuncionPeliculaModel methodsFor!

dia
	^diaElegido!

dia: unDia
	diaElegido := unDia!

dias
	^dias!

dias: listaDias
	dias := listaDias!

formato
	^formato!

formato: unFormato
	formato := unFormato!

horas
	^horas!

horas: cantHoras
	horas := cantHoras!

idioma
	^idiomaElegido!

idioma: unIdioma
	idiomaElegido := unIdioma!

idiomas
	^idiomas!

idiomas: listaIdiomas
	idiomas := listaIdiomas!

initialize
	nombreSala := String new.
	nombrePelicula := String new.
	idiomas := String new.
	dias := String new.
	formato := String new.
	horas := 08.
	minutos := 00!

minutos
	^minutos!

minutos: cantMinutos
	minutos := cantMinutos!

nombrePelicula
	^nombrePelicula!

nombrePelicula: unTitulo
	nombrePelicula := unTitulo!

nombreSala
	^nombreSala!

nombreSala: unNombreSala
	nombreSala := unNombreSala!

salas
	^salas!

salas: lisaSalas
	salas := lisaSalas! !
!FuncionPeliculaModel categoriesFor: #dia!public! !
!FuncionPeliculaModel categoriesFor: #dia:!public! !
!FuncionPeliculaModel categoriesFor: #dias!public! !
!FuncionPeliculaModel categoriesFor: #dias:!public! !
!FuncionPeliculaModel categoriesFor: #formato!public! !
!FuncionPeliculaModel categoriesFor: #formato:!public! !
!FuncionPeliculaModel categoriesFor: #horas!public! !
!FuncionPeliculaModel categoriesFor: #horas:!public! !
!FuncionPeliculaModel categoriesFor: #idioma!public! !
!FuncionPeliculaModel categoriesFor: #idioma:!public! !
!FuncionPeliculaModel categoriesFor: #idiomas!public! !
!FuncionPeliculaModel categoriesFor: #idiomas:!public! !
!FuncionPeliculaModel categoriesFor: #initialize!private! !
!FuncionPeliculaModel categoriesFor: #minutos!public! !
!FuncionPeliculaModel categoriesFor: #minutos:!public! !
!FuncionPeliculaModel categoriesFor: #nombrePelicula!public! !
!FuncionPeliculaModel categoriesFor: #nombrePelicula:!public! !
!FuncionPeliculaModel categoriesFor: #nombreSala!public! !
!FuncionPeliculaModel categoriesFor: #nombreSala:!public! !
!FuncionPeliculaModel categoriesFor: #salas!public! !
!FuncionPeliculaModel categoriesFor: #salas:!public! !

PeliculaModel guid: (GUID fromString: '{37C9C1A7-F002-478B-A18D-EE5C46E140B1}')!
PeliculaModel comment: ''!
!PeliculaModel categoriesForClass!MVP-Models! !
!PeliculaModel methodsFor!

addFormato: unFormato
	formato := unFormato.
	^unFormato!

duracion
	^duracion!

duracion: unaDuracion
	duracion := unaDuracion!

formato
	^formato!

formato: unFormato
	formato := unFormato!

formatos
	^formatos!

initialize
	titulo := String new.
	duracion := 0.
	precio := 0.00.
	formato := String new!

precio
	^precio!

precio: unPrecio
	precio := unPrecio!

titulo
	^titulo!

titulo: unTitulo
	titulo := unTitulo! !
!PeliculaModel categoriesFor: #addFormato:!public! !
!PeliculaModel categoriesFor: #duracion!public! !
!PeliculaModel categoriesFor: #duracion:!public! !
!PeliculaModel categoriesFor: #formato!public! !
!PeliculaModel categoriesFor: #formato:!public! !
!PeliculaModel categoriesFor: #formatos!public! !
!PeliculaModel categoriesFor: #initialize!private! !
!PeliculaModel categoriesFor: #precio!public! !
!PeliculaModel categoriesFor: #precio:!public! !
!PeliculaModel categoriesFor: #titulo!public! !
!PeliculaModel categoriesFor: #titulo:!public! !

ReservaModel guid: (GUID fromString: '{F6D22238-7C31-4273-A864-4EECA04FC5C4}')!
ReservaModel comment: ''!
!ReservaModel categoriesForClass!MVP-Models! !
!ReservaModel methodsFor!

asientos
	^asientos!

asientos: listaAsientos
	asientos := listaAsientos!

cantAsientosSeleccionados
	asientos size > 0
		ifTrue: [^(asientos collect: [:asiento | asiento estado]) occurrencesOf: true]
		ifFalse: [^0]!

idCliente
	^idCliente!

idCliente: unIdCliente
	idCliente := unIdCliente!

idFuncionPelicula
	^idFuncionPelicula!

idFuncionPelicula: unIdFuncionPelicula
	idFuncionPelicula := unIdFuncionPelicula!

initialize
	idFuncionPelicula := String new.
	asientos := OrderedCollection new.
	idCliente := 0! !
!ReservaModel categoriesFor: #asientos!public! !
!ReservaModel categoriesFor: #asientos:!public! !
!ReservaModel categoriesFor: #cantAsientosSeleccionados!public! !
!ReservaModel categoriesFor: #idCliente!public! !
!ReservaModel categoriesFor: #idCliente:!public! !
!ReservaModel categoriesFor: #idFuncionPelicula!public! !
!ReservaModel categoriesFor: #idFuncionPelicula:!public! !
!ReservaModel categoriesFor: #initialize!public! !

SalaModel guid: (GUID fromString: '{4A7EF8D2-7E8E-462B-8ED0-4A886C5CF4AA}')!
SalaModel comment: ''!
!SalaModel categoriesForClass!MVP-Models! !
!SalaModel methodsFor!

cantidadColumnas
	^cantidadColumnas!

cantidadColumnas: cantColumnas
	cantidadColumnas := cantColumnas!

cantidadFilas
	^cantidadFilas!

cantidadFilas: cantFilas
	cantidadFilas := cantFilas!

initialize
	nombre := String new.
	tipo := String new.
	cantidadFilas := 0.
	cantidadColumnas := 0!

nombre
	^nombre!

nombre: unNombre
	nombre := unNombre!

tipo
	^tipo!

tipo: unTipo
	tipo := unTipo! !
!SalaModel categoriesFor: #cantidadColumnas!public! !
!SalaModel categoriesFor: #cantidadColumnas:!public! !
!SalaModel categoriesFor: #cantidadFilas!public! !
!SalaModel categoriesFor: #cantidadFilas:!public! !
!SalaModel categoriesFor: #initialize!private! !
!SalaModel categoriesFor: #nombre!public! !
!SalaModel categoriesFor: #nombre:!public! !
!SalaModel categoriesFor: #tipo!public! !
!SalaModel categoriesFor: #tipo:!public! !

Cliente guid: (GUID fromString: '{AA0E5212-BA8B-4021-A05F-D81F3420DB9D}')!
Cliente comment: ''!
!Cliente categoriesForClass!TPI-Cine! !
!Cliente methodsFor!

iniCliente: id nombre: unNombre apellido: unApellido dni: unDni telefono: unTelefono direccion: unaDireccion email: unEmail nomUsuario: nomUsuario contrasenia: contrasenia
	super
		iniPersonaNombre: unNombre
		apellido: unApellido
		dni: unDni
		telefono: unTelefono
		direccion: unaDireccion
		email: unEmail.
	thisIdCliente := id.
	thisCuenta := CuentaCliente crearCuentaCliente: nomUsuario contrasenia: contrasenia!

printOn: aStream
	aStream nextPutAll: 'Nombre: ' , self verNombre , '   Apellido: ' , self verApellido , '   DNI: '
				, self verDni printString!

verCuenta
	^thisCuenta!

verIdCliente
	^thisIdCliente.!

verReservas
	"Devuelve las reservas realizadas por el cliente"

	^self verCuenta verReservas select: [:r | r verCosto > 0]! !
!Cliente categoriesFor: #iniCliente:nombre:apellido:dni:telefono:direccion:email:nomUsuario:contrasenia:!private! !
!Cliente categoriesFor: #printOn:!public! !
!Cliente categoriesFor: #verCuenta!public! !
!Cliente categoriesFor: #verIdCliente!public! !
!Cliente categoriesFor: #verReservas!public! !

!Cliente class methodsFor!

crearClienteId: id nombre: nombre apellido: apellido dni: dni telefono: telefono direccion: direccion email: email noUsuario: nomUsuario contrasenia: contrasenia
	^self new
		iniCliente: id
		nombre: nombre
		apellido: apellido
		dni: dni
		telefono: telefono
		direccion: direccion
		email: email
		nomUsuario: nomUsuario
		contrasenia: contrasenia! !
!Cliente class categoriesFor: #crearClienteId:nombre:apellido:dni:telefono:direccion:email:noUsuario:contrasenia:!public! !

CineShell guid: (GUID fromString: '{4E3BD062-F9F5-4102-BC11-D94461873933}')!
CineShell comment: ''!
!CineShell categoriesForClass!MVP-Presenters! !
!CineShell methodsFor!

actualizarFunciones
	self hasSelectedPelicula
		ifTrue: 
			[| funcionesAMostrar |
			funcionesAMostrar := model
						verFuncionesFiltradasPelicula: peliculas selection
						idioma: idiomas value
						boolLunes: lunes value
						boolMartes: martes value
						boolMiercoles: miercoles value
						boolJueves: jueves value
						boolViernes: viernes value
						boolSabado: sabado value
						boolDomingo: domingo value.
			funciones model: funcionesAMostrar]
		ifFalse: [funciones model: OrderedCollection new].
	infoPeliculaAbierta := infoPeliculaAbierta reject: [:ventana | ventana view isKindOf: DeafObject].
	1 to: infoPeliculaAbierta size
		do: [:ventana | (infoPeliculaAbierta at: ventana) actualizarFunciones]!

actualizarPeliculas
	| cartelera |
	cartelera := model
				verPeliculasFiltradasBool2D: f2D value
				bool3D: f3D value
				bool4D: f4D value
				boolIMAX: fIMAX value.
	cantidadPeliculasMostradas model: cartelera size.
	peliculas model: cartelera!

createComponents
	super createComponents.

	"ventanas info"
	infoPeliculaAbierta := OrderedCollection new.

	"Texto"
	nombreCine := self add: TextPresenter new name: 'nombre'.
	cuit := self add: TextPresenter new name: 'cuit'.
	direccion := self add: TextPresenter new name: 'direccion'.
	fechaActual := self add: TextPresenter new name: 'fechaActual'.
	cantidadClientes := self add: TextPresenter new name: 'cantidadClientes'.
	cantidadSalas := self add: TextPresenter new name: 'cantidadSalas'.
	cantidadPeliculasTotales := self add: TextPresenter new name: 'cantidadPeliculasTotales'.
	cantidadPeliculasMostradas := self add: TextPresenter new name: 'cantidadPeliculasMostradas'.
	cantidadFunciones := self add: TextPresenter new name: 'cantidadFunciones'.

	"Listas"
	peliculas := self add: ListPresenter new name: 'peliculas'.
	funciones := self add: ListPresenter new name: 'funciones'.

	"Choises"
	idiomas := self add: ChoicePresenter new name: 'idiomas'.

	"Checkboxes"
	lunes := self add: BooleanPresenter new name: 'lunes'.
	martes := self add: BooleanPresenter new name: 'martes'.
	miercoles := self add: BooleanPresenter new name: 'miercoles'.
	jueves := self add: BooleanPresenter new name: 'jueves'.
	viernes := self add: BooleanPresenter new name: 'viernes'.
	sabado := self add: BooleanPresenter new name: 'sabado'.
	domingo := self add: BooleanPresenter new name: 'domingo'.
	f2D := self add: BooleanPresenter new name: '2D'.
	f3D := self add: BooleanPresenter new name: '3D'.
	f4D := self add: BooleanPresenter new name: '4D'.
	fIMAX := self add: BooleanPresenter new name: 'IMAX'.

	"inicializaxion de los checkboxes en true"
	f2D value: true.
	f3D value: true.
	f4D value: true.
	fIMAX value: true.
	lunes value: true.
	martes value: true.
	miercoles value: true.
	jueves value: true.
	viernes value: true.
	sabado value: true.
	domingo value: true!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	model
		when: #peliculasChanged
		send: #actualizarPeliculas
		to: self.
	model
		when: #funcionesChanged
		send: #actualizarFunciones
		to: subPresenters.
	peliculas
		when: #actionPerformed
		send: #infoPelicula
		to: self.
	peliculas
		when: #selectionChanged
		send: #actualizarFunciones
		to: self.
	funciones
		when: #actionPerformed
		send: #infoFuncion
		to: self.
	cantidadPeliculasTotales
		when: #valueChanged
		send: #actualizarPeliculas
		to: self.
	"Actualizar Peliculas"
	f2D
		when: #valueChanged
		send: #actualizarPeliculas
		to: self.
	f3D
		when: #valueChanged
		send: #actualizarPeliculas
		to: self.
	f4D
		when: #valueChanged
		send: #actualizarPeliculas
		to: self.
	fIMAX
		when: #valueChanged
		send: #actualizarPeliculas
		to: self.

	"Actualizar Funciones"
	idiomas
		when: #selectionChanged
		send: #actualizarFunciones
		to: self.
	lunes
		when: #valueChanged
		send: #actualizarFunciones
		to: self.
	martes
		when: #valueChanged
		send: #actualizarFunciones
		to: self.
	miercoles
		when: #valueChanged
		send: #actualizarFunciones
		to: self.
	jueves
		when: #valueChanged
		send: #actualizarFunciones
		to: self.
	viernes
		when: #valueChanged
		send: #actualizarFunciones
		to: self.
	sabado
		when: #valueChanged
		send: #actualizarFunciones
		to: self.
	domingo
		when: #valueChanged
		send: #actualizarFunciones
		to: self!

eliminarFuncionPelicula
	| funcionAeliminar confirmado |
	funcionAeliminar := funciones selection.
	confirmado := MessageBox
				confirm: '¿Seguro desea eliminar la funcion? 
' , funcionAeliminar verIdFuncionPelicula.
	confirmado
		ifTrue: 
			[model eliminarFuncionPelicula: funcionAeliminar.
			self actualizarFunciones]!

eliminarPelicula
	| peliculaAeliminar confirmado funcionesPelicula |
	peliculaAeliminar := peliculas selection.
	funcionesPelicula := String streamContents: 
					[:s |
					s
						nextPut: Character lf;
						nextPutAll: 'Se eliminaran las siguientes funciones: '].
	peliculaAeliminar verFunciones keysDo: 
			[:key |
			funcionesPelicula := funcionesPelicula , (peliculaAeliminar verFunciones at: key) printString , '.'].
	confirmado := MessageBox
				confirm: '¿Seguro desea eliminar la Pelicula? 
' , peliculaAeliminar verTitulo , ' '
						, peliculaAeliminar verFormato , funcionesPelicula.
	confirmado
		ifTrue: 
			[model eliminarPelicula: peliculaAeliminar.
			self actualizarPeliculas]!

fechaActual
	^Date today printString!

hasSelectedFuncion
	^funciones hasSelection!

hasSelectedPelicula
	^peliculas hasSelection!

infoFuncion
	InfoFuncionPelicula showOn: funciones selection!

infoPelicula
	infoPeliculaAbierta add: (InfoPelicula showOn: peliculas selection)!

menuClientes
	!

menuSalas
	"Crea una instancia unica de MenuSalas. cuando la ventana del menu de salas se cierra solo se elimina la view. esto es para evitar estar creando infinitas instancias.
	una vez creada la instancia se guarda en la variable menuSala. y al cerrar la ventana solamente se elimina la view"

	menuSala notNil
		ifTrue: 
			["Si existe una ventana abierta (existe una view) no hace nada. Si no existe crea una view"
			(menuSala view isKindOf: DeafObject) ifTrue: [menuSala createView: 'Default view'].
			menuSala show]
		ifFalse: [menuSala := MenuSalas show model: model]!

model: unCineModel
	super model: unCineModel.
	nombreCine model: unCineModel verNombre.
	cuit model: unCineModel verCuit.
	direccion model: unCineModel verDireccion.
	fechaActual model: unCineModel verFechaActual.
	peliculas model: unCineModel verPeliculas.
	cantidadSalas model: (unCineModel aspectValue: #cantidadSalas).
	cantidadPeliculasTotales model: (unCineModel aspectValue: #cantidadPeliculas).
	cantidadPeliculasMostradas model: unCineModel verPeliculas size.
	cantidadClientes model: (unCineModel aspectValue: #cantidadClientes).
	cantidadSalas model aspectTriggers: #salasChanged.
	cantidadPeliculasTotales model aspectTriggers: #peliculasChanged.
	cantidadClientes model aspectTriggers: #clientesChanged.
	cantidadFunciones model: (unCineModel aspectValue: #cantidadPeliculas).
	cantidadFunciones model aspectTriggers: #funcionesChanged!

nuevaFuncionPelicula
	| nuevaFuncionPelicula fpmodel pelicula |
	pelicula := peliculas selection.
	fpmodel := FuncionPeliculaModel new.
	fpmodel formato: pelicula verFormato.
	fpmodel nombrePelicula: pelicula verTitulo.
	fpmodel salas: (model verNombreSalasFormato: pelicula verFormato).
	(nuevaFuncionPelicula := CineNuevaFuncionPelicula showModalOn: fpmodel) notNil
		ifTrue: 
			[(nuevaFuncionPelicula nombreSala ~= '' and: 
					[nuevaFuncionPelicula nombrePelicula ~= '' and: 
							[nuevaFuncionPelicula formato ~= '' and: 
									[nuevaFuncionPelicula dia ~= '' and: 
											[(nuevaFuncionPelicula horas between: 8 and: 24)
												and: [(nuevaFuncionPelicula minutos between: 0 and: 59) and: [nuevaFuncionPelicula idioma ~= '']]]]]])
				ifTrue: 
					[(self model nuevaFuncionPelicula: nuevaFuncionPelicula)
						ifTrue: [MessageBox warning: 'Funcion agregada exitosamente']
						ifFalse: [MessageBox warning: 'Ya existe otra funcion en ese horario']]
				ifFalse: [MessageBox warning: 'Los datos ingresados son incorrectos']].
	self actualizarFunciones!

nuevaPelicula
	| nuevaPelicula |
	(nuevaPelicula := CineNuevaPelicula showModal) notNil
		ifTrue: 
			[(nuevaPelicula titulo ~= '' and: 
					[(nuevaPelicula duracion between: 1 and: 300) and: 
							[nuevaPelicula formato ~= '' and: [nuevaPelicula precio isNumber and: [nuevaPelicula precio > 0.0]]]])
				ifTrue: [self model nuevaPelicula: nuevaPelicula]
				ifFalse: [MessageBox warning: 'Los datos ingresados son incorrectos']]
	"self actualizarPeliculas"!

nuevaReserva
	| resModel nuevaReserva |
	resModel := ReservaModel new.
	resModel idFuncionPelicula: funciones selection verIdFuncionPelicula.
	(nuevaReserva := CineNuevaReserva showModalOn: resModel) notNil
		ifTrue: 
			["Filtra unicamente los asientos que fueron seleccionado (estado = true)"
			nuevaReserva asientos: (nuevaReserva asientos select: [:asi | asi estado]).
			"Verigica que se hayan ingresado los datos de id de la pelicula, se haya seleccionado un cliente y se haya selecionado al menos un asiento"
			(nuevaReserva idFuncionPelicula ~= ''
				and: [nuevaReserva idCliente > 0 and: [nuevaReserva asientos size > 0]])
					ifTrue: 
						[(self model nuevaReserva: nuevaReserva)
							ifTrue: [MessageBox warning: 'Reserva agregada exitosamente']
							ifFalse: [MessageBox warning: 'No se ha podido realizar la reserva']]
					ifFalse: [MessageBox warning: 'Los datos ingresados son incorrectos']]!

onViewOpened
	super onViewOpened.
	self view text: 'ProyectoCine UNAJ - Grupo 7'.
	idiomas
		choices: #('Todos' 'Español' 'Subtitulado (Eng)' 'Subtitulado (Port)' 'Subtitulado (Jap)' 'Subtitulado (Kor)' 'Subtitulado (Fr)');
		value: 'Todos'.
	f2D value: true.
	f3D value: true.
	f4D value: true.
	fIMAX value: true.
	lunes value: true.
	martes value: true.
	miercoles value: true.
	jueves value: true.
	viernes value: true.
	sabado value: true.
	domingo value: true!

puedeReservarFuncion
	^self hasSelectedFuncion and: 
			[((model buscarSala: funciones selection verNombreSalaAsignada)
				verAsientosDisp: funciones selection verIdFuncionPelicula) size
				> 0 and: [cantidadClientes value > 0]]!

queryCommand: aCommandQuery
	"Enters details about a potential command for the receiver into aCommandQuery"

	super queryCommand: aCommandQuery.
	(#(#eliminarPelicula #infoPelicula #nuevaFuncionPelicula) includes: aCommandQuery command)
		ifTrue: [aCommandQuery isEnabled: self hasSelectedPelicula].
	(#(#nuevaReserva) includes: aCommandQuery command)
		ifTrue: [aCommandQuery isEnabled: self puedeReservarFuncion].
	(#(#eliminarFuncionPelicula #infoFuncion) includes: aCommandQuery command)
		ifTrue: [aCommandQuery isEnabled: self hasSelectedFuncion]! !
!CineShell categoriesFor: #actualizarFunciones!public! !
!CineShell categoriesFor: #actualizarPeliculas!public! !
!CineShell categoriesFor: #createComponents!public! !
!CineShell categoriesFor: #createSchematicWiring!public! !
!CineShell categoriesFor: #eliminarFuncionPelicula!public! !
!CineShell categoriesFor: #eliminarPelicula!public! !
!CineShell categoriesFor: #fechaActual!public! !
!CineShell categoriesFor: #hasSelectedFuncion!public! !
!CineShell categoriesFor: #hasSelectedPelicula!public! !
!CineShell categoriesFor: #infoFuncion!public! !
!CineShell categoriesFor: #infoPelicula!public! !
!CineShell categoriesFor: #menuClientes!public! !
!CineShell categoriesFor: #menuSalas!public! !
!CineShell categoriesFor: #model:!public! !
!CineShell categoriesFor: #nuevaFuncionPelicula!public! !
!CineShell categoriesFor: #nuevaPelicula!public! !
!CineShell categoriesFor: #nuevaReserva!public! !
!CineShell categoriesFor: #onViewOpened!public! !
!CineShell categoriesFor: #puedeReservarFuncion!public! !
!CineShell categoriesFor: #queryCommand:!public! !

!CineShell class methodsFor!

defaultModel
	^CineModel new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 39 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 109 105 66 111 108 100 0 67 0 115 101 100 32 69 120 116 114 97 32 66 111 108 100 0 0] 328198 ##(Smalltalk.Point) 193 193 0 416 0 234 256 98 52 410 8 ##(Smalltalk.PushButton) 98 20 0 416 98 2 8 1140924416 1 672 0 0 0 7 0 0 0 672 0 8 4294904801 1180998 4 ##(Smalltalk.CommandDescription) 8 #nuevaReserva 8 'Nueva Reserva' 1 1 0 0 32 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 3 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 610 1591 1081 610 171 81 672 898 8 #isEnabled: 98 1 32 672 898 8 #text: 98 1 8 'Nueva Reserva' 672 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 27 3 0 0 28 2 0 0 112 3 0 0 68 2 0 0] 98 0 610 193 193 0 29 8 'btnNuevaReserva' 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 65 1200 0 0 0 7 0 530 0 16 562 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 2 86 65 76 79 82 65 78 84 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 624 0 1200 0 8 4294904793 852486 ##(Smalltalk.NullConverter) 0 0 0 834 202 208 98 2 898 928 98 2 610 105 19 610 401 41 1200 898 1056 98 1 8 'Nombre del cine' 1200 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 52 0 0 0 9 0 0 0 252 0 0 0 29 0 0 0] 98 0 1168 0 27 8 'nombre' 410 1216 98 16 0 416 98 2 8 1140850956 65 1600 0 0 0 7 0 530 0 16 562 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 610 193 193 0 1600 786694 ##(Smalltalk.IndexedColor) 33554437 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 445 979 610 171 31 1600 898 1056 98 1 8 '0' 1600 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 222 0 0 0 233 1 0 0 51 1 0 0 248 1 0 0] 98 0 1168 0 27 8 'cantidadPeliculasTotales' 410 1216 98 16 0 416 98 2 8 1140850956 65 2016 0 0 0 7 0 530 0 16 562 1696 610 193 193 0 2016 196934 1 ##(Smalltalk.RGB) 25231361 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 445 1019 610 171 31 2016 898 1056 98 1 8 '0' 2016 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 222 0 0 0 253 1 0 0 51 1 0 0 12 2 0 0] 98 0 1168 0 27 8 'cantidadPeliculasMostradas' 410 8 ##(Smalltalk.CheckBox) 98 16 0 416 98 2 8 1409297667 1 2416 721990 2 ##(Smalltalk.ValueHolder) 0 32 1114118 ##(Smalltalk.NeverSearchPolicy) 16 0 0 1031 0 0 0 2416 0 8 4294904801 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 21 201 610 81 41 2416 898 1056 98 1 8 '2D' 2416 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 100 0 0 0 50 0 0 0 120 0 0 0] 98 0 1168 0 27 8 '2D' 410 1216 98 16 0 416 98 2 8 1140850944 65 2816 0 0 0 7 0 530 0 16 562 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 255 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1712 0 2816 1744 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 1681 81 610 91 33 2816 898 1056 98 1 8 '0' 2816 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 72 3 0 0 40 0 0 0 117 3 0 0 56 0 0 0] 98 0 1168 0 27 8 'cantidadSalas' 410 8 ##(Smalltalk.ListBox) 98 17 0 416 98 2 8 1412497665 1025 3184 590662 2 ##(Smalltalk.ListModel) 202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 482 8 4278190080 0 7 0 0 0 3184 0 8 4294905345 459270 ##(Smalltalk.Message) 8 #displayString 98 0 3312 32 834 202 208 98 2 898 928 98 2 610 545 241 610 1221 731 3184 898 8 #horizontalExtent: 98 1 1 3184 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 16 1 0 0 120 0 0 0 114 3 0 0 229 1 0 0] 98 0 1168 0 27 8 'funciones' 410 1216 98 16 0 416 98 2 8 1140850944 65 3696 0 0 0 7 0 530 0 16 562 2912 610 193 193 0 3696 1730 33554459 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 1681 51 610 91 35 3696 898 1056 98 1 8 '0' 3696 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 72 3 0 0 25 0 0 0 117 3 0 0 42 0 0 0] 98 0 1168 0 27 8 'cantidadClientes' 410 2432 98 16 0 416 98 2 8 1409363203 1 4080 2498 0 32 2544 16 0 0 7 0 0 0 4080 0 8 4294904801 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 141 201 610 81 41 4080 898 1056 98 1 8 '3D' 4080 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 100 0 0 0 110 0 0 0 120 0 0 0] 98 0 1168 0 27 8 '3D' 410 2432 98 16 0 416 98 2 8 1409363203 1 4416 2498 0 32 2544 16 0 0 7 0 0 0 4416 0 8 4294904801 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 861 201 610 191 41 4416 898 1056 98 1 8 'Miércoles' 4416 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 174 1 0 0 100 0 0 0 13 2 0 0 120 0 0 0] 98 0 1168 0 27 8 'miercoles' 410 2432 98 16 0 416 98 2 8 1409363203 1 4752 2498 0 32 2544 16 0 0 7 0 0 0 4752 0 8 4294904801 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 391 201 610 131 41 4752 898 1056 98 1 8 'IMAX' 4752 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 100 0 0 0 4 1 0 0 120 0 0 0] 98 0 1168 0 27 8 'IMAX' 410 688 98 20 0 416 98 2 8 1140924416 1 5088 0 0 0 7 0 0 0 5088 0 8 4294904801 770 8 #nuevaFuncionPelicula 8 'Nueva Función' 1 1 0 0 32 0 0 0 834 202 208 98 3 898 928 98 2 610 561 1081 610 171 81 5088 898 1008 98 1 32 5088 898 1056 98 1 8 'Nueva Función' 5088 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 24 1 0 0 28 2 0 0 109 1 0 0 68 2 0 0] 98 0 1168 0 29 8 'btnNuevaFuncion' 410 1216 98 16 0 416 98 2 8 1140850946 65 5472 0 0 0 7 0 530 0 16 562 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 610 193 193 0 5472 2130 33488897 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 1241 11 610 511 41 5472 898 1056 98 1 8 'fecha' 5472 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 108 2 0 0 5 0 0 0 107 3 0 0 25 0 0 0] 98 0 1168 0 27 8 'fechaActual' 410 3200 98 17 0 416 98 2 8 1412497665 1025 5872 3266 202 208 3312 0 3344 482 8 4278190080 0 7 0 0 0 5872 0 8 4294905345 3410 3440 98 0 3312 32 834 202 208 98 2 898 928 98 2 610 15 241 610 511 731 5872 898 3600 98 1 1001 5872 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 120 0 0 0 6 1 0 0 229 1 0 0] 98 0 1168 0 27 8 'peliculas' 410 2432 98 16 0 416 98 2 8 1409363203 1 6256 2498 0 32 2544 16 0 0 7 0 0 0 6256 0 8 4294904801 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 1581 201 610 181 41 6256 898 1056 98 1 8 'Domingo' 6256 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 22 3 0 0 100 0 0 0 112 3 0 0 120 0 0 0] 98 0 1168 0 27 8 'domingo' 410 2432 98 16 0 416 98 2 8 1409363203 1 6592 2498 0 32 2544 16 0 0 7 0 0 0 6592 0 8 4294904801 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 261 201 610 81 41 6592 898 1056 98 1 8 '4D' 6592 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 130 0 0 0 100 0 0 0 170 0 0 0 120 0 0 0] 98 0 1168 0 27 8 '4D' 410 2432 98 16 0 416 98 2 8 1409363203 1 6928 2498 0 32 2544 16 0 0 7 0 0 0 6928 0 8 4294904801 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 701 201 610 151 41 6928 898 1056 98 1 8 'Martes' 6928 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 94 1 0 0 100 0 0 0 169 1 0 0 120 0 0 0] 98 0 1168 0 27 8 'martes' 410 1216 98 16 0 416 98 2 8 1140850944 65 7264 0 0 0 7 0 530 0 16 562 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 109 105 66 111 108 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 610 193 193 0 7264 0 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 185 89 610 521 41 7264 898 1056 98 1 8 'direccion' 7264 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 92 0 0 0 44 0 0 0 96 1 0 0 64 0 0 0] 98 0 1168 0 27 8 'direccion' 410 2432 98 16 0 416 98 2 8 1409363203 1 7648 2498 0 32 2544 16 0 0 7 0 0 0 7648 0 8 4294904801 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 1231 201 610 161 41 7648 898 1056 98 1 8 'Viernes' 7648 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 103 2 0 0 100 0 0 0 183 2 0 0 120 0 0 0] 98 0 1168 0 27 8 'viernes' 410 1216 98 16 0 416 98 2 8 1140850944 65 7984 0 0 0 7 0 530 0 16 562 7360 610 193 193 0 7984 0 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 145 49 610 301 41 7984 898 1056 98 1 8 'cuit' 7984 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 72 0 0 0 24 0 0 0 222 0 0 0 44 0 0 0] 98 0 1168 0 27 8 'cuit' 410 2432 98 16 0 416 98 2 8 1409363203 1 8352 2498 0 32 2544 16 0 0 7 0 0 0 8352 0 8 4294904801 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 541 201 610 131 41 8352 898 1056 98 1 8 'Lunes' 8352 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 1 0 0 100 0 0 0 79 1 0 0 120 0 0 0] 98 0 1168 0 27 8 'lunes' 410 2432 98 16 0 416 98 2 8 1409363203 1 8688 2498 0 32 2544 16 0 0 7 0 0 0 8688 0 8 4294904801 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 1411 201 610 161 41 8688 898 1056 98 1 8 'Sábado' 8688 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 2 0 0 100 0 0 0 17 3 0 0 120 0 0 0] 98 0 1168 0 27 8 'sabado' 410 8 ##(Smalltalk.ComboBox) 98 17 0 416 98 2 8 1412498947 1025 9024 3266 202 208 3312 0 3344 482 8 4278190080 0 7 0 0 0 9024 0 8 4294904819 3410 3440 98 0 3312 401 834 202 208 98 1 898 928 98 2 610 1225 139 610 551 55 9024 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 100 2 0 0 69 0 0 0 119 3 0 0 96 0 0 0] 98 0 1168 0 27 8 'idiomas' 410 688 98 20 0 416 98 2 8 1140924416 1 9392 0 0 0 7 0 0 0 9392 0 8 4294904801 770 8 #nuevaPelicula 8 'Nueva Pelicula' 1 1 0 0 32 0 0 0 834 202 208 98 3 898 928 98 2 610 21 1081 610 171 81 9392 898 1008 98 1 32 9392 898 1056 98 1 8 'Nueva Pelicula' 9392 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 28 2 0 0 95 0 0 0 68 2 0 0] 98 0 1168 0 29 8 'btnNuevaPelicula' 410 2432 98 16 0 416 98 2 8 1409363203 1 9776 2498 0 32 2544 16 0 0 7 0 0 0 9776 0 8 4294904801 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 1071 201 610 151 41 9776 898 1056 98 1 8 'Jueves' 9776 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 23 2 0 0 100 0 0 0 98 2 0 0 120 0 0 0] 98 0 1168 0 27 8 'jueves' 410 688 98 20 0 416 98 2 8 1140924416 1 10112 0 0 0 7 0 0 0 10112 0 8 4294904801 770 8 #menuClientes 8 'Clientes' 1 1 0 0 32 0 0 0 834 202 208 98 3 898 928 98 2 610 721 71 610 171 61 10112 898 1008 98 1 32 10112 898 1056 98 1 8 'Clientes' 10112 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 104 1 0 0 35 0 0 0 189 1 0 0 65 0 0 0] 98 0 1168 0 29 8 'btnClientes' 0 0 0 0 0 1 0 0 610 1821 1251 610 1821 1251 1 0 0 834 202 208 98 2 898 928 98 2 610 3839 21 610 1821 1251 416 898 8 #updateMenuBar 3312 416 1106 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 13 11 0 0 123 2 0 0] 98 42 9392 1200 410 1216 98 16 0 416 98 2 8 1140850944 65 10720 0 0 0 7 0 0 0 10720 0 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 11 11 610 91 41 10720 898 1056 98 1 8 'Cine:' 10720 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 50 0 0 0 25 0 0 0] 98 0 1168 0 27 410 1216 98 16 0 416 98 2 8 1140850944 65 11024 0 0 0 7 0 0 0 11024 0 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 11 51 610 131 41 11024 898 1056 98 1 8 'C.U.I.T:' 11024 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 25 0 0 0 70 0 0 0 45 0 0 0] 98 0 1168 0 27 410 1216 98 16 0 416 98 2 8 1140850944 65 11328 0 0 0 7 0 0 0 11328 0 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 11 89 610 161 41 11328 898 1056 98 1 8 'Dirección:' 11328 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 44 0 0 0 85 0 0 0 64 0 0 0] 98 0 1168 0 27 7984 7264 5472 5088 5872 3184 410 1216 98 16 0 416 98 2 8 1140850944 65 11632 0 0 0 7 0 0 0 11632 0 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 25 159 610 191 41 11632 898 1056 98 1 8 'Peliculas:' 11632 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 12 0 0 0 79 0 0 0 107 0 0 0 99 0 0 0] 98 0 1168 0 27 410 1216 98 16 0 416 98 2 8 1140850944 65 11936 0 0 0 7 0 0 0 11936 0 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 549 159 610 201 41 11936 898 1056 98 1 8 'Funciones:' 11936 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 18 1 0 0 79 0 0 0 118 1 0 0 99 0 0 0] 98 0 1168 0 27 410 8 ##(Smalltalk.Splitter) 98 12 0 416 98 2 8 1149239296 262145 12240 0 1730 33554433 0 519 0 0 0 12240 834 202 208 98 2 898 928 98 2 610 1 131 610 1791 9 12240 898 1008 98 1 32 12240 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 65 0 0 0 127 3 0 0 69 0 0 0] 98 0 1168 0 27 10112 672 2416 4080 6592 4752 8352 6928 4416 9776 7648 8688 6256 9024 410 1216 98 16 0 416 98 2 8 1140850944 65 12528 0 0 0 7 0 0 0 12528 0 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 1091 151 610 131 41 12528 898 1056 98 1 8 'Idioma:' 12528 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 33 2 0 0 75 0 0 0 98 2 0 0 95 0 0 0] 98 0 1168 0 27 410 688 98 20 0 416 98 2 8 1140924416 1 12832 0 0 0 7 0 0 0 12832 0 8 4294904801 770 8 #menuSalas 8 'Salas' 1 1 0 0 32 0 0 0 834 202 208 98 3 898 928 98 2 610 901 71 610 171 61 12832 898 1008 98 1 32 12832 898 1056 98 1 8 'Salas' 12832 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 194 1 0 0 35 0 0 0 23 2 0 0 65 0 0 0] 98 0 1168 0 29 410 1216 98 16 0 416 98 2 8 1140850944 65 13200 0 0 0 7 0 0 0 13200 0 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 1371 81 610 311 41 13200 898 1056 98 1 8 'Cantidad de Salas:' 13200 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 173 2 0 0 40 0 0 0 72 3 0 0 60 0 0 0] 98 0 1168 0 27 410 1216 98 16 0 416 98 2 8 1140850944 65 13504 0 0 0 7 0 530 0 16 562 8 #[244 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1712 0 13504 0 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 15 979 610 423 31 13504 898 1056 98 1 8 'Cantidad de Peliculas en Cartelera:' 13504 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 233 1 0 0 218 0 0 0 248 1 0 0] 98 0 1168 0 27 410 1216 98 16 0 416 98 2 8 1140850944 65 13856 0 0 0 7 0 530 0 16 562 8 #[244 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1712 0 13856 0 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 15 1019 610 411 41 13856 898 1056 98 1 8 'Cantidad de Peliculas mostradas:' 13856 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 253 1 0 0 212 0 0 0 17 2 0 0] 98 0 1168 0 27 2816 1600 2016 410 1216 98 16 0 416 98 2 8 1140850944 65 14208 0 0 0 7 0 0 0 14208 0 8 4294904793 1346 0 0 0 834 202 208 98 2 898 928 98 2 610 1325 49 610 351 41 14208 898 1056 98 1 8 'Cantidad de Clientes:' 14208 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 2 0 0 24 0 0 0 69 3 0 0 44 0 0 0] 98 0 1168 0 27 3696 410 688 98 20 0 416 98 2 8 1140924416 1 14512 0 0 0 7 0 0 0 14512 0 8 4294904801 770 8 #eliminarPelicula 8 'Eliminar Pelicula' 1 1 0 0 32 0 0 0 834 202 208 98 3 898 928 98 2 610 201 1081 610 171 81 14512 898 1008 98 1 32 14512 898 1056 98 1 8 'Eliminar Pelicula' 14512 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 100 0 0 0 28 2 0 0 185 0 0 0 68 2 0 0] 98 0 1168 0 29 410 688 98 20 0 416 98 2 8 1140924416 1 14880 0 0 0 7 0 0 0 14880 0 8 4294904801 770 8 #eliminarFuncionPelicula 8 'Eliminar Función' 1 1 0 0 32 0 0 0 834 202 208 98 3 898 928 98 2 610 741 1081 610 171 81 14880 898 1008 98 1 32 14880 898 1056 98 1 8 'Eliminar Función' 14880 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 114 1 0 0 28 2 0 0 199 1 0 0 68 2 0 0] 98 0 1168 0 29 410 688 98 20 0 416 98 2 8 1140924416 1 15248 0 0 0 7 0 0 0 15248 0 8 4294904801 770 8 #infoPelicula 8 '+ Info Pelicula' 1 1 0 0 32 0 0 0 834 202 208 98 3 898 928 98 2 610 381 1081 610 171 81 15248 898 1008 98 1 32 15248 898 1056 98 1 8 '+ Info Pelicula' 15248 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 190 0 0 0 28 2 0 0 19 1 0 0 68 2 0 0] 98 0 1168 0 29 410 688 98 20 0 416 98 2 8 1140924416 1 15616 0 0 0 7 0 0 0 15616 0 8 4294904801 770 8 #infoFuncion 8 '+ Info Función' 1 1 0 0 32 0 0 0 834 202 208 98 3 898 928 98 2 610 921 1081 610 171 81 15616 898 1008 98 1 32 15616 898 1056 98 1 8 '+ Info Función' 15616 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 204 1 0 0 28 2 0 0 33 2 0 0 68 2 0 0] 98 0 1168 0 29 1168 0 27 )! !
!CineShell class categoriesFor: #defaultModel!public! !
!CineShell class categoriesFor: #resource_Default_view!public!resources-views! !

InfoCliente guid: (GUID fromString: '{69859559-8255-4FFA-A0BB-B8D47DEA5F7A}')!
InfoCliente comment: ''!
!InfoCliente categoriesForClass!MVP-Presenters! !
InfoFuncionPelicula guid: (GUID fromString: '{1EFB1103-CA26-450A-AA3E-4528FA5021B2}')!
InfoFuncionPelicula comment: ''!
!InfoFuncionPelicula categoriesForClass!MVP-Presenters! !
!InfoFuncionPelicula methodsFor!

createComponents
	super createComponents.
	idFuncionPelicula := self add: TextPresenter new name: 'idFuncionPelicula'.
	nombreSala := self add: TextPresenter new name: 'nombreSalaAsignada'.
	nombrePelicula := self add: TextPresenter new name: 'nombrePelicula'.
	horario := self add: TextPresenter new name: 'horario'.
	idioma := self add: TextPresenter new name: 'idioma'.
	dia := self add: TextPresenter new name: 'dia'!

model: unaFuncionPelicula
	super model: unaFuncionPelicula.
	idFuncionPelicula model: unaFuncionPelicula verIdFuncionPelicula.
	nombreSala model: unaFuncionPelicula verNombreSalaAsignada.
	nombrePelicula model: unaFuncionPelicula verNombrePelicula.
	horario model: unaFuncionPelicula verHorarioConFormato.
	idioma model: unaFuncionPelicula verIdioma.
	dia model: unaFuncionPelicula verDia!

onViewOpened
	super onViewOpened.
	self view text: 'Detalles de Funcion'! !
!InfoFuncionPelicula categoriesFor: #createComponents!public! !
!InfoFuncionPelicula categoriesFor: #model:!public! !
!InfoFuncionPelicula categoriesFor: #onViewOpened!public! !

!InfoFuncionPelicula class methodsFor!

defaultModel
	^FuncionPelicula
		crearFuncionPeliculaNombreSala: 'N/A'
		nombrePelicula: 'N/A'
		horario: 1000
		idioma: 'N/A'
		formato: 'N/A'
		dia: 'N/A'!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 551 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 416 0 234 256 98 12 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 65 672 0 0 0 7 0 530 0 16 562 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 255 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 610 193 193 0 672 196934 1 ##(Smalltalk.RGB) 33488897 8 4294903505 852486 ##(Smalltalk.NullConverter) 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 2 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 610 361 61 610 721 39 672 962 8 #text: 98 1 8 'Nombre de Pelicula' 672 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 30 0 0 0 28 2 0 0 49 0 0 0] 98 0 610 193 193 0 27 8 'nombrePelicula' 410 688 98 16 0 416 98 2 8 1140850944 65 1216 0 0 0 7 0 530 0 16 562 784 610 193 193 0 1216 818 33488897 8 4294903505 866 0 0 0 898 202 208 98 2 962 992 98 2 610 251 301 610 561 39 1216 962 1072 98 1 8 'Id de Función' 1216 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 125 0 0 0 150 0 0 0 149 1 0 0 169 0 0 0] 98 0 1184 0 27 8 'idFuncionPelicula' 410 688 98 16 0 416 98 2 8 1140850944 65 1600 0 0 0 7 0 530 0 16 562 784 610 193 193 0 1600 818 33488897 8 4294903505 866 0 0 0 898 202 208 98 2 962 992 98 2 610 131 181 610 241 39 1600 962 1072 98 1 8 'Dia asignado' 1600 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 65 0 0 0 90 0 0 0 185 0 0 0 109 0 0 0] 98 0 1184 0 27 8 'dia' 410 688 98 16 0 416 98 2 8 1140850944 65 1984 0 0 0 7 0 530 0 16 562 784 610 193 193 0 1984 818 33488897 8 4294903505 866 0 0 0 898 202 208 98 2 962 992 98 2 610 731 421 610 361 39 1984 962 1072 98 1 8 'Idioma' 1984 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 109 1 0 0 210 0 0 0 33 2 0 0 229 0 0 0] 98 0 1184 0 27 8 'idioma' 410 688 98 16 0 416 98 2 8 1140850944 65 2368 0 0 0 7 0 530 0 16 562 784 610 193 193 0 2368 818 33488897 8 4294903505 866 0 0 0 898 202 208 98 2 962 992 98 2 610 301 421 610 241 39 2368 962 1072 98 1 8 'Sala asignada' 2368 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 210 0 0 0 14 1 0 0 229 0 0 0] 98 0 1184 0 27 8 'nombreSalaAsignada' 410 688 98 16 0 416 98 2 8 1140850944 65 2752 0 0 0 7 0 530 0 16 562 784 610 193 193 0 2752 818 33488897 8 4294903505 866 0 0 0 898 202 208 98 2 962 992 98 2 610 541 181 610 561 39 2752 962 1072 98 1 8 'Horario asignado' 2752 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 1 0 0 90 0 0 0 38 2 0 0 109 0 0 0] 98 0 1184 0 27 8 'horario' 0 0 0 0 0 1 0 0 610 1201 591 610 1201 591 1 0 0 898 202 208 98 2 962 992 98 2 610 3839 21 610 1201 591 416 962 8 #updateMenuBar 98 0 416 1122 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 215 9 0 0 49 1 0 0] 98 12 410 688 98 16 0 416 98 2 8 1140850944 65 3376 0 0 0 7 0 0 0 3376 0 8 4294903505 866 0 0 0 898 202 208 98 2 962 992 98 2 610 61 301 610 191 39 3376 962 1072 98 1 8 'Id. Funcion:' 3376 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 30 0 0 0 150 0 0 0 125 0 0 0 169 0 0 0] 98 0 1184 0 27 410 688 98 16 0 416 98 2 8 1140850944 65 3680 0 0 0 7 0 0 0 3680 0 8 4294903505 866 0 0 0 898 202 208 98 2 962 992 98 2 610 61 61 610 301 39 3680 962 1072 98 1 8 'Titulo de pelicula:' 3680 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 30 0 0 0 30 0 0 0 180 0 0 0 49 0 0 0] 98 0 1184 0 27 410 688 98 16 0 416 98 2 8 1140850944 65 3984 0 0 0 7 0 0 0 3984 0 8 4294903505 866 0 0 0 898 202 208 98 2 962 992 98 2 610 61 181 610 71 39 3984 962 1072 98 1 8 'Día:' 3984 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 30 0 0 0 90 0 0 0 65 0 0 0 109 0 0 0] 98 0 1184 0 27 410 688 98 16 0 416 98 2 8 1140850944 65 4288 0 0 0 7 0 0 0 4288 0 8 4294903505 866 0 0 0 898 202 208 98 2 962 992 98 2 610 401 181 610 141 39 4288 962 1072 98 1 8 'Horario:' 4288 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 200 0 0 0 90 0 0 0 14 1 0 0 109 0 0 0] 98 0 1184 0 27 410 688 98 16 0 416 98 2 8 1140850944 65 4592 0 0 0 7 0 0 0 4592 0 8 4294903505 866 0 0 0 898 202 208 98 2 962 992 98 2 610 61 421 610 241 39 4592 962 1072 98 1 8 'Sala asignada:' 4592 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 30 0 0 0 210 0 0 0 150 0 0 0 229 0 0 0] 98 0 1184 0 27 410 688 98 16 0 416 98 2 8 1140850944 65 4896 0 0 0 7 0 0 0 4896 0 8 4294903505 866 0 0 0 898 202 208 98 2 962 992 98 2 610 601 421 610 131 39 4896 962 1072 98 1 8 'Idioma:' 4896 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 1 0 0 210 0 0 0 109 1 0 0 229 0 0 0] 98 0 1184 0 27 672 1600 2752 1216 2368 1984 1184 0 27 )! !
!InfoFuncionPelicula class categoriesFor: #defaultModel!public! !
!InfoFuncionPelicula class categoriesFor: #resource_Default_view!public!resources-views! !

InfoPelicula guid: (GUID fromString: '{4299BFA4-F60A-4A12-8F95-35D929CFB695}')!
InfoPelicula comment: ''!
!InfoPelicula categoriesForClass!MVP-Presenters! !
!InfoPelicula methodsFor!

actualizarFunciones
	funciones model: (ListModel new list: self model verFunciones asOrderedCollection)!

createComponents
	super createComponents.
	titulo := self add: TextPresenter new name: 'titulo'.
	duracion := self add: NumberPresenter new name: 'duracion'.
	formato := self add: TextPresenter new name: 'formato'.
	funciones := self add: ListPresenter new name: 'funciones'.
	precio := self add: NumberPresenter new name: 'precio'.!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	self
		when: #funcionesChanged
		send: #actualizarFunciones
		to: self!

editarPrecio
	| flag sigue nuevoPrecio |
	flag := true.
	sigue := true.
	[flag] whileTrue: 
			[nuevoPrecio := Prompter prompt: 'Ingrese el precio.'.
			([nuevoPrecio asDecimalOrFloat] on: BoundsError do: [:ex | -1]) > 0
				ifTrue: 
					[model modiPrecio: nuevoPrecio asNumber.
					precio model: '$' , model verPrecio printString.
					flag := false]
				ifFalse: 
					[MessageBox warning: 'El número ingresado es incorrecto'.
					sigue := MessageBox confirm: '¿Desea volver a intentar?'.
					sigue ifFalse: [flag := false]]]!

model: unaPelicula
	super model: unaPelicula.
	titulo model: unaPelicula verTitulo.
	duracion model: unaPelicula verDuracion printString , ' minutos.'.
	formato model: unaPelicula verFormato.
	funciones model: (ListModel new list: unaPelicula verFunciones asOrderedCollection).
	precio model: '$' , unaPelicula verPrecio printString!

onViewOpened
	super onViewOpened.
	self view text: 'Detalles de Pelicula'.
	! !
!InfoPelicula categoriesFor: #actualizarFunciones!public! !
!InfoPelicula categoriesFor: #createComponents!public! !
!InfoPelicula categoriesFor: #createSchematicWiring!public! !
!InfoPelicula categoriesFor: #editarPrecio!public! !
!InfoPelicula categoriesFor: #model:!public! !
!InfoPelicula categoriesFor: #onViewOpened!public! !

!InfoPelicula class methodsFor!

defaultModel
	^Pelicula
		crearPeliculaTitulo: 'N/A'
		duracion: 0
		formato: 'N/A'
		precio: 0.0!

publishedEventsOfInstances
	"Answer a Set of Symbols that describe the published events triggered
	by instances of the receiver."

	^(super publishedEventsOfInstances)
		add: #funcionesChanged;
		yourself!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 551 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 416 0 234 256 98 10 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 65 672 0 0 0 7 0 530 0 16 562 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 255 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 610 193 193 0 672 196934 1 ##(Smalltalk.RGB) 33488897 8 4294903307 852486 ##(Smalltalk.NullConverter) 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 2 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 610 1101 51 610 241 37 672 962 8 #text: 98 1 8 'Static Text' 672 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 2 0 0 25 0 0 0 158 2 0 0 43 0 0 0] 98 0 610 193 193 0 27 8 'formato' 410 688 98 16 0 416 98 2 8 1140850944 65 1216 0 0 0 7 0 530 0 16 562 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 255 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 800 0 1216 818 8454145 8 4294903307 866 0 0 0 898 202 208 98 2 962 992 98 2 610 1031 121 610 301 37 1216 962 1072 98 1 8 'Static Text' 1216 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 3 2 0 0 60 0 0 0 153 2 0 0 78 0 0 0] 98 0 1184 0 27 8 'precio' 410 688 98 16 0 416 98 2 8 1140850944 65 1600 0 0 0 7 0 530 0 16 562 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 255 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 800 0 1600 818 33488897 8 4294903307 866 0 0 0 898 202 208 98 2 962 992 98 2 610 171 51 610 701 37 1600 962 1072 98 1 8 'Static Text' 1600 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 85 0 0 0 25 0 0 0 179 1 0 0 43 0 0 0] 98 0 1184 0 27 8 'titulo' 410 688 98 16 0 416 98 2 8 1140850944 65 1984 0 0 0 7 0 530 0 16 562 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 255 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 800 0 1984 818 33488897 8 4294903307 866 0 0 0 898 202 208 98 2 962 992 98 2 610 231 121 610 361 39 1984 962 1072 98 1 8 'Static Text' 1984 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 115 0 0 0 60 0 0 0 39 1 0 0 79 0 0 0] 98 0 1184 0 27 8 'duracion' 410 8 ##(Smalltalk.ListView) 98 30 0 416 98 2 8 1409355853 1025 2368 590662 2 ##(Smalltalk.ListModel) 202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 482 8 4278190080 0 7 0 0 0 2368 0 8 4294903399 459270 ##(Smalltalk.Message) 8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 202 208 98 5 920646 5 ##(Smalltalk.ListViewColumn) 8 'Id. Funcion' 541 8 #left 2594 8 #verIdFuncionPelicula 2640 8 ##(Smalltalk.SortedCollection) 0 0 2368 0 1 0 0 2738 8 'Dia' 189 2784 2594 8 #verDia 98 0 2594 8 #mayorDia: 2912 0 0 2368 0 1 0 0 2738 8 'Horario' 199 2784 2594 8 #verHorarioConFormato 2912 2594 8 #mayorHorario: 2912 0 0 2368 0 1 0 0 2738 8 'Sala' 101 2784 2594 8 #verNombreSalaAsignada 2912 2594 8 #mayorSala: 2912 0 0 2368 0 1 0 0 2738 8 'Idioma' 531 2784 2594 8 #verIdioma 2912 2594 8 #mayorIdioma: 2912 0 0 2368 0 1 0 0 8 #report 2496 0 131169 0 98 4 0 0 610 1 1 0 898 202 208 98 2 962 992 98 2 610 51 259 610 1451 601 2368 962 1072 98 1 8 'Id. Funcion' 2368 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 0 0 0 129 0 0 0 238 2 0 0 173 1 0 0] 98 0 1184 0 27 8 'funciones' 0 0 0 0 0 1 0 0 610 1571 991 610 1571 991 1 0 0 898 202 208 98 2 962 992 98 2 610 3839 21 610 1571 991 416 962 8 #updateMenuBar 2496 416 1122 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 144 10 0 0 249 1 0 0] 98 11 410 688 98 16 0 416 98 2 8 1140850944 65 3744 0 0 0 7 0 0 0 3744 0 8 4294903307 866 0 0 0 898 202 208 98 2 962 992 98 2 610 51 51 610 107 37 3744 962 1072 98 1 8 'Titulo:' 3744 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 0 0 0 25 0 0 0 78 0 0 0 43 0 0 0] 98 0 1184 0 27 410 688 98 16 0 416 98 2 8 1140850944 65 4048 0 0 0 7 0 0 0 4048 0 8 4294903307 866 0 0 0 898 202 208 98 2 962 992 98 2 610 899 51 610 147 37 4048 962 1072 98 1 8 'Formato:' 4048 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 1 0 0 25 0 0 0 10 2 0 0 43 0 0 0] 98 0 1184 0 27 410 688 98 16 0 416 98 2 8 1140850944 65 4352 0 0 0 7 0 0 0 4352 0 8 4294903307 866 0 0 0 898 202 208 98 2 962 992 98 2 610 51 121 610 155 37 4352 962 1072 98 1 8 'Duración:' 4352 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 0 0 0 60 0 0 0 102 0 0 0 78 0 0 0] 98 0 1184 0 27 410 688 98 16 0 416 98 2 8 1140850944 65 4656 0 0 0 7 0 0 0 4656 0 8 4294903307 866 0 0 0 898 202 208 98 2 962 992 98 2 610 899 121 610 113 37 4656 962 1072 98 1 8 'Precio:' 4656 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 193 1 0 0 60 0 0 0 249 1 0 0 78 0 0 0] 98 0 1184 0 27 410 688 98 16 0 416 98 2 8 1140850944 65 4960 0 0 0 7 0 0 0 4960 0 8 4294903307 866 0 0 0 898 202 208 98 2 962 992 98 2 610 51 209 610 171 37 4960 962 1072 98 1 8 'Funciones:' 4960 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 0 0 0 104 0 0 0 110 0 0 0 122 0 0 0] 98 0 1184 0 27 1600 1984 672 1216 410 8 ##(Smalltalk.PushButton) 98 20 0 416 98 2 8 1140924416 1 5264 0 0 0 7 0 0 0 5264 0 8 4294903309 1180998 4 ##(Smalltalk.CommandDescription) 8 #editarPrecio 8 'Editar' 1 1 0 0 32 0 0 0 898 202 208 98 3 962 992 98 2 610 1361 115 610 141 51 5264 962 8 #isEnabled: 98 1 32 5264 962 1072 98 1 8 'Editar' 5264 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 168 2 0 0 57 0 0 0 238 2 0 0 82 0 0 0] 98 0 1184 0 29 2368 1184 0 27 )! !
!InfoPelicula class categoriesFor: #defaultModel!public! !
!InfoPelicula class categoriesFor: #publishedEventsOfInstances!public! !
!InfoPelicula class categoriesFor: #resource_Default_view!public!resources-views! !

InfoReserva guid: (GUID fromString: '{784EA81F-E06E-4CE9-8E4A-7610E80ACFF1}')!
InfoReserva comment: ''!
!InfoReserva categoriesForClass!MVP-Presenters! !
InfoSala guid: (GUID fromString: '{4D1F182D-1563-4F2A-9C1A-2B9EB4C9699C}')!
InfoSala comment: ''!
!InfoSala categoriesForClass!MVP-Presenters! !
!InfoSala methodsFor!

actualizarAsientosFiltrados
	"Actualizar el texto del boton y el titulo de la lista de asientos a mostrar"

	| asientosAMostrar |
	btnToggleAsientos value
		ifTrue: 
			[btnToggleAsientos view text: 'Mostrar Reservados'.
			textoAsientos value: 'Asientos Disponibles:']
		ifFalse: 
			[btnToggleAsientos view text: 'Mostrar Disponibles'.
			textoAsientos value: 'Asientos Reservados:'].
	"Mostrar los asientos correspondientes"
	self hasSelectedHorario
		ifTrue: 
			[btnToggleAsientos value
				ifTrue: [asientosAMostrar := self model verAsientosDisp: horarios selection verIdFuncionPelicula]
				ifFalse: [asientosAMostrar := self model verAsientosRes: horarios selection verIdFuncionPelicula]].
	asientosFiltrados model: asientosAMostrar!

createComponents
	super createComponents.
	nombreSala := self add: TextPresenter new name: 'nombreSala'.
	asientos := self add: ListPresenter new name: 'asientos'.
	horarios := self add: ListPresenter new name: 'horarios'.
	tipo := self add: TextPresenter new name: 'tipo'.
	asientosFiltrados := self add: ListPresenter new name: 'asientosFiltrados'.
	btnToggleAsientos := self add: BooleanPresenter new name: 'btnToggleAsientos'.
	textoAsientos := self add: TextPresenter new name: 'textoAsientos'!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	btnToggleAsientos
		when: #valueChanged
		send: #actualizarAsientosFiltrados
		to: self.
	horarios
		when: #selectionChanged
		send: #actualizarAsientosFiltrados
		to: self!

hasSelectedHorario
	^horarios hasSelection!

model: unaSala
	super model: unaSala.
	nombreSala model: unaSala verNombreSala.
	asientos model: unaSala verAsientos.
	horarios model: unaSala verHorarios.
	tipo model: unaSala verTipo.
	btnToggleAsientos model: true asValue!

nuevaSala
	| nuevaSala |
	(nuevaSala := CineNuevaSala showModal) notNil
		ifTrue: 
			[(nuevaSala nombre ~= '' and: 
					[nuevaSala tipo ~= '' and: 
							[nuevaSala cantidadFilas isNumber and: 
									[nuevaSala cantidadFilas > 0
										and: [nuevaSala cantidadColumnas isNumber and: [nuevaSala cantidadColumnas > 0]]]]])
				ifTrue: [self model nuevaSala: nuevaSala]
				ifFalse: [MessageBox warning: 'Los datos ingresados son incorrectos']]!

onViewOpened
	super onViewOpened.
	self view text: 'Detalles de Sala'.
	self actualizarAsientosFiltrados! !
!InfoSala categoriesFor: #actualizarAsientosFiltrados!public! !
!InfoSala categoriesFor: #createComponents!public! !
!InfoSala categoriesFor: #createSchematicWiring!public! !
!InfoSala categoriesFor: #hasSelectedHorario!public! !
!InfoSala categoriesFor: #model:!public! !
!InfoSala categoriesFor: #nuevaSala!public! !
!InfoSala categoriesFor: #onViewOpened!public! !

!InfoSala class methodsFor!

defaultModel
	^Sala crearSalaNombre: 'N/A' Tipo: 'NA'!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 551 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 416 0 234 256 98 14 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 65 672 0 0 0 7 0 0 0 672 196934 1 ##(Smalltalk.RGB) 33488897 8 4294902669 852486 ##(Smalltalk.NullConverter) 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 2 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 610 311 41 610 301 39 672 898 8 #text: 98 1 8 'NOMBRE' 672 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 0 0 0 20 0 0 0 49 1 0 0 39 0 0 0] 98 0 610 193 193 0 27 8 'nombreSala' 410 8 ##(Smalltalk.CheckBox) 98 16 0 416 98 2 8 1409367811 1 1152 721990 2 ##(Smalltalk.ValueHolder) 0 32 1114118 ##(Smalltalk.NeverSearchPolicy) 16 482 8 4278190080 0 7 0 0 0 1152 0 8 4294902213 802 0 0 0 834 202 208 98 2 898 928 98 2 610 1151 441 610 361 61 1152 898 1008 98 1 8 'Mostrar Reservados' 1152 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 63 2 0 0 220 0 0 0 243 2 0 0 250 0 0 0] 98 0 1120 0 27 8 'btnToggleAsientos' 410 688 98 16 0 416 98 2 8 1140850944 65 1584 0 0 0 7 0 0 0 1584 754 33488897 8 4294902669 802 0 0 0 834 202 208 98 2 898 928 98 2 610 1437 41 610 121 39 1584 898 1008 98 1 8 'TIPO' 1584 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 206 2 0 0 20 0 0 0 10 3 0 0 39 0 0 0] 98 0 1120 0 27 8 'tipo' 410 688 98 16 0 416 98 2 8 1140850944 65 1920 0 0 0 7 0 0 0 1920 0 8 4294902669 802 0 0 0 834 202 208 98 2 898 928 98 2 610 1121 501 610 361 41 1920 898 1008 98 1 8 'Asientos Disponibles:' 1920 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 48 2 0 0 250 0 0 0 228 2 0 0 14 1 0 0] 98 0 1120 0 27 8 'textoAsientos' 410 8 ##(Smalltalk.ListBox) 98 17 0 416 98 2 8 1412497665 1025 2240 590662 2 ##(Smalltalk.ListModel) 202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 482 8 4278190080 0 7 0 530 0 16 562 8 #[241 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 610 193 193 0 2240 0 8 4294902207 459270 ##(Smalltalk.Message) 8 #displayString 98 0 2368 32 834 202 208 98 2 898 928 98 2 610 1121 551 610 421 211 2240 898 8 #horizontalExtent: 98 1 1 2240 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 48 2 0 0 19 1 0 0 2 3 0 0 124 1 0 0] 98 0 1120 0 27 8 'asientosFiltrados' 410 2256 98 17 0 416 98 2 8 1412501761 1025 2816 2322 202 208 2368 0 2400 482 2432 0 7 0 530 0 16 562 8 #[241 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2496 0 2816 0 8 4294902207 2530 2560 98 0 2368 32 834 202 208 98 2 898 928 98 2 610 31 179 610 1061 581 2816 898 2720 98 1 1201 2816 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 89 0 0 0 33 2 0 0 123 1 0 0] 98 0 1120 0 27 8 'horarios' 410 2256 98 17 0 416 98 2 8 1412497665 1025 3232 2322 202 208 2368 0 2400 482 2432 0 7 0 530 0 16 562 8 #[241 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2496 0 3232 0 8 4294902207 2530 2560 98 0 2368 32 834 202 208 98 2 898 928 98 2 610 1121 179 610 421 241 3232 898 2720 98 1 1 3232 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 48 2 0 0 89 0 0 0 2 3 0 0 209 0 0 0] 98 0 1120 0 27 8 'asientos' 0 0 0 0 0 1 0 0 610 1601 871 610 1601 871 1 0 0 834 202 208 98 2 898 928 98 2 610 3839 21 610 1601 871 416 898 8 #updateMenuBar 2368 416 1058 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 159 10 0 0 189 1 0 0] 98 11 410 688 98 16 0 416 98 2 8 1140850944 65 3872 0 0 0 7 0 0 0 3872 0 8 4294902669 802 0 0 0 834 202 208 98 2 898 928 98 2 610 31 41 610 291 39 3872 898 1008 98 1 8 'Nombre de Sala:' 3872 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 20 0 0 0 160 0 0 0 39 0 0 0] 98 0 1120 0 27 410 688 98 16 0 416 98 2 8 1140850944 65 4176 0 0 0 7 0 0 0 4176 0 8 4294902669 802 0 0 0 834 202 208 98 2 898 928 98 2 610 1209 41 610 215 39 4176 898 1008 98 1 8 'Tipo de Sala:' 4176 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 92 2 0 0 20 0 0 0 199 2 0 0 39 0 0 0] 98 0 1120 0 27 410 688 98 16 0 416 98 2 8 1140850944 65 4480 0 0 0 7 0 0 0 4480 0 8 4294902669 802 0 0 0 834 202 208 98 2 898 928 98 2 610 31 131 610 149 39 4480 898 1008 98 1 8 'Horarios:' 4480 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 65 0 0 0 89 0 0 0 84 0 0 0] 98 0 1120 0 27 410 688 98 16 0 416 98 2 8 1140850944 65 4784 0 0 0 7 0 0 0 4784 0 8 4294902669 802 0 0 0 834 202 208 98 2 898 928 98 2 610 1121 129 610 291 41 4784 898 1008 98 1 8 'Asientos Totales:' 4784 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 48 2 0 0 64 0 0 0 193 2 0 0 84 0 0 0] 98 0 1120 0 27 2816 3232 672 1584 1920 2240 1152 1120 0 27 )! !
!InfoSala class categoriesFor: #defaultModel!public! !
!InfoSala class categoriesFor: #resource_Default_view!public!resources-views! !

MenuSalas guid: (GUID fromString: '{92610D43-2C61-4591-830F-DE5CC69508E0}')!
MenuSalas comment: ''!
!MenuSalas categoriesForClass!MVP-Presenters! !
!MenuSalas methodsFor!

actualizarSalas
	| salasAMostrar |
	salasAMostrar := model
				verSalasFiltradasBool2D: s2D value
				bool3D: s3D value
				bool4D: s4D value
				boolIMAX: sIMAX value.
	salas model: salasAMostrar!

agregarSala
	| nuevaSala |
	(nuevaSala := CineNuevaSala showModal) notNil
		ifTrue: 
			[(nuevaSala nombre ~= '' and: 
					[nuevaSala tipo ~= '' and: 
							[nuevaSala cantidadFilas isNumber and: 
									[(nuevaSala cantidadFilas between: 1 and: 30)
										and: [nuevaSala cantidadColumnas isNumber and: [nuevaSala cantidadColumnas between: 1 and: 30]]]]])
				ifTrue: 
					[(self model nuevaSala: nuevaSala)
						ifTrue: [MessageBox warning: 'Sala agregada exitosamente']
						ifFalse: [MessageBox warning: 'La Sala ya existe.']]
				ifFalse: [MessageBox warning: 'Los datos ingresados son incorrectos']]!

createComponents
	super createComponents.
	salas := self add: ListPresenter new name: 'salas'.
	cantSalas := self add: NumberPresenter new name: 'cantSalas'.
	s2D := self add: BooleanPresenter new name: 's2D'.
	s3D := self add: BooleanPresenter new name: 's3D'.
	s4D := self add: BooleanPresenter new name: 's4D'.
	sIMAX := self add: BooleanPresenter new name: 'sIMAX'.
	s2D value: true.
	s3D value: true.
	s4D value: true.
	sIMAX value: true!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	cantSalas
		when: #valueChanged
		send: #actualizarSalas
		to: self.
	salas
		when: #actionPerformed
		send: #infoSala
		to: self.
	"Actualizar Salas"
	s2D
		when: #valueChanged
		send: #actualizarSalas
		to: self.
	s3D
		when: #valueChanged
		send: #actualizarSalas
		to: self.
	s4D
		when: #valueChanged
		send: #actualizarSalas
		to: self.
	sIMAX
		when: #valueChanged
		send: #actualizarSalas
		to: self!

hasSelectedSala
	^salas hasSelection!

infoSala
	infoSalaAbierta := infoSalaAbierta reject: [:ventana | ventana view isKindOf: DeafObject].
	infoSalaAbierta add: (InfoSala showOn: salas selection)!

model: unCineModel
	super model: unCineModel.
	salas model: unCineModel verSalas.
	cantSalas model: (unCineModel aspectValue: #cantidadSalas).
	self actualizarSalas.
	cantSalas model aspectTriggers: #salasChanged!

onViewOpened
	super onViewOpened.
	self view text: 'Menu Salas'.
	infoSalaAbierta isNil ifTrue: [infoSalaAbierta := OrderedCollection new].
	s2D value: true.
	s3D value: true.
	s4D value: true.
	sIMAX value: true!

queryCommand: aCommandQuery
	"Enters details about a potential command for the receiver into aCommandQuery"

	super queryCommand: aCommandQuery.
	(#(#infoSala) includes: aCommandQuery command)
		ifTrue: [aCommandQuery isEnabled: self hasSelectedSala]! !
!MenuSalas categoriesFor: #actualizarSalas!public! !
!MenuSalas categoriesFor: #agregarSala!public! !
!MenuSalas categoriesFor: #createComponents!public! !
!MenuSalas categoriesFor: #createSchematicWiring!public! !
!MenuSalas categoriesFor: #hasSelectedSala!public! !
!MenuSalas categoriesFor: #infoSala!public! !
!MenuSalas categoriesFor: #model:!public! !
!MenuSalas categoriesFor: #onViewOpened!public! !
!MenuSalas categoriesFor: #queryCommand:!public! !

!MenuSalas class methodsFor!

defaultModel
	^CineModel new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 551 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 416 0 234 256 98 10 410 8 ##(Smalltalk.CheckBox) 98 16 0 416 98 2 8 1409363203 1 672 721990 2 ##(Smalltalk.ValueHolder) 0 0 1114118 ##(Smalltalk.NeverSearchPolicy) 32 0 0 7 0 0 0 672 0 8 4294902213 852486 ##(Smalltalk.NullConverter) 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 2 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 610 261 111 610 91 41 672 930 8 #text: 98 1 8 '2D' 672 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 130 0 0 0 55 0 0 0 175 0 0 0 75 0 0 0] 98 0 610 193 193 0 27 8 's2D' 410 8 ##(Smalltalk.ListBox) 98 17 0 416 98 2 8 1412497665 1025 1184 590662 2 ##(Smalltalk.ListModel) 202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 482 8 4278190080 0 7 0 0 0 1184 0 8 4294902207 459270 ##(Smalltalk.Message) 8 #displayString 98 0 1312 32 866 202 208 98 2 930 960 98 2 610 35 159 610 691 531 1184 930 8 #horizontalExtent: 98 1 1 1184 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 17 0 0 0 79 0 0 0 106 1 0 0 88 1 0 0] 98 0 1152 0 27 8 'salas' 410 688 98 16 0 416 98 2 8 1409363203 1 1696 754 0 0 800 32 0 0 7 0 0 0 1696 0 8 4294902213 834 0 0 0 866 202 208 98 2 930 960 98 2 610 371 111 610 91 41 1696 930 1040 98 1 8 '3D' 1696 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 55 0 0 0 230 0 0 0 75 0 0 0] 98 0 1152 0 27 8 's3D' 410 688 98 16 0 416 98 2 8 1409363203 1 2032 754 0 0 800 32 0 0 7 0 0 0 2032 0 8 4294902213 834 0 0 0 866 202 208 98 2 930 960 98 2 610 591 111 610 131 41 2032 930 1040 98 1 8 'IMAX' 2032 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 39 1 0 0 55 0 0 0 104 1 0 0 75 0 0 0] 98 0 1152 0 27 8 'sIMAX' 410 688 98 16 0 416 98 2 8 1409363203 1 2368 754 0 0 800 32 0 0 7 0 0 0 2368 0 8 4294902213 834 0 0 0 866 202 208 98 2 930 960 98 2 610 481 111 610 91 41 2368 930 1040 98 1 8 '4D' 2368 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 0 0 0 55 0 0 0 29 1 0 0 75 0 0 0] 98 0 1152 0 27 8 's4D' 0 0 0 0 0 1 0 0 610 791 801 610 791 801 1 0 0 866 202 208 98 2 930 960 98 2 610 3839 21 610 791 801 416 930 8 #updateMenuBar 1312 416 1090 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 10 9 0 0 154 1 0 0] 98 8 1184 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 65 2928 0 0 0 7 0 0 0 2928 0 8 4294902669 834 0 0 0 866 202 208 98 2 930 960 98 2 610 31 111 610 101 39 2928 930 1040 98 1 8 'Salas:' 2928 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 55 0 0 0 65 0 0 0 74 0 0 0] 98 0 1152 0 27 672 1696 2368 2032 410 8 ##(Smalltalk.PushButton) 98 20 0 416 98 2 8 1140924416 1 3248 0 0 0 7 0 0 0 3248 0 8 4294902213 1180998 4 ##(Smalltalk.CommandDescription) 8 #agregarSala 8 'Agregar Sala' 1 1 0 0 32 0 0 0 866 202 208 98 3 930 960 98 2 610 541 11 610 181 81 3248 930 8 #isEnabled: 98 1 32 3248 930 1040 98 1 8 'Agregar Sala' 3248 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 1 0 0 5 0 0 0 104 1 0 0 45 0 0 0] 98 0 1152 0 29 410 3264 98 20 0 416 98 2 8 1140924416 1 3664 0 0 0 7 0 0 0 3664 0 8 4294902213 3346 8 #infoSala 8 'Detalles de Sala' 1 1 0 0 32 0 0 0 866 202 208 98 3 930 960 98 2 610 31 11 610 181 81 3664 930 3536 98 1 32 3664 930 1040 98 1 8 'Detalles de Sala' 3664 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 5 0 0 0 105 0 0 0 45 0 0 0] 98 0 1152 0 29 1152 0 27 )! !
!MenuSalas class categoriesFor: #defaultModel!public! !
!MenuSalas class categoriesFor: #resource_Default_view!public!resources-views! !

CineNuevaFuncionPelicula guid: (GUID fromString: '{9C14C0DD-35AF-4A57-9175-2F37D3A063D2}')!
CineNuevaFuncionPelicula comment: ''!
!CineNuevaFuncionPelicula categoriesForClass!MVP-Presenters! !
!CineNuevaFuncionPelicula methodsFor!

createComponents
	super createComponents.
	salas := self add: ChoicePresenter new name: 'sala'.
	dias := self add: ChoicePresenter new name: 'dia'.
	nombrePelicula := self add: TextPresenter new name: 'nombrePelicula'.
	horas := self add: NumberPresenter new name: 'horas'.
	minutos := self add: NumberPresenter new name: 'minutos'.
	idiomas := self add: ChoicePresenter new name: 'idioma'.
	nombreSala := self add: TextPresenter new name: 'nombreSala'.
	idiomaElegido := self add: TextPresenter new name: 'idiomaElegido'.
	diaElegido := self add: TextPresenter new name: 'diaElegido'.
	formato := self add: TextPresenter new name: 'formato'!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	salas
		when: #actionPerformed
		send: #editSala
		to: self.
	dias
		when: #actionPerformed
		send: #editDia
		to: self.
	idiomas
		when: #actionPerformed
		send: #editIdioma
		to: self!

editDia
	| dia |
	dia := self selectedDiaOrNil.
	dia notNil ifTrue: [self model dia: dia]!

editIdioma
	| idioma |
	idioma := self selectedIdiomaOrNil.
	idioma notNil ifTrue: [self model idioma: idioma]!

editSala
	| sala |
	sala := self selectedSalaOrNil.
	sala notNil ifTrue: [self model nombreSala: sala]!

hasSelectedDia
	^dias hasSelection!

hasSelectedIdioma
	^idiomas hasSelection!

hasSelectedSala
	^salas hasSelection!

model: aFuncionPeliculaModel
	| aspectBuffer |
	super model: aFuncionPeliculaModel.
	aspectBuffer := self model.
	salas model: aspectBuffer salas.
	nombrePelicula model: (aspectBuffer aspectValue: #nombrePelicula).
	dias model: aspectBuffer dias.
	horas model: (aspectBuffer aspectValue: #horas).
	minutos model: (aspectBuffer aspectValue: #minutos).
	idiomas model: aspectBuffer idiomas.
	nombreSala model: (aspectBuffer aspectValue: #nombreSala).
	diaElegido model: (aspectBuffer aspectValue: #dia).
	idiomaElegido model: (aspectBuffer aspectValue: #idioma).
	formato model: (aspectBuffer aspectValue: #formato)!

onViewOpened
	super onViewOpened.
	self view text: 'Agregar Nueva Funcion'.
	salas
		choices: model salas;
		value: (model salas at: 1).
	dias
		choices: #('Lunes' 'Martes' 'Miércoles' 'Jueves' 'Viernes' 'Sábado' 'Domingo');
		value: 'Lunes'.
	idiomas
		choices: #('Español' 'Subtitulado (Eng)' 'Subtitulado (Port)' 'Subtitulado (Jap)' 'Subtitulado (Kor)' 'Subtitulado (Fr)');
		value: 'Español'.
	self editSala.
	self editDia.
	self editIdioma!

selectedDiaOrNil
	^dias getValue!

selectedDiaOrNil: aDiaOrNil
	^dias selectionOrNil: aDiaOrNil!

selectedIdiomaOrNil
	^idiomas getValue!

selectedIdiomaOrNil: aIdiomaOrNil
	^idiomas selectionOrNil: aIdiomaOrNil!

selectedSalaOrNil
	^salas getValue!

selectedSalaOrNil: aSalaOrNil
	^salas selectionOrNil: aSalaOrNil! !
!CineNuevaFuncionPelicula categoriesFor: #createComponents!public! !
!CineNuevaFuncionPelicula categoriesFor: #createSchematicWiring!public! !
!CineNuevaFuncionPelicula categoriesFor: #editDia!public! !
!CineNuevaFuncionPelicula categoriesFor: #editIdioma!public! !
!CineNuevaFuncionPelicula categoriesFor: #editSala!public! !
!CineNuevaFuncionPelicula categoriesFor: #hasSelectedDia!public! !
!CineNuevaFuncionPelicula categoriesFor: #hasSelectedIdioma!public! !
!CineNuevaFuncionPelicula categoriesFor: #hasSelectedSala!public! !
!CineNuevaFuncionPelicula categoriesFor: #model:!public! !
!CineNuevaFuncionPelicula categoriesFor: #onViewOpened!public! !
!CineNuevaFuncionPelicula categoriesFor: #selectedDiaOrNil!public! !
!CineNuevaFuncionPelicula categoriesFor: #selectedDiaOrNil:!public! !
!CineNuevaFuncionPelicula categoriesFor: #selectedIdiomaOrNil!public! !
!CineNuevaFuncionPelicula categoriesFor: #selectedIdiomaOrNil:!public! !
!CineNuevaFuncionPelicula categoriesFor: #selectedSalaOrNil!public! !
!CineNuevaFuncionPelicula categoriesFor: #selectedSalaOrNil:!public! !

!CineNuevaFuncionPelicula class methodsFor!

defaultModel
	^FuncionPeliculaModel new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.DialogView) 98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 167 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 416 788230 ##(Smalltalk.BorderLayout) 1 1 0 410 8 ##(Smalltalk.ReferenceView) 98 14 0 416 98 2 8 1140850688 131073 672 0 0 0 7 0 0 0 672 1180166 ##(Smalltalk.ResourceIdentifier) 8 ##(Smalltalk.Presenter) 8 #resource_OK_Cancel_button_block 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 1 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 610 21 303 610 889 71 672 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 151 0 0 0 198 1 0 0 186 0 0 0] 98 0 610 193 193 0 27 0 0 0 234 256 98 14 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 65 1088 0 0 0 7 0 530 0 16 562 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 255 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 624 0 1088 196934 1 ##(Smalltalk.RGB) 33488897 8 4294903637 852486 ##(Smalltalk.NullConverter) 0 0 0 818 202 208 98 2 882 912 98 2 610 165 31 610 401 41 1088 882 8 #text: 98 1 8 'Static Text' 1088 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 82 0 0 0 15 0 0 0 26 1 0 0 35 0 0 0] 98 0 1040 0 27 8 'nombrePelicula' 410 8 ##(Smalltalk.TextEdit) 98 16 0 416 98 2 8 1140924416 1025 1536 0 482 8 4278190080 0 7 0 0 0 1536 0 8 4294903711 852742 ##(Smalltalk.IntegerToText) 0 8 '' 0 1 818 202 208 98 3 882 912 98 2 610 641 151 610 61 55 1536 882 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval) 3 1 3 1536 882 8 #isTextModified: 98 1 32 1536 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 64 1 0 0 75 0 0 0 94 1 0 0 102 0 0 0] 98 0 1040 0 27 8 'minutos' 410 1104 98 16 0 416 98 2 8 1140850944 65 2016 0 0 0 7 0 530 0 16 562 1200 610 193 193 0 2016 1218 33488897 8 4294903637 1266 0 0 0 818 202 208 98 2 882 912 98 2 610 725 29 610 181 41 2016 882 1424 98 1 8 'Static Text' 2016 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 106 1 0 0 14 0 0 0 196 1 0 0 34 0 0 0] 98 0 1040 0 27 8 'formato' 410 8 ##(Smalltalk.ComboBox) 98 17 0 416 98 2 8 1412498947 1025 2400 590662 2 ##(Smalltalk.ListModel) 202 208 1024 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 482 8 4278190080 0 7 0 0 0 2400 0 8 4294903707 459270 ##(Smalltalk.Message) 8 #displayString 98 0 1024 401 818 202 208 98 1 882 912 98 2 610 521 241 610 251 55 2400 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 1 0 0 120 0 0 0 129 1 0 0 147 0 0 0] 98 0 1040 0 27 8 'sala' 410 2416 98 17 0 416 98 2 8 1412498947 1025 2848 2482 202 208 1024 0 2544 482 2576 0 7 0 0 0 2848 0 8 4294903707 2610 2640 98 0 1024 401 818 202 208 98 1 882 912 98 2 610 151 241 610 251 55 2848 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 120 0 0 0 200 0 0 0 147 0 0 0] 98 0 1040 0 27 8 'idioma' 410 1552 98 16 0 416 98 2 8 1140924416 1025 3184 0 482 1632 0 7 0 0 0 3184 0 8 4294903711 1666 0 8 '' 0 1 818 202 208 98 3 882 912 98 2 610 561 151 610 61 55 3184 882 1840 98 1 1874 3 1 3 3184 882 1920 98 1 32 3184 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 24 1 0 0 75 0 0 0 54 1 0 0 102 0 0 0] 98 0 1040 0 27 8 'horas' 410 2416 98 17 0 416 98 2 8 1412498947 1025 3568 2482 202 208 1024 0 2544 482 2576 0 7 0 0 0 3568 0 8 4294903707 2610 2640 98 0 1024 401 818 202 208 98 1 882 912 98 2 610 111 151 610 291 55 3568 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 55 0 0 0 75 0 0 0 200 0 0 0 102 0 0 0] 98 0 1040 0 27 8 'dia' 590342 ##(Smalltalk.Rectangle) 610 21 21 610 21 21 0 0 0 0 29173 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore) 0 0 1 0 8 2009535680 818 202 208 98 2 882 912 98 2 610 3839 21 610 961 471 416 882 8 #updateMenuBar 1024 416 978 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 95 9 0 0 245 0 0 0] 98 15 672 410 1104 98 16 0 416 98 2 8 1140850944 65 4208 0 0 0 7 0 0 0 4208 0 8 4294903637 1266 0 0 0 818 202 208 98 2 882 912 98 2 610 441 251 610 71 55 4208 882 1424 98 1 8 'Sala:' 4208 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 220 0 0 0 125 0 0 0 255 0 0 0 152 0 0 0] 98 0 1040 0 27 410 1104 98 16 0 416 98 2 8 1140850944 65 4512 0 0 0 7 0 0 0 4512 0 8 4294903637 1266 0 0 0 818 202 208 98 2 882 912 98 2 610 31 31 610 121 55 4512 882 1424 98 1 8 'Pelicula:' 4512 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 15 0 0 0 75 0 0 0 42 0 0 0] 98 0 1040 0 27 410 1104 98 16 0 416 98 2 8 1140850944 65 4816 0 0 0 7 0 0 0 4816 0 8 4294903637 1266 0 0 0 818 202 208 98 2 882 912 98 2 610 31 161 610 61 55 4816 882 1424 98 1 8 'Dia:' 4816 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 80 0 0 0 45 0 0 0 107 0 0 0] 98 0 1040 0 27 410 1104 98 16 0 416 98 2 8 1140850944 65 5120 0 0 0 7 0 0 0 5120 0 8 4294903637 1266 0 0 0 818 202 208 98 2 882 912 98 2 610 441 161 610 121 55 5120 882 1424 98 1 8 'Horario:' 5120 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 220 0 0 0 80 0 0 0 24 1 0 0 107 0 0 0] 98 0 1040 0 27 410 1104 98 16 0 416 98 2 8 1140850944 65 5424 0 0 0 7 0 0 0 5424 0 8 4294903637 1266 0 0 0 818 202 208 98 2 882 912 98 2 610 621 161 610 11 55 5424 882 1424 98 1 8 ':' 5424 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 54 1 0 0 80 0 0 0 59 1 0 0 107 0 0 0] 98 0 1040 0 27 410 1104 98 16 0 416 98 2 8 1140850944 65 5728 0 0 0 7 0 0 0 5728 0 8 4294903637 1266 0 0 0 818 202 208 98 2 882 912 98 2 610 31 251 610 121 55 5728 882 1424 98 1 8 'Idioma:' 5728 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 125 0 0 0 75 0 0 0 152 0 0 0] 98 0 1040 0 27 2400 3568 3184 1536 1088 2848 410 1104 98 16 0 416 98 2 8 1140850944 65 6032 0 0 0 7 0 0 0 6032 0 8 4294903637 1266 0 0 0 818 202 208 98 2 882 912 98 2 610 585 31 610 131 41 6032 882 1424 98 1 8 'Formato:' 6032 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 36 1 0 0 15 0 0 0 101 1 0 0 35 0 0 0] 98 0 1040 0 27 2016 1040 0 27 )! !
!CineNuevaFuncionPelicula class categoriesFor: #defaultModel!public! !
!CineNuevaFuncionPelicula class categoriesFor: #resource_Default_view!public!resources-views! !

CineNuevaPelicula guid: (GUID fromString: '{A354957D-ADA4-4EB0-97F1-2A6297A3AA48}')!
CineNuevaPelicula comment: ''!
!CineNuevaPelicula categoriesForClass!MVP-Presenters! !
!CineNuevaPelicula methodsFor!

createComponents
	super createComponents.
	tituloPresenter := self add: TextPresenter new name: 'titulo'.
	duracionPresenter := self add: NumberPresenter new name: 'duracion'.
	precioPresenter := self add: NumberPresenter new name: 'precio'.
	formatoPresenter := self add: ChoicePresenter new name: 'formato'.
	formatoElegido := self add: TextPresenter new name: 'formatoElegido'!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	formatoPresenter
		when: #actionPerformed
		send: #editFormato
		to: self!

editFormato
	| formato |
	formato := self selectedFormatoOrNil.
	formato notNil ifTrue: [self model formato: formato]!

hasSelectedFormato
	^formatoPresenter hasSelection!

model: aPeliculaModel
	| aspectBuffer |
	super model: aPeliculaModel.
	aspectBuffer := self model.
	tituloPresenter model: (aspectBuffer aspectValue: #titulo).
	duracionPresenter model: (aspectBuffer aspectValue: #duracion).
	precioPresenter model: (aspectBuffer aspectValue: #precio).
	formatoPresenter model: aspectBuffer formatos.
	formatoElegido model: (aspectBuffer aspectValue: #formato)!

onViewOpened
	super onViewOpened.
	self view text: 'Agregar Nueva Pelicula'.
	formatoPresenter
		choices: #('2D' '3D' '4D' 'IMAX');
		value: '2D'.
	self editFormato!

selectedFormatoOrNil
	^formatoPresenter getValue!

selectedFormatoOrNil: aFormatoOrNil
	^formatoPresenter selectionOrNil: aFormatoOrNil! !
!CineNuevaPelicula categoriesFor: #createComponents!public! !
!CineNuevaPelicula categoriesFor: #createSchematicWiring!public! !
!CineNuevaPelicula categoriesFor: #editFormato!public! !
!CineNuevaPelicula categoriesFor: #hasSelectedFormato!public! !
!CineNuevaPelicula categoriesFor: #model:!public! !
!CineNuevaPelicula categoriesFor: #onViewOpened!public! !
!CineNuevaPelicula categoriesFor: #selectedFormatoOrNil!public! !
!CineNuevaPelicula categoriesFor: #selectedFormatoOrNil:!public! !

!CineNuevaPelicula class methodsFor!

defaultModel
	^PeliculaModel new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.DialogView) 98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 167 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 416 788230 ##(Smalltalk.BorderLayout) 1 1 0 0 0 0 0 234 256 98 8 410 8 ##(Smalltalk.ComboBox) 98 17 0 416 98 2 8 1412498946 1025 704 590662 2 ##(Smalltalk.ListModel) 202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 482 8 4278190080 0 7 0 0 0 704 0 8 4294903515 459270 ##(Smalltalk.Message) 8 #displayString 98 0 832 401 983302 ##(Smalltalk.MessageSequence) 202 208 98 1 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 610 181 205 610 251 55 704 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 90 0 0 0 102 0 0 0 215 0 0 0 129 0 0 0] 98 0 610 193 193 0 27 8 'formato' 410 8 ##(Smalltalk.TextEdit) 98 16 0 416 98 2 8 1140924416 1025 1248 0 482 8 4278190080 0 7 0 0 0 1248 0 8 4294903511 852742 ##(Smalltalk.IntegerToText) 0 8 '' 0 1 994 202 208 98 3 1058 1088 98 2 610 185 119 610 101 51 1248 1058 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval) 3 1 3 1248 1058 8 #isTextModified: 98 1 32 1248 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 92 0 0 0 59 0 0 0 142 0 0 0 84 0 0 0] 98 0 1216 0 27 8 'duracion' 410 1264 98 16 0 416 98 2 8 1140916352 1025 1728 0 482 8 4278190080 0 7 0 0 0 1728 0 8 4294903511 852486 ##(Smalltalk.NullConverter) 0 0 1 994 202 208 98 3 1058 1088 98 2 610 181 31 610 661 55 1728 1058 1552 98 1 1586 3 1 3 1728 1058 1632 98 1 32 1728 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 90 0 0 0 15 0 0 0 164 1 0 0 42 0 0 0] 98 0 1216 0 27 8 'titulo' 410 1264 98 16 0 416 98 2 8 1140916224 1025 2128 0 482 8 4278190080 0 7 0 0 0 2128 0 8 4294903511 787206 ##(Smalltalk.NumberToText) 0 8 '' 0 1 994 202 208 98 3 1058 1088 98 2 610 655 119 610 151 51 2128 1058 1552 98 1 1586 3 1 3 2128 1058 1632 98 1 32 2128 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 71 1 0 0 59 0 0 0 146 1 0 0 84 0 0 0] 98 0 1216 0 27 8 'precio' 590342 ##(Smalltalk.Rectangle) 610 21 21 610 21 21 0 0 0 0 20879 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore) 0 0 1 0 8 2010780864 994 202 208 98 2 1058 1088 98 2 610 3839 21 610 901 361 416 1058 8 #updateMenuBar 832 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 65 9 0 0 190 0 0 0] 98 12 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 65 2848 0 0 0 7 0 0 0 2848 0 8 4294903507 1842 0 0 0 994 202 208 98 2 1058 1088 98 2 610 21 41 610 141 55 2848 1058 8 #text: 98 1 8 'Titulo:' 2848 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 20 0 0 0 80 0 0 0 47 0 0 0] 98 0 1216 0 27 410 2864 98 16 0 416 98 2 8 1140850944 65 3184 0 0 0 7 0 0 0 3184 0 8 4294903507 1842 0 0 0 994 202 208 98 2 1058 1088 98 2 610 21 121 610 141 55 3184 1058 3088 98 1 8 'Duración:' 3184 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 60 0 0 0 80 0 0 0 87 0 0 0] 98 0 1216 0 27 410 2864 98 16 0 416 98 2 8 1140850944 65 3488 0 0 0 7 0 0 0 3488 0 8 4294903507 1842 0 0 0 994 202 208 98 2 1058 1088 98 2 610 551 121 610 101 55 3488 1058 3088 98 1 8 'Precio:' 3488 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 19 1 0 0 60 0 0 0 69 1 0 0 87 0 0 0] 98 0 1216 0 27 410 2864 98 16 0 416 98 2 8 1140850944 65 3792 0 0 0 7 0 0 0 3792 0 8 4294903507 1842 0 0 0 994 202 208 98 2 1058 1088 98 2 610 21 211 610 141 55 3792 1058 3088 98 1 8 'Formato:' 3792 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 105 0 0 0 80 0 0 0 132 0 0 0] 98 0 1216 0 27 1728 1248 2128 704 410 8 ##(Smalltalk.PushButton) 98 20 0 416 98 2 8 1140924416 1 4096 0 0 0 7 0 0 0 4096 0 8 4294903497 1180998 4 ##(Smalltalk.CommandDescription) 8 #ok 8 '&Agregar' 1 1 0 0 16 0 0 0 994 202 208 98 3 1058 1088 98 2 610 481 201 610 161 61 4096 1058 8 #isEnabled: 98 1 32 4096 1058 3088 98 1 8 '&Agregar' 4096 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 0 0 0 100 0 0 0 64 1 0 0 130 0 0 0] 98 0 1216 0 29 410 4112 98 20 0 416 98 2 8 1140924416 1 4512 0 0 0 7 0 0 0 4512 0 8 4294903497 4194 8 #cancel 8 '&Cancelar' 1 1 0 0 32 0 0 0 994 202 208 98 2 1058 1088 98 2 610 681 201 610 161 61 4512 1058 3088 98 1 8 '&Cancelar' 4512 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 84 1 0 0 100 0 0 0 164 1 0 0 130 0 0 0] 98 0 1216 0 29 410 2864 98 16 0 416 98 2 8 1140850944 65 4848 0 0 0 7 0 0 0 4848 0 8 4294903507 1842 0 0 0 994 202 208 98 2 1058 1088 98 2 610 285 119 610 241 41 4848 1058 3088 98 1 8 '(max 300 mins.)' 4848 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 142 0 0 0 59 0 0 0 6 1 0 0 79 0 0 0] 98 0 1216 0 27 410 2864 98 16 0 416 98 2 8 1140850944 65 5152 0 0 0 7 0 0 0 5152 0 8 4294903507 1842 0 0 0 994 202 208 98 2 1058 1088 98 2 610 811 121 610 31 41 5152 1058 3088 98 1 8 '$' 5152 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 149 1 0 0 60 0 0 0 164 1 0 0 80 0 0 0] 98 0 1216 0 27 1216 0 27 )! !
!CineNuevaPelicula class categoriesFor: #defaultModel!public! !
!CineNuevaPelicula class categoriesFor: #resource_Default_view!public!resources-views! !

CineNuevaReserva guid: (GUID fromString: '{F9351246-FC17-48B5-8226-C8BE629398AB}')!
CineNuevaReserva comment: ''!
!CineNuevaReserva categoriesForClass!MVP-Presenters! !
!CineNuevaReserva methodsFor!

actualizarCliente
	self hasSelectedCliente
		ifTrue: 
			[idCliente model value: clientes selection verIdCliente.
			nombreCompleto model value: clientes selection verNombre , ' ' , clientes selection verApellido.
			dni model value: clientes selection verDni.
			telefono model value: clientes selection verTelefono]
		ifFalse: 
			[self model idCliente: 0.
			nombreCompleto model value: 'N/A'.
			dni model value: 'N/A'.
			telefono model value: 'N/A']!

actualizarPrecio
	| precioFinal precioPorAsiento |
	precioPorAsiento := (ownerView model buscarPeliculaDeLaFuncion: self model idFuncionPelicula)
				verPrecio.
	precioFinal := precioPorAsiento * model cantAsientosSeleccionados.
	precioTotal model: '$' , precioFinal printString!

buscar
	| cliente listaClientes |
	buscador model value ~= ''
		ifTrue: 
			[cliente := ownerView model buscarUnCliente: buscador model value.
			listaClientes := ownerView model buscarVariosClientes: buscador model value.
			clientes model: listaClientes.
			self clienteSelectedOrNil: cliente]
		ifFalse: 
			[self clienteSelectedOrNil: nil.
			clientes model: ownerView model verClientes]!

clienteSelectedOrNil
	^clientes selectionOrNil!

clienteSelectedOrNil: unClienteSelectedOrNil
	clientes selectionOrNil: unClienteSelectedOrNil!

createComponents
	super createComponents.
	buscador := self add: TextPresenter new name: 'buscador'.
	clientes := self add: ListPresenter new name: 'clientes'.
	idCliente := self add: TextPresenter new name: 'idCliente'.
	nombreCompleto := self add: TextPresenter new name: 'nombreCompleto'.
	dni := self add: TextPresenter new name: 'dni'.
	telefono := self add: TextPresenter new name: 'telefono'.
	titulo := self add: TextPresenter new name: 'titulo'.
	formato := self add: TextPresenter new name: 'formato'.
	sala := self add: TextPresenter new name: 'sala'.
	dia := self add: TextPresenter new name: 'dia'.
	horario := self add: TextPresenter new name: 'horario'.
	idioma := self add: TextPresenter new name: 'idioma'.
	okCancel := self add: Presenter new name: 'okCancel'.
	asientos := self add: ListPresenter new name: 'asientos'.
	precioUnitario := self add: TextPresenter new name: 'precioUnitario'.
	precioTotal := self add: TextPresenter new name: 'precioTotal'!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	buscador value
		when: #valueChanged
		send: #buscar
		to: self.
	clientes
		when: #selectionChanged
		send: #actualizarCliente
		to: self.
	asientosPresenter := OrderedCollection new.
	self dibujarAsientosDesdeX: 300 desdeY: 200.
	asientosPresenter do: 
			[:booleanPresenter |
			booleanPresenter
				when: #valueChanged
				send: #actualizarPrecio
				to: self]!

dibujarAsientos
	"Asientos de la sala"

	| tpx tpy bp asientosDisp asientosRes cantFilas cantColumnas |
	asientosDisp := ownerView model verAsientosDisponibles: self model idFuncionPelicula
				sala: (ownerView model buscarFuncionPelicula: self model idFuncionPelicula) verNombreSalaAsignada.
	asientosRes := ownerView model verAsientosReservados: self model idFuncionPelicula
				sala: (ownerView model buscarFuncionPelicula: self model idFuncionPelicula) verNombreSalaAsignada.
	"El valor maximo de columna y fila"
	cantFilas := asientosDisp , asientosRes inject: (asientosDisp first at: 1)
				into: [:max :subCollection | (subCollection at: 1) > max ifTrue: [subCollection at: 1] ifFalse: [max]].
	cantColumnas := asientosDisp , asientosRes inject: (asientosDisp first at: 2)
				into: [:max :subCollection | (subCollection at: 2) > max ifTrue: [subCollection at: 2] ifFalse: [max]].

	"Dibujar Los numeros que representan el numero de Fila y Columna"
	1 to: cantColumnas
		do: 
			[:x |
			1 to: cantFilas
				do: 
					[:y |
					y = 1
						ifTrue: 
							[tpx := TextPresenter createIn: self.
							tpx extent: 30 @ 30.
							tpx position: (x * 30 + 160) @ 155.
							tpx view text: x printString.
							tpx view isReadOnly: true].
					x = cantFilas
						ifTrue: 
							[tpy := TextPresenter createIn: self.
							tpy extent: 30 @ 30.
							tpy position: 160 @ (y * 30 + 155).
							tpy view text: y printString.
							tpy view isReadOnly: true]]].
	"Dibujar los Asientos disponibles"
	1 to: asientosDisp size
		do: 
			[:i |
			"asientos disponibles"

			"Instancia un asientoReservaModel que representa el asiento seleccionado"
			| asi |
			asi := AsientoReservaModel new.
			asi fila: ((asientosDisp at: i) at: 1).
			asi columna: ((asientosDisp at: i) at: 2).
			asi estado: ((asientosDisp at: i) at: 3).
			"Agrega el asiento a la lista de asientos que luego se va a utilizar para la reserva si estado = true"
			self model asientos add: asi.
			"crea el checkbox que se utiliza para seleccionar el asiento"
			bp := BooleanPresenter createIn: self.
			bp extent: 30 @ 30.
			bp position: (((asientosDisp at: i) at: 2) * 30 + 160) @ (((asientosDisp at: i) at: 1) * 30 + 155).
			bp view isPushLike: true.
			bp view
				name: ((asientosDisp at: i) at: 1) printString , '-' , ((asientosDisp at: i) at: 2) printString.
			bp view value: asi estado.
			bp view isEnabled: true.
			bp view hasClientEdge: false.
			bp view hasBorder: false.
			"Vincula el model con el checkbox de selexion de asiento"
			bp model: ((self model asientos at: i) aspectValue: #estado)].
	"Dibujar los asientos reservados"
	1 to: asientosRes size
		do: 
			[:i |
			"asientos reservados"
			bp := BooleanPresenter createIn: self.
			bp extent: 30 @ 30.
			bp position: (((asientosRes at: i) at: 2) * 30 + 160) @ (((asientosRes at: i) at: 1) * 30 + 155).
			bp view isPushLike: true.
			bp view value: ((asientosRes at: i) at: 3).
			bp view isEnabled: false.
			bp view hasClientEdge: true.
			bp view hasBorder: true]!

dibujarAsientosDesdeX: xPos desdeY: yPos
	"Asientos de la sala"

	| tpx tpy bp asientosDisp asientosRes cantFilas cantColumnas |
	asientosDisp := ownerView model verAsientosDisponibles: self model idFuncionPelicula
				sala: (ownerView model buscarFuncionPelicula: self model idFuncionPelicula) verNombreSalaAsignada.
	asientosRes := ownerView model verAsientosReservados: self model idFuncionPelicula
				sala: (ownerView model buscarFuncionPelicula: self model idFuncionPelicula) verNombreSalaAsignada.
	"El valor maximo de columna y fila"
	cantFilas := asientosDisp , asientosRes inject: (asientosDisp first at: 1)
				into: [:max :subCollection | (subCollection at: 1) > max ifTrue: [subCollection at: 1] ifFalse: [max]].
	cantColumnas := asientosDisp , asientosRes inject: (asientosDisp first at: 2)
				into: [:max :subCollection | (subCollection at: 2) > max ifTrue: [subCollection at: 2] ifFalse: [max]].

	"Dibujar Los numeros que representan el numero de Fila y Columna"
	1 to: cantColumnas
		do: 
			[:x |
			1 to: cantFilas
				do: 
					[:y |
					y = 1
						ifTrue: 
							[tpx := TextPresenter createIn: self.
							tpx extent: 30 @ 30.
							tpx position: (x * 30 + xPos) @ yPos.
							tpx view text: x printString.
							tpx view isReadOnly: true].
					x = cantFilas
						ifTrue: 
							[tpy := TextPresenter createIn: self.
							tpy extent: 30 @ 30.
							tpy position: xPos @ (y * 30 + yPos).
							tpy view text: y printString.
							tpy view isReadOnly: true]]].
	"Dibujar los Asientos disponibles"
	1 to: asientosDisp size
		do: 
			[:i |
			"Instancia un asientoReservaModel que representa el asiento seleccionado"
			| asi |
			asi := AsientoReservaModel new.
			asi fila: ((asientosDisp at: i) at: 1).
			asi columna: ((asientosDisp at: i) at: 2).
			asi estado: ((asientosDisp at: i) at: 3).
			"Agrega el asiento a la lista de asientos que luego se va a utilizar para la reserva si estado = true"
			self model asientos add: asi.
			"crea el checkbox que se utiliza para seleccionar el asiento"
			bp := BooleanPresenter createIn: self.
			bp extent: 30 @ 30.
			bp position: (((asientosDisp at: i) at: 2) * 30 + xPos) @ (((asientosDisp at: i) at: 1) * 30 + yPos).
			bp view isPushLike: true.
			bp view
				name: ((asientosDisp at: i) at: 1) printString , '-' , ((asientosDisp at: i) at: 2) printString.
			bp view value: asi estado.
			bp view isEnabled: true.
			bp view hasClientEdge: false.
			bp view hasBorder: false.
			"Vincula el model con el checkbox de selexion de asiento"
			bp model: ((self model asientos at: i) aspectValue: #estado).
			asientosPresenter add: bp].
	"Dibujar los asientos reservados"
	1 to: asientosRes size
		do: 
			[:i |
			bp := BooleanPresenter createIn: self.
			bp extent: 30 @ 30.
			bp position: (((asientosRes at: i) at: 2) * 30 + xPos) @ (((asientosRes at: i) at: 1) * 30 + yPos).
			bp view isPushLike: true.
			bp view value: ((asientosRes at: i) at: 3).
			bp view isEnabled: false.
			bp view hasClientEdge: true.
			bp view hasBorder: true]!

hasSelectedCliente
	^clientes hasSelection!

model: unaReservaModel
	super model: unaReservaModel.
	idCliente model: (unaReservaModel aspectValue: #idCliente).
	asientos model: (unaReservaModel aspectValue: #asientos).
	clientes model: ownerView model verClientes.
	titulo
		model: (ownerView model buscarFuncionPelicula: self model idFuncionPelicula) verNombrePelicula.
	sala
		model: (ownerView model buscarFuncionPelicula: self model idFuncionPelicula) verNombreSalaAsignada.
	formato model: (ownerView model buscarPeliculaDeLaFuncion: self model idFuncionPelicula) verFormato.
	dia model: (ownerView model buscarFuncionPelicula: self model idFuncionPelicula) verDia.
	horario
		model: (ownerView model buscarFuncionPelicula: self model idFuncionPelicula) verHorarioConFormato.
	idioma model: (ownerView model buscarFuncionPelicula: self model idFuncionPelicula) verIdioma.
	precioUnitario model: '$'
				, (ownerView model buscarPeliculaDeLaFuncion: self model idFuncionPelicula) verPrecio printString.
	precioTotal model: '$0'.
	idCliente model aspectTriggers: #valueChanged!

onViewOpened
	super onViewOpened.
	self view text: 'Menu Reserva'.
	self actualizarCliente!

queryCommand: aCommandQuery
	"Enters details about a potential command for the receiver into aCommandQuery"

	super queryCommand: aCommandQuery.
	(#(#ok) includes: aCommandQuery command) ifTrue: [aCommandQuery isEnabled: self hasSelectedCliente]! !
!CineNuevaReserva categoriesFor: #actualizarCliente!public! !
!CineNuevaReserva categoriesFor: #actualizarPrecio!public! !
!CineNuevaReserva categoriesFor: #buscar!public! !
!CineNuevaReserva categoriesFor: #clienteSelectedOrNil!public! !
!CineNuevaReserva categoriesFor: #clienteSelectedOrNil:!public! !
!CineNuevaReserva categoriesFor: #createComponents!public! !
!CineNuevaReserva categoriesFor: #createSchematicWiring!public! !
!CineNuevaReserva categoriesFor: #dibujarAsientos!public! !
!CineNuevaReserva categoriesFor: #dibujarAsientosDesdeX:desdeY:!public! !
!CineNuevaReserva categoriesFor: #hasSelectedCliente!public! !
!CineNuevaReserva categoriesFor: #model:!public! !
!CineNuevaReserva categoriesFor: #onViewOpened!public! !
!CineNuevaReserva categoriesFor: #queryCommand:!public! !

!CineNuevaReserva class methodsFor!

defaultModel
	^ReservaModel new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.DialogView) 98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout) 1 1 0 0 0 0 0 234 256 98 18 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 65 592 0 0 0 7 0 0 0 592 0 8 4294904529 852486 ##(Smalltalk.NullConverter) 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 2 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point) 861 191 850 451 41 592 786 8 #text: 98 1 8 'Static Text' 592 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 174 1 0 0 95 0 0 0 143 2 0 0 115 0 0 0] 98 0 850 193 193 0 27 8 'telefono' 410 8 ##(Smalltalk.ListBox) 98 17 0 416 98 2 8 1412497665 1025 1056 590662 2 ##(Smalltalk.ListModel) 202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 482 8 4278190080 0 7 0 0 0 1056 0 8 4294904423 459270 ##(Smalltalk.Message) 8 #displayString 98 0 1184 32 722 202 208 98 2 786 816 98 2 850 31 401 850 501 1181 1056 786 8 #horizontalExtent: 98 1 1001 1056 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 200 0 0 0 9 1 0 0 22 3 0 0] 98 0 1024 0 27 8 'clientes' 410 608 98 16 0 416 98 2 8 1140850944 65 1568 0 0 0 7 0 0 0 1568 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 861 151 850 451 41 1568 786 912 98 1 8 'Static Text' 1568 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 174 1 0 0 75 0 0 0 143 2 0 0 95 0 0 0] 98 0 1024 0 27 8 'dni' 410 608 98 16 0 416 98 2 8 1140850944 65 1888 0 0 0 7 0 0 0 1888 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 861 51 850 451 41 1888 786 912 98 1 8 'Static Text' 1888 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 174 1 0 0 25 0 0 0 143 2 0 0 45 0 0 0] 98 0 1024 0 27 8 'idCliente' 410 608 98 16 0 416 98 2 8 1140850944 65 2208 0 0 0 7 0 0 0 2208 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 861 111 850 451 41 2208 786 912 98 1 8 'Static Text' 2208 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 174 1 0 0 55 0 0 0 143 2 0 0 75 0 0 0] 98 0 1024 0 27 8 'nombreCompleto' 410 8 ##(Smalltalk.TextEdit) 98 16 0 416 98 2 8 1140916352 1025 2528 0 482 8 4278190080 0 7 0 0 0 2528 0 8 4294904283 690 0 0 1 722 202 208 98 3 786 816 98 2 850 131 341 850 281 51 2528 786 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval) 3 1 3 2528 786 8 #isTextModified: 98 1 32 2528 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 65 0 0 0 170 0 0 0 205 0 0 0 195 0 0 0] 98 0 1024 0 27 8 'buscador' 410 608 98 16 0 416 98 2 8 1140850944 65 2976 0 0 0 7 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 255 0 0 0 3 2 1 2 82 111 98 111 116 111 0 76 105 103 104 116 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 850 193 193 0 2976 196934 1 ##(Smalltalk.RGB) 8454145 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 311 21 850 241 39 2976 786 912 98 1 8 'Static Text' 2976 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 0 0 0 10 0 0 0 19 1 0 0 29 0 0 0] 98 0 1024 0 27 8 'precioUnitario' 410 608 98 16 0 416 98 2 8 1140850944 65 3424 0 0 0 7 0 3042 0 16 3074 8 #[232 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 255 0 0 0 3 2 1 2 82 111 98 111 116 111 0 76 105 103 104 116 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 3120 0 3424 3138 8454145 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 35 159 850 461 61 3424 786 912 98 1 8 'Static Text' 3424 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 17 0 0 0 79 0 0 0 247 0 0 0 109 0 0 0] 98 0 1024 0 27 8 'precioTotal' 410 8 ##(Smalltalk.PushButton) 98 20 0 416 98 2 8 1140924416 1 3808 0 0 0 7 0 0 0 3808 0 8 4294904537 1180998 4 ##(Smalltalk.CommandDescription) 8 #ok 8 '&Reservar' 1 1 0 0 16 0 0 0 722 202 208 98 3 786 816 98 2 850 71 251 850 161 61 3808 786 8 #isEnabled: 98 1 32 3808 786 912 98 1 8 '&Reservar' 3808 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 0 0 0 125 0 0 0 115 0 0 0 155 0 0 0] 98 0 1024 0 29 8 'okBtn' 590342 ##(Smalltalk.Rectangle) 850 21 21 850 21 21 0 0 0 0 31205 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore) 0 0 1 0 8 1997870272 722 202 208 98 2 786 816 98 2 850 3839 21 850 1941 1681 416 786 8 #updateMenuBar 1184 416 962 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 73 11 0 0 82 3 0 0] 98 24 2528 1056 410 8 ##(Smalltalk.StyledContainer) 98 22 0 416 98 2 8 1174405120 131073 4544 0 0 0 7 0 0 0 4544 0 234 256 98 12 410 608 98 16 0 4544 98 2 8 1140850944 65 4656 0 0 0 7 0 0 0 4656 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 161 51 850 281 39 4656 786 912 98 1 8 'Static Text' 4656 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 25 0 0 0 220 0 0 0 44 0 0 0] 98 0 1024 0 27 8 'sala' 410 608 98 16 0 4544 98 2 8 1140850944 65 4976 0 0 0 7 0 0 0 4976 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 161 131 850 281 39 4976 786 912 98 1 8 'Static Text' 4976 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 65 0 0 0 220 0 0 0 84 0 0 0] 98 0 1024 0 27 8 'horario' 410 608 98 16 0 4544 98 2 8 1140850944 65 5296 0 0 0 7 0 0 0 5296 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 161 171 850 281 39 5296 786 912 98 1 8 'Static Text' 5296 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 85 0 0 0 220 0 0 0 104 0 0 0] 98 0 1024 0 27 8 'idioma' 410 608 98 16 0 4544 98 2 8 1140850944 65 5616 0 0 0 7 0 0 0 5616 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 161 211 850 281 39 5616 786 912 98 1 8 'Static Text' 5616 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 105 0 0 0 220 0 0 0 124 0 0 0] 98 0 1024 0 27 8 'formato' 410 608 98 16 0 4544 98 2 8 1140850944 65 5936 0 0 0 7 0 0 0 5936 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 161 91 850 281 39 5936 786 912 98 1 8 'Static Text' 5936 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 45 0 0 0 220 0 0 0 64 0 0 0] 98 0 1024 0 27 8 'dia' 410 608 98 16 0 4544 98 2 8 1140850944 65 6256 0 0 0 7 0 0 0 6256 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 161 11 850 281 39 6256 786 912 98 1 8 'Static Text' 6256 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 5 0 0 0 220 0 0 0 24 0 0 0] 98 0 1024 0 27 8 'titulo' 0 202 208 98 4 8 #top 8 #bottom 8 #left 8 #right 590598 ##(Smalltalk.StyledPen) 786694 ##(Smalltalk.IndexedColor) 33554433 5 98 1 3 1246214 ##(Smalltalk.StyledGradientBrush) 721158 ##(Smalltalk.SystemColor) 31 0 850 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 1 850 6834 8 0 0 0 0 0 0 224 63 3 98 4 41 41 41 41 16 0 1 722 202 208 98 1 786 816 98 2 850 1405 19 850 471 251 4544 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 190 2 0 0 9 0 0 0 169 3 0 0 134 0 0 0] 98 12 410 608 98 16 0 4544 98 2 8 1140850944 65 7072 0 0 0 7 0 0 0 7072 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 21 11 850 101 39 7072 786 912 98 1 8 'Pelicula:' 7072 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 5 0 0 0 60 0 0 0 24 0 0 0] 98 0 1024 0 27 410 608 98 16 0 4544 98 2 8 1140850944 65 7376 0 0 0 7 0 0 0 7376 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 21 91 850 101 39 7376 786 912 98 1 8 'Dia:' 7376 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 45 0 0 0 60 0 0 0 64 0 0 0] 98 0 1024 0 27 410 608 98 16 0 4544 98 2 8 1140850944 65 7680 0 0 0 7 0 0 0 7680 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 21 131 850 101 39 7680 786 912 98 1 8 'Horario:' 7680 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 65 0 0 0 60 0 0 0 84 0 0 0] 98 0 1024 0 27 410 608 98 16 0 4544 98 2 8 1140850944 65 7984 0 0 0 7 0 0 0 7984 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 21 171 850 101 39 7984 786 912 98 1 8 'Idioma:' 7984 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 85 0 0 0 60 0 0 0 104 0 0 0] 98 0 1024 0 27 410 608 98 16 0 4544 98 2 8 1140850944 65 8288 0 0 0 7 0 0 0 8288 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 21 51 850 101 39 8288 786 912 98 1 8 'Sala:' 8288 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 25 0 0 0 60 0 0 0 44 0 0 0] 98 0 1024 0 27 6256 4656 5936 4976 5296 410 608 98 16 0 4544 98 2 8 1140850944 65 8592 0 0 0 7 0 0 0 8592 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 21 211 850 101 39 8592 786 912 98 1 8 'Formato:' 8592 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 105 0 0 0 60 0 0 0 124 0 0 0] 98 0 1024 0 27 5616 1024 0 27 410 8 ##(Smalltalk.GroupBox) 98 14 0 416 98 2 8 1140850695 263233 8896 0 482 8 4278190080 0 7 0 0 0 8896 0 8 4294904537 722 202 208 98 1 786 816 98 2 850 561 19 850 771 231 8896 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 24 1 0 0 9 0 0 0 153 2 0 0 124 0 0 0] 98 0 1024 0 27 410 608 98 16 0 416 98 2 8 1140850944 1 9184 0 0 0 7 0 0 0 9184 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 601 51 850 161 39 9184 786 912 98 1 8 'Id. Cliente:' 9184 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 1 0 0 25 0 0 0 124 1 0 0 44 0 0 0] 98 0 1024 0 27 410 608 98 16 0 416 98 2 8 1140850944 65 9488 0 0 0 7 0 0 0 9488 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 601 111 850 221 39 9488 786 912 98 1 8 'Nombre y Apellido:' 9488 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 1 0 0 55 0 0 0 154 1 0 0 74 0 0 0] 98 0 1024 0 27 410 608 98 16 0 416 98 2 8 1140850944 65 9792 0 0 0 7 0 0 0 9792 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 601 151 850 121 39 9792 786 912 98 1 8 'D.N.I:' 9792 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 1 0 0 75 0 0 0 104 1 0 0 94 0 0 0] 98 0 1024 0 27 410 608 98 16 0 416 98 2 8 1140850944 65 10096 0 0 0 7 0 0 0 10096 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 601 191 850 121 39 10096 786 912 98 1 8 'Telefono:' 10096 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 1 0 0 95 0 0 0 104 1 0 0 114 0 0 0] 98 0 1024 0 27 410 8 ##(Smalltalk.Splitter) 98 12 0 416 98 2 8 1140850688 1 10400 0 482 8 4278190080 0 519 0 0 0 10400 722 202 208 98 1 786 816 98 2 850 841 45 850 5 197 10400 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 164 1 0 0 22 0 0 0 166 1 0 0 120 0 0 0] 98 0 1024 0 27 1888 2208 1568 592 410 608 98 16 0 416 98 2 8 1140850944 65 10672 0 0 0 7 0 0 0 10672 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 601 271 850 241 39 10672 786 912 98 1 8 'SeleccionarAsientos:' 10672 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 1 0 0 135 0 0 0 164 1 0 0 154 0 0 0] 98 0 1024 0 27 410 608 98 16 0 416 98 2 8 1140850944 65 10976 0 0 0 7 0 0 0 10976 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 41 351 850 91 39 10976 786 912 98 1 8 'Buscar:' 10976 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 0 0 0 175 0 0 0 65 0 0 0 194 0 0 0] 98 0 1024 0 27 410 3824 98 20 0 416 98 2 8 1140924416 1 11280 0 0 0 7 0 3042 0 16 3074 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 105 108 108 101 32 79 108 100 32 70 97 99 101 0 0 0 0 0 0 0 0 0 0 0 0] 850 193 193 0 11280 0 8 4294904537 3906 8 #buscar 8 'buscar' 1 1 0 0 32 0 0 0 722 202 208 98 3 786 816 98 2 850 417 341 850 117 55 11280 786 4096 98 1 32 11280 786 912 98 1 8 'buscar' 11280 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 208 0 0 0 170 0 0 0 10 1 0 0 197 0 0 0] 98 0 1024 0 29 3808 410 3824 98 20 0 416 98 2 8 1140924416 1 11712 0 0 0 7 0 0 0 11712 0 8 4294904537 3906 8 #cancel 8 '&Cancelar' 1 1 0 0 32 0 0 0 722 202 208 98 2 786 816 98 2 850 301 251 850 161 61 11712 786 912 98 1 8 '&Cancelar' 11712 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 125 0 0 0 230 0 0 0 155 0 0 0] 98 0 1024 0 29 410 8 ##(Smalltalk.StaticBitmap) 98 17 0 416 98 2 8 1149239566 1 12048 721990 2 ##(Smalltalk.ValueHolder) 0 0 1376774 ##(Smalltalk.PluggableSearchPolicy) 1282 8 #== 98 0 1282 8 #hash 98 0 0 6720 0 7 0 0 0 12048 0 8 4294904529 690 0 0 32 0 722 202 208 98 1 786 816 98 2 850 811 321 850 441 21 12048 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 149 1 0 0 160 0 0 0 113 2 0 0 170 0 0 0] 98 0 1024 0 27 410 608 98 16 0 416 98 2 8 1140850944 65 12480 0 0 0 7 0 3042 0 16 3074 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 850 193 193 0 12480 3138 33488897 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 951 281 850 141 41 12480 786 912 98 1 8 'Pantalla' 12480 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 219 1 0 0 140 0 0 0 33 2 0 0 160 0 0 0] 98 0 1024 0 27 410 608 98 16 0 416 98 2 8 1140850944 65 12864 0 0 0 7 0 3042 0 16 3074 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 2 82 117 115 115 111 32 79 110 101 0 0 108 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 3120 0 12864 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 31 21 850 275 39 12864 786 912 98 1 8 'Precio x Asiento:' 12864 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 10 0 0 0 152 0 0 0 29 0 0 0] 98 0 1024 0 27 2976 410 608 98 16 0 416 98 2 8 1140850944 65 13216 0 0 0 7 0 3042 0 16 3074 8 #[232 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 2 82 117 115 115 111 32 79 110 101 0 0 108 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 3120 0 13216 0 8 4294904529 690 0 0 0 722 202 208 98 2 786 816 98 2 850 31 101 850 441 61 13216 786 912 98 1 8 'Precio Total:' 13216 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 50 0 0 0 235 0 0 0 80 0 0 0] 98 0 1024 0 27 3424 1024 0 27 )! !
!CineNuevaReserva class categoriesFor: #defaultModel!public! !
!CineNuevaReserva class categoriesFor: #resource_Default_view!public!resources-views! !

CineNuevaSala guid: (GUID fromString: '{D32CC11E-7B01-4DDA-ACB2-9E392AA11FA2}')!
CineNuevaSala comment: ''!
!CineNuevaSala categoriesForClass!MVP-Presenters! !
!CineNuevaSala methodsFor!

createComponents
	super createComponents.
	nombre := self add: TextPresenter new name: 'nombre'.
	tipo := self add: ChoicePresenter new name: 'tipo'.
	cantidadFilas := self add: NumberPresenter new name: 'cantidadFilas'.
	cantidadColumnas := self add: NumberPresenter new name: 'cantidadColumnas'.
	tipoElegido := self add: TextPresenter new name: 'tipoElegido'!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	tipo
		when: #actionPerformed
		send: #editTipo
		to: self!

editTipo
	| formato |
	formato := self selectedTipoOrNil.
	formato notNil ifTrue: [self model tipo: formato]!

hasSelectedTipo
	^tipo hasSelection!

model: aSalaModel
	| aspectBuffer |
	super model: aSalaModel.
	aspectBuffer := self model.
	nombre model: (aspectBuffer aspectValue: #nombre).
	tipoElegido model: (aspectBuffer aspectValue: #tipo).
	cantidadFilas model: (aspectBuffer aspectValue: #cantidadFilas).
	cantidadColumnas model: (aspectBuffer aspectValue: #cantidadColumnas)!

onViewOpened
	super onViewOpened.
	self view text: 'Agregar Nueva Sala'.
	tipo choices: #('2D' '3D' '4D' 'IMAX');
		value: '2D'.
	self editTipo!

selectedTipoOrNil
	^tipo getValue! !
!CineNuevaSala categoriesFor: #createComponents!public! !
!CineNuevaSala categoriesFor: #createSchematicWiring!public! !
!CineNuevaSala categoriesFor: #editTipo!public! !
!CineNuevaSala categoriesFor: #hasSelectedTipo!public! !
!CineNuevaSala categoriesFor: #model:!public! !
!CineNuevaSala categoriesFor: #onViewOpened!public! !
!CineNuevaSala categoriesFor: #selectedTipoOrNil!public! !

!CineNuevaSala class methodsFor!

defaultModel
	^SalaModel new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.DialogView) 98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 167 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 84 97 104 111 109 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 416 788230 ##(Smalltalk.BorderLayout) 1 1 0 410 8 ##(Smalltalk.ReferenceView) 98 14 0 416 98 2 8 1140850688 131073 672 0 0 0 7 0 0 0 672 1180166 ##(Smalltalk.ResourceIdentifier) 8 ##(Smalltalk.Presenter) 8 #resource_OK_Cancel_button_block 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 1 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 610 21 323 610 669 71 672 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 161 0 0 0 88 1 0 0 196 0 0 0] 98 0 610 193 193 0 27 0 0 0 234 256 98 8 410 8 ##(Smalltalk.TextEdit) 98 16 0 416 98 2 8 1140924416 1025 1088 0 482 8 4278190080 0 7 0 0 0 1088 0 8 4294904729 852742 ##(Smalltalk.IntegerToText) 0 8 '' 0 1 818 202 208 98 3 882 912 98 2 610 431 261 610 241 51 1088 882 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval) 3 1 3 1088 882 8 #isTextModified: 98 1 32 1088 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 215 0 0 0 130 0 0 0 79 1 0 0 155 0 0 0] 98 0 1040 0 27 8 'cantidadColumnas' 410 8 ##(Smalltalk.ComboBox) 98 17 0 416 98 2 8 1412498947 1025 1568 590662 2 ##(Smalltalk.ListModel) 202 208 1024 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 482 8 4278190080 0 7 0 0 0 1568 0 8 4294903585 459270 ##(Smalltalk.Message) 8 #displayString 98 0 1024 401 818 202 208 98 1 882 912 98 2 610 431 101 610 241 55 1568 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 215 0 0 0 50 0 0 0 79 1 0 0 77 0 0 0] 98 0 1040 0 27 8 'tipo' 410 1104 98 16 0 416 98 2 8 1140924416 1025 2016 0 482 1184 0 7 0 0 0 2016 0 8 4294904729 1218 0 1248 0 1 818 202 208 98 3 882 912 98 2 610 431 181 610 241 51 2016 882 1392 98 1 1426 3 1 3 2016 882 1472 98 1 32 2016 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 215 0 0 0 90 0 0 0 79 1 0 0 115 0 0 0] 98 0 1040 0 27 8 'cantidadFilas' 410 1104 98 16 0 416 98 2 8 1140916352 1025 2384 0 482 8 4278190080 0 7 0 0 0 2384 0 8 4294904729 852486 ##(Smalltalk.NullConverter) 0 0 1 818 202 208 98 3 882 912 98 2 610 431 21 610 241 51 2384 882 1392 98 1 1426 3 1 3 2384 882 1472 98 1 32 2384 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 215 0 0 0 10 0 0 0 79 1 0 0 35 0 0 0] 98 0 1040 0 27 8 'nombre' 590342 ##(Smalltalk.Rectangle) 610 21 21 610 21 21 0 0 0 0 22141 0 0 610 741 491 610 741 491 1 0 0 590598 ##(Smalltalk.Semaphore) 0 0 1 0 8 2003965120 818 202 208 98 2 882 912 98 2 610 3839 21 610 741 491 416 882 8 #updateMenuBar 1024 416 978 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 241 8 0 0 255 0 0 0] 98 9 672 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 65 3120 0 0 0 7 0 0 0 3120 0 8 4294903505 2498 0 0 0 818 202 208 98 2 882 912 98 2 610 31 31 610 271 39 3120 882 8 #text: 98 1 8 'Nombre de Sala:' 3120 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 15 0 0 0 150 0 0 0 34 0 0 0] 98 0 1040 0 27 410 3136 98 16 0 416 98 2 8 1140850944 65 3456 0 0 0 7 0 0 0 3456 0 8 4294903505 2498 0 0 0 818 202 208 98 2 882 912 98 2 610 31 111 610 221 39 3456 882 3360 98 1 8 'Tipo de Sala:' 3456 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 55 0 0 0 125 0 0 0 74 0 0 0] 98 0 1040 0 27 410 3136 98 16 0 416 98 2 8 1140850944 65 3760 0 0 0 7 0 0 0 3760 0 8 4294903505 2498 0 0 0 818 202 208 98 2 882 912 98 2 610 31 191 610 301 39 3760 882 3360 98 1 8 'Cantidad de Filas:' 3760 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 95 0 0 0 165 0 0 0 114 0 0 0] 98 0 1040 0 27 410 3136 98 16 0 416 98 2 8 1140850944 65 4064 0 0 0 7 0 0 0 4064 0 8 4294903505 2498 0 0 0 818 202 208 98 2 882 912 98 2 610 31 271 610 381 39 4064 882 3360 98 1 8 'Cantidad de Columnas:' 4064 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 135 0 0 0 205 0 0 0 154 0 0 0] 98 0 1040 0 27 2016 1088 2384 1568 1040 0 27 )! !
!CineNuevaSala class categoriesFor: #defaultModel!public! !
!CineNuevaSala class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

