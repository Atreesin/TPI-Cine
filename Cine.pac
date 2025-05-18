| package |
package := Package name: 'Cine'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Asiento;
	add: #Cine;
	add: #Cliente;
	add: #CuentaCliente;
	add: #FuncionPelicula;
	add: #Horario;
	add: #Pelicula;
	add: #Persona;
	add: #Reserva;
	add: #Sala;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter';
	yourself).

package!

"Class Definitions"!

Object subclass: #Asiento
	instanceVariableNames: 'fila columna estaReservado'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Cine
	instanceVariableNames: 'nombre salas cartelera candy empleados clientes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #CuentaCliente
	instanceVariableNames: 'thisNombreUsuario thisContrasenia thisPuntos thisReservas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #FuncionPelicula
	instanceVariableNames: 'idFuncionPelicula nombreSala nombrePelicula horario idioma'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Horario
	instanceVariableNames: 'inicio fin peliculaEnEmision'
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
	instanceVariableNames: 'thisIdReserva thisIdFuncionPelicula thisAsientos thisCosto'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Sala
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Persona subclass: #Cliente
	instanceVariableNames: 'thisIdCliente thisCuenta'
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

    ^estaReservado at: idFuncionPelicula ifAbsent: [^nil ].!

eliminarEstadoReserva: idFuncionPelicula
    
    estaReservado removeKey: idFuncionPelicula ifAbsent: [^nil ].!

iniAsientoFila: unaFila Columna: unaColumna
    fila := unaFila.
    columna := unaColumna.
    estaReservado := Dictionary new.!

liberarReserva: idFuncionPelicula
(estaReservado includesKey: idFuncionPelicula )
    ifTrue: [ estaReservado at: idFuncionPelicula put: false ]
    ifFalse: [ ^nil ].!

nuevoEstadoReserva: idFuncionPelicula
    
    ( estaReservado includesKey: idFuncionPelicula ) 
      ifTrue: [ ^nil ]
      ifFalse: [ estaReservado at: idFuncionPelicula put: false ].  
                             !

reservar: idFuncionPelicula

(estaReservado includesKey: idFuncionPelicula )
    ifTrue: [ estaReservado at: idFuncionPelicula put: true ]
    ifFalse: [ ^nil ].!

verColumna
^ columna.!

verFila
          ^fila. !

verReservas
    ^estaReservado.! !
!Asiento categoriesFor: #comprobarReserva:!public! !
!Asiento categoriesFor: #eliminarEstadoReserva:!public! !
!Asiento categoriesFor: #iniAsientoFila:Columna:!private! !
!Asiento categoriesFor: #liberarReserva:!public! !
!Asiento categoriesFor: #nuevoEstadoReserva:!public! !
!Asiento categoriesFor: #reservar:!public! !
!Asiento categoriesFor: #verColumna!public! !
!Asiento categoriesFor: #verFila!public! !
!Asiento categoriesFor: #verReservas!public! !

!Asiento class methodsFor!

crearAsientoFila: unaFila Columna: unaColumna

    ^self new iniAsientoFila: unaFila Columna: unaColumna.! !
!Asiento class categoriesFor: #crearAsientoFila:Columna:!public! !

Cine guid: (GUID fromString: '{66982B7D-A579-4656-BFD8-F742AB78A63D}')!
Cine comment: ''!
!Cine categoriesForClass!TPI-Cine! !
!Cine methodsFor!

agregarPelicula: unaPelicula
	(cartelera includesKey: unaPelicula verTitulo , unaPelicula verFormato)
		ifFalse: [cartelera at: unaPelicula verTitulo , unaPelicula verFormato put: unaPelicula]!

agregarSala: unaSala
	
	salas add: unaSala.!

crearSala

	| unaSala  nombreSala tipoSala|
	"A implementar"
	"opcion1
	salas detect:[:sal|sal verNombreSala = unaSala verNombreSala] ifNone:[salas add: unaSala].

	opcion 2 mas probable
	(salas includes: unaSala) ifTrue: [^nil]
						ifFalse: [salas add: unaSala ].
	
	esta opcion creo que seria la mejor:
	"
	nombreSala := Prompter prompt:'Ingrese el nombre de la sala'.
	tipoSala :=  Prompter prompt:'Ingrese el tipo de sala'.
	"unaSala := Sala crearSala...."
	
	(self seleccionarSala: unaSala verNombreSala) ifNil: [self agregarSala: unaSala ] 
										ifNotNil: [^'La sala ya existe.'].!

eliminarPelicula: unaPelicula
	cartelera removeKey: unaPelicula verTitulo , unaPelicula verFormato ifAbsent: [^nil]!

iniCine: unNombre

	nombre := unNombre.
	salas := OrderedCollection new.
	cartelera := Dictionary new.
	candy := 'No implementado'.
	empleados := 'No implementado'.
	clientes := OrderedCollection.!

nuevaPelicula
	| unaPelicula titPelicula durPelicula formato precio formatosDisp flag sigue |
	flag := true.
	sigue := true.
	"Elegir tipo de sala - primero comprobar que existe una sala de cada tipo (IMPLEMENTAR)"
	formatosDisp := #('2D' '3D' '4D').
	[flag] whileTrue: 
			[formato := Prompter prompt: 'Ingrese una opción (1: 2D, 2: 3D, 3: 4D)'.
			(formato isAllDigits and: [formato asNumber between: 1 and: 3])
				ifTrue: [flag := false]
				ifFalse: 
					[MessageBox warning: 'El valor ingresado es incorrecto'.
					sigue := MessageBox confirm: '¿Desea volver a intentar?'.
					sigue ifFalse: [flag := false]]].

	"verificar que elgio una sala correctamente (si sigue es falso no eligio sala)
	si eligio una sala entonces pedir el resto de los datos"
	sigue ifTrue: [titPelicula := Prompter prompt: 'Ingrese el titulo de la pelicula'].
	flag := true.
	[flag and: [sigue]] whileTrue: 
			[durPelicula := Prompter prompt: 'Ingrese la duracion en minutos (max: 300min)'.
			(durPelicula isAllDigits and: [durPelicula asNumber between: 1 and: 300])
				ifTrue: [flag := false]
				ifFalse: 
					[MessageBox warning: 'El número ingresado es incorrecto'.
					sigue := MessageBox confirm: '¿Desea volver a intentar?'.
					sigue ifFalse: [flag := false]]].
	flag := true.
	[flag and: [sigue]] whileTrue: 
			[precio := Prompter prompt: 'Ingrese el precio.'.
			precio asDecimalOrFloat > 0
				ifTrue: [flag := false]
				ifFalse: 
					[MessageBox warning: 'El número ingresado es incorrecto'.
					sigue := MessageBox confirm: '¿Desea volver a intentar?'.
					sigue ifFalse: [flag := false]]].


	"Comprueba que esa pelicula no exista antes de agregarla"
	sigue
		ifTrue: 
			[(self seleccionarPelicula: titPelicula formato: (formatosDisp at: formato asNumber))
				ifNotNil: 
					["si la pelicula existe"
					MessageBox warning: 'La pelicula ingresada ya existe.'.
					sigue := MessageBox confirm: '¿Desea volver a intentar?'.
					sigue ifFalse: [flag := false]]
				ifNil: 
					["crea la pelicula"
					unaPelicula := Pelicula
								crearPeliculaTitulo: titPelicula
								duracion: durPelicula asNumber
								formato: (formatosDisp at: formato asNumber)
								precio: precio asDecimalOrFloat.

					"muestra los datos para confirmar"
					(MessageBox
						confirm: (String streamContents: 
									[:s |
									s
										nextPutAll: '¿La pelicula que desea agregar es:';
										nextPut: Character lf.
									s
										nextPutAll: 'Titulo: ';
										nextPutAll: unaPelicula verTitulo;
										nextPut: Character lf.
									s
										nextPutAll: 'Duracion: ';
										nextPutAll: durPelicula;
										nextPutAll: ' minutos.';
										nextPut: Character lf.
									s
										nextPutAll: 'Formato: ';
										nextPutAll: unaPelicula verFormato;
										nextPut: Character lf.
									s
										nextPutAll: 'Precio: $';
										nextPutAll: precio;
										nextPutAll: '?']))
							ifTrue: 
								[self agregarPelicula: unaPelicula.
								MessageBox warning: 'Pelicula agregada exitosamente'.
								sigue := MessageBox confirm: '¿Desea agregar otra pelicula?'.
								sigue ifTrue: [self nuevaPelicula]]
							ifFalse: 
								[sigue := MessageBox confirm: '¿Desea agregar otra pelicula?'.
								sigue ifTrue: [self nuevaPelicula]]]]!

seleccionarPelicula: titulo formato: formato
	^cartelera at: (titulo , formato) ifAbsent: [^nil]!

seleccionarSala: nombreSala

	
	^salas detect:[:sal|sal verNombreSala = nombreSala] ifNone:[^nil].!

verClientes
	^clientes!

verHorariosSala: unaSala

	"implementar"!

verNombre

	^nombre.!

verPeliculas
	^cartelera!

verSalas

	^salas.! !
!Cine categoriesFor: #agregarPelicula:!private! !
!Cine categoriesFor: #agregarSala:!private! !
!Cine categoriesFor: #crearSala!public! !
!Cine categoriesFor: #eliminarPelicula:!private! !
!Cine categoriesFor: #iniCine:!private! !
!Cine categoriesFor: #nuevaPelicula!public! !
!Cine categoriesFor: #seleccionarPelicula:formato:!public! !
!Cine categoriesFor: #seleccionarSala:!public! !
!Cine categoriesFor: #verClientes!public! !
!Cine categoriesFor: #verHorariosSala:!public! !
!Cine categoriesFor: #verNombre!public! !
!Cine categoriesFor: #verPeliculas!public! !
!Cine categoriesFor: #verSalas!public! !

!Cine class methodsFor!

crearCine: unNombre

	^self new iniCine: unNombre.! !
!Cine class categoriesFor: #crearCine:!public! !

CuentaCliente guid: (GUID fromString: '{7514C972-5771-4862-BC3B-B3E9EAA40340}')!
CuentaCliente comment: ''!
!CuentaCliente categoriesForClass!TPI-Cine! !
!CuentaCliente methodsFor!

agregarPuntos:cantPuntos
	thisPuntos:=(thisPuntos+cantPuntos).!

agregarReserva:unaReserva

	thisReservas add:unaReserva.!

eliminarReserva: idReserva
	"NO FUNCIONABA
	1 to: thisReservas size
		do: 
			[:i |
			(thisReservas at: i) verIdReserva = idReserva
				ifTrue: 
					[reservaRemove := thisReservas at: i.
					thisReservas remove: reservaRemove].
			flag := true].
	flag ifTrue: [^nil]."

	| reservaRemove |
	reservaRemove := thisReservas detect: [:res | res verIdReserva = idReserva] ifNone: [^nil].
	reservaRemove ifNotNil: [thisReservas remove: reservaRemove]!

iniCuentaCliente: nombreUsuario contrasenia: contrasenia
	thisNombreUsuario := nombreUsuario.
	thisContrasenia := contrasenia.
	thisPuntos:=0.
	thisReservas:=OrderedCollection new.!

modiContrasenia: contrasenia
	thisContrasenia := contrasenia.!

quitarPuntos:cantPuntos
	(thisPuntos<cantPuntos) ifTrue: [^nil] 
					     ifFalse: [thisPuntos:=thisPuntos-cantPuntos].!

seleccionarReserva: idReserva
	| flag |
	flag:=false.
	(1 to: thisReservas size
		do: [:i | (thisReservas at: i) verIdReserva = idReserva ifTrue: [(flag:=true).
													      ^thisReservas at: i]]).
	(flag=false) ifTrue: [^nil].

	"^thisReservas detect:[:res| res verIdReserva = idReserva] ifNone:[^nil]."!

verContrasenia
	^thisContrasenia.!

verNombreUsuario
	^thisNombreUsuario.!

verPuntos
	^thisPuntos.!

verReservas
	^thisReservas.! !
!CuentaCliente categoriesFor: #agregarPuntos:!public! !
!CuentaCliente categoriesFor: #agregarReserva:!public! !
!CuentaCliente categoriesFor: #eliminarReserva:!public! !
!CuentaCliente categoriesFor: #iniCuentaCliente:contrasenia:!private! !
!CuentaCliente categoriesFor: #modiContrasenia:!public! !
!CuentaCliente categoriesFor: #quitarPuntos:!public! !
!CuentaCliente categoriesFor: #seleccionarReserva:!public! !
!CuentaCliente categoriesFor: #verContrasenia!public! !
!CuentaCliente categoriesFor: #verNombreUsuario!public! !
!CuentaCliente categoriesFor: #verPuntos!public! !
!CuentaCliente categoriesFor: #verReservas!public! !

!CuentaCliente class methodsFor!

crearCuentaCliente: nombreUsuario contrasenia: contrasenia
	^self new iniCuentaCliente: nombreUsuario contrasenia: contrasenia.! !
!CuentaCliente class categoriesFor: #crearCuentaCliente:contrasenia:!public! !

FuncionPelicula guid: (GUID fromString: '{4204F44D-F7E9-4846-8644-03C0EF2C3B26}')!
FuncionPelicula comment: ''!
!FuncionPelicula categoriesForClass!TPI-Cine! !
!FuncionPelicula methodsFor!

iniFuncionPeliculaNombreSala: nomSala nombrePelicula: nomPelicula horario: unahora idioma: unIdioma formato: formato

	idFuncionPelicula := ((nomPelicula copyFrom: 1 to: 3) , unahora , '-' , (unIdioma copyFrom: 1 to: 3) , formato) asUppercase.
	nombreSala := nomSala.
	nombrePelicula := nomPelicula.
	horario := unahora.
	idioma := unIdioma.!

verHorario

	^horario.!

verIdFuncionPelicula

	^idFuncionPelicula.!

verIdioma

	^idioma.!

verNombrePelicula

	^nombrePelicula.!

verNombreSalaAsignada

	^nombreSala.! !
!FuncionPelicula categoriesFor: #iniFuncionPeliculaNombreSala:nombrePelicula:horario:idioma:formato:!private! !
!FuncionPelicula categoriesFor: #verHorario!public! !
!FuncionPelicula categoriesFor: #verIdFuncionPelicula!public! !
!FuncionPelicula categoriesFor: #verIdioma!public! !
!FuncionPelicula categoriesFor: #verNombrePelicula!public! !
!FuncionPelicula categoriesFor: #verNombreSalaAsignada!public! !

!FuncionPelicula class methodsFor!

crearFuncionPeliculaNombreSala: nombreSala nombrePelicula: nombrePelicula horario: horario idioma: idioma formato: formato

	^self new iniFuncionPeliculaNombreSala: nombreSala nombrePelicula: nombrePelicula horario: horario idioma: idioma formato: formato.! !
!FuncionPelicula class categoriesFor: #crearFuncionPeliculaNombreSala:nombrePelicula:horario:idioma:formato:!public! !

Horario guid: (GUID fromString: '{CA24DA3B-D18F-4325-883B-240969AAF974}')!
Horario comment: ''!
!Horario categoriesForClass!TPI-Cine! !
!Horario methodsFor!

comprobarSupHoraria: unHorario

	^((unHorario verHoraInicio between: inicio and: fin) or: [unHorario verHoraFin between: inicio and: fin]).
	!

iniHorarioInicio: horaInicio horaFin: horaFin nombrePelicula: nombrePelicula

	inicio := horaInicio.
	fin := horaFin.
	peliculaEnEmision := nombrePelicula.!

verHoraFin

	^fin.!

verHoraInicio
	
	^inicio.!

verPeliculaEnEmision

	^peliculaEnEmision.! !
!Horario categoriesFor: #comprobarSupHoraria:!public! !
!Horario categoriesFor: #iniHorarioInicio:horaFin:nombrePelicula:!private! !
!Horario categoriesFor: #verHoraFin!public! !
!Horario categoriesFor: #verHoraInicio!public! !
!Horario categoriesFor: #verPeliculaEnEmision!public! !

!Horario class methodsFor!

crearHorarioInicio: horaInicio horaFin: horaFin nombrePelicula: nombrePelicula

	^self new iniHorarioInicio: horaInicio horaFin: horaFin nombrePelicula: nombrePelicula.! !
!Horario class categoriesFor: #crearHorarioInicio:horaFin:nombrePelicula:!public! !

Pelicula guid: (GUID fromString: '{1053C213-B7AE-43D6-83F4-329D428DEFF7}')!
Pelicula comment: ''!
!Pelicula categoriesForClass!TPI-Cine! !
!Pelicula methodsFor!

agregarFuncionPelicula: unaFuncionPelicula
	
	(funciones includesKey: unaFuncionPelicula verIdFuncionPelicula)
		ifFalse:[funciones at: (unaFuncionPelicula verIdFuncionPelicula) put: unaFuncionPelicula.]
		ifTrue: [^nil].!

eliminarFuncionPelicula: idFuncionPelicula

	funciones removeKey: idFuncionPelicula ifAbsent: [^nil]!

iniPeliculaTitulo: unTitulo duracion: minutos formato: unFormato precio: unPrecio

	titulo := unTitulo.
	duracion := minutos.
	formato := unFormato.
	funciones := Dictionary new.
	precio := unPrecio.!

modiPrecio: unPrecio

	precio := unPrecio.!

seleccionarFuncionPelicula: idFuncionPelicula

	^funciones at: idFuncionPelicula ifAbsent: [^nil] .!

verDuracion
	^duracion!

verFormato

	^formato.!

verFunciones

	^funciones.!

verPrecio

	^precio.!

verTitulo

	^titulo.! !
!Pelicula categoriesFor: #agregarFuncionPelicula:!public! !
!Pelicula categoriesFor: #eliminarFuncionPelicula:!public! !
!Pelicula categoriesFor: #iniPeliculaTitulo:duracion:formato:precio:!private! !
!Pelicula categoriesFor: #modiPrecio:!public! !
!Pelicula categoriesFor: #seleccionarFuncionPelicula:!public! !
!Pelicula categoriesFor: #verDuracion!public! !
!Pelicula categoriesFor: #verFormato!public! !
!Pelicula categoriesFor: #verFunciones!public! !
!Pelicula categoriesFor: #verPrecio!public! !
!Pelicula categoriesFor: #verTitulo!public! !

!Pelicula class methodsFor!

crearPeliculaTitulo: unTitulo duracion: minutos formato: unFormato precio: unPrecio

	^self new iniPeliculaTitulo: unTitulo duracion: minutos formato: unFormato precio: unPrecio.! !
!Pelicula class categoriesFor: #crearPeliculaTitulo:duracion:formato:precio:!public! !

Persona guid: (GUID fromString: '{67D6CC25-2B73-4D58-A3EE-D97573BB68AF}')!
Persona comment: ''!
!Persona categoriesForClass!TPI-Cine! !
!Persona methodsFor!

iniPersonaNombre: unNombre apellido: unApellido dni: unDni telefono: unTelefono direccion: unaDireccion email: unEmail

	nombre:=unNombre.
	apellido:=unApellido.
	dni:=unDni.
	telefono:=unTelefono.
	direccion:=unaDireccion.
	email:=unEmail.!

modDireccion: unaDireccion
    direccion := unaDireccion.!

modEmail: unEmail
	email := unEmail.!

modTelefono: unTelefono
    telefono := unTelefono.!

verApellido
	^apellido.!

verDireccion
	^direccion.!

verDni
	^dni.!

verEmail
	^email.!

verNombre
    ^nombre.
!

verTelefono
	^telefono.! !
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

iniciarReserva: idReserva idFuncionPelicula: idFuncionPelicula asientos: asientos costo: costo
	thisIdReserva := idReserva.
	thisIdFuncionPelicula := idFuncionPelicula.
	thisAsientos := asientos.
	thisCosto := costo.
	"use el this pq me daba error si tenian el mismo nombre"!

verAsientos
	^thisAsientos.!

verCosto
	^thisCosto.!

verIdFuncionPelicula
	^thisIdFuncionPelicula.!

verIdReserva
	^thisIdReserva.! !
!Reserva categoriesFor: #iniciarReserva:idFuncionPelicula:asientos:costo:!private! !
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

verNombreSala

	"Implementar"! !
!Sala categoriesFor: #verNombreSala!public! !

Cliente guid: (GUID fromString: '{AA0E5212-BA8B-4021-A05F-D81F3420DB9D}')!
Cliente comment: ''!
!Cliente categoriesForClass!TPI-Cine! !
!Cliente methodsFor!

iniCliente:id nombre:unNombre apellido:unApellido dni:unDni telefono:unTelefono direccion:unaDireccion email:unEmail nomUsuario:nomUsuario contrasenia:contrasenia
	
	super iniPersonaNombre: unNombre apellido: unApellido dni: unDni telefono: unTelefono direccion: unaDireccion email: unEmail.

	thisIdCliente:=id.
	thisCuenta:=CuentaCliente crearCuentaCliente: nomUsuario contrasenia: contrasenia.
	!

verCuenta
	^thisCuenta.!

verIdCliente
	^thisIdCliente.! !
!Cliente categoriesFor: #iniCliente:nombre:apellido:dni:telefono:direccion:email:nomUsuario:contrasenia:!private! !
!Cliente categoriesFor: #verCuenta!public! !
!Cliente categoriesFor: #verIdCliente!public! !

!Cliente class methodsFor!

crearClienteId: id nombre: nombre apellido: apellido dni: dni telefono: telefono direccion: direccion email: email noUduario: nomUsuario contrasenia: contrasenia

	^self new iniCliente:id nombre:nombre apellido:apellido dni:dni telefono:telefono direccion:direccion email:email nomUsuario:nomUsuario contrasenia:contrasenia.! !
!Cliente class categoriesFor: #crearClienteId:nombre:apellido:dni:telefono:direccion:email:noUduario:contrasenia:!public! !

"Binary Globals"!

