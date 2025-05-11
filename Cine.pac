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
	yourself).

package!

"Class Definitions"!

Object subclass: #Asiento
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Cine
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #CuentaCliente
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #FuncionPelicula
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Horario
	instanceVariableNames: 'inicio fin peliculaEnEmision'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Pelicula
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Persona
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Reserva
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Sala
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Persona subclass: #Cliente
	instanceVariableNames: ''
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
Cine guid: (GUID fromString: '{66982B7D-A579-4656-BFD8-F742AB78A63D}')!
Cine comment: ''!
!Cine categoriesForClass!TPI-Cine! !
CuentaCliente guid: (GUID fromString: '{7514C972-5771-4862-BC3B-B3E9EAA40340}')!
CuentaCliente comment: ''!
!CuentaCliente categoriesForClass!TPI-Cine! !
FuncionPelicula guid: (GUID fromString: '{4204F44D-F7E9-4846-8644-03C0EF2C3B26}')!
FuncionPelicula comment: ''!
!FuncionPelicula categoriesForClass!TPI-Cine! !
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
!Horario categoriesFor: #iniHorarioInicio:horaFin:nombrePelicula:!public! !
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
Persona guid: (GUID fromString: '{67D6CC25-2B73-4D58-A3EE-D97573BB68AF}')!
Persona comment: ''!
!Persona categoriesForClass!TPI-Cine! !
Reserva guid: (GUID fromString: '{550FEF76-40EE-4125-8FDF-55792D9F5664}')!
Reserva comment: ''!
!Reserva categoriesForClass!TPI-Cine! !
Sala guid: (GUID fromString: '{6EFA1A17-5CE3-4E8A-9BA9-CADE78918F62}')!
Sala comment: ''!
!Sala categoriesForClass!TPI-Cine! !
Cliente guid: (GUID fromString: '{AA0E5212-BA8B-4021-A05F-D81F3420DB9D}')!
Cliente comment: ''!
!Cliente categoriesForClass!TPI-Cine! !
"Binary Globals"!

