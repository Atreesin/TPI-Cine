| peli fp1 fp2 fp3 fp4 fp5|

peli := Pelicula crearPeliculaTitulo: 'Spiderman 3' duracion: 120 formato: '3D' precio: 500.5.

fp1 := FuncionPelicula crearFuncionPeliculaNombreSala: 'S1' nombrePelicula: (peli verTitulo) horario: 'MI2200' idioma: 'español' formato: (peli verFormato).

fp2 := FuncionPelicula crearFuncionPeliculaNombreSala: 'S1' nombrePelicula: (peli verTitulo) horario: 'MI2300' idioma: 'español' formato: (peli verFormato).

fp3 := FuncionPelicula crearFuncionPeliculaNombreSala: 'S1' nombrePelicula: (peli verTitulo) horario: 'MI2400' idioma: 'subtitulada' formato: (peli verFormato).

fp4 := FuncionPelicula crearFuncionPeliculaNombreSala: 'S1' nombrePelicula: (peli verTitulo) horario: 'JU2200' idioma: 'español' formato: (peli verFormato).

fp5 := FuncionPelicula crearFuncionPeliculaNombreSala: 'S1' nombrePelicula: (peli verTitulo) horario: 'VI2000' idioma: 'español' formato: (peli verFormato).

peli agregarFuncionPelicula: fp1.
peli agregarFuncionPelicula: fp2.
peli agregarFuncionPelicula: fp3.
peli agregarFuncionPelicula: fp4.
peli agregarFuncionPelicula: fp5.

(peli seleccionarFuncionPelicula: 'SPIMI2300-ESP3D') verIdioma inspect.
Transcript show.
Transcript clear.
Transcript nextPutAll: (peli verFunciones at: 'SPIMI2300-ESP3D' )verIdFuncionPelicula.
Transcript cr.