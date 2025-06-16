cm:= CineModel new.
cm crearCine: 'Hoyts' cuit: '20-12345678-00' direccion: 'Av. Calchaqui 1234 '.
cm preset.
c:= CineShell show.
c model: cm.

