#Script para la creación de la base de datos local, es necesario correrla la primera vez que se
#quiera usar la BD.

#Script para crear la base de datos con las columnas id_movimiento, fecha_movimiento, descripcion_movimiento, monto
library(DBI) #Esta es la libreria database interface, la cual contiene los comandos que podemos usar para manipular una base de datos desde lña interfaz de R
library(RSQLite) #carga la libreria
coneccion <- dbConnect(SQLite(), dbname="finanzas.sqlite") #realiza la coneccion con el objeto coneccion
dbWriteTable(coneccion, "cuentas", data.frame(id_movimiento = NA, fecha_movimiento = NA, descripcion_movimiento = NA, monto = NA)) #crea la tabla cuentas, donde se guardara el df que el usuario subio
dbDisconnect(coneccion) #cierra la coneccion con la base de datos. Si quieres hacer algun cambio en la base de datos siempre tienes que abrir la coneccion y despues cerrarla, para que solo cargue comandos de base de datos por un tiempo y no afecte las funcionalidades de la app

