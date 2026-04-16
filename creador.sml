(* ========================================================================
   creador.sml
   Autor: Luis [Nombre completo del estudiante]
   Carnet: [Numero de carnet]
   Fecha: Abril 2026
   Descripcion:
       Programa que administra un catalogo de matriculas universitarias.
       Permite agregar nuevos registros a un archivo CSV y limpiar el catalogo.
       Los datos se almacenan en el archivo "matricula.csv" con la estructura:
       carnet_estudiante,nombre,curso,creditos,costo_credito
   ========================================================================= *)

(* Definicion del tipo de datos que representa una matricula.
   Cada matricula contiene:
   - carnet   : string   (identificador unico del estudiante)
   - nombre   : string   (nombre completo)
   - curso    : string   (codigo del curso)
   - creditos : int      (numero de creditos del curso)
   - costo    : real     (costo por credito) *)
type matricula = string * string * string * int * real

(* Archivo por defecto donde se almacenan las matriculas *)
val archivoPorDefecto = "matricula.csv"

(* ------------------------------------------------------------------------
   formatoCSV : matricula -> string
   Convierte una tupla de tipo matricula en una linea con formato CSV,
   separando cada campo por comas.
   Ejemplo: ("200440117","Juan Perez","IC4301",4,15000.0) ->
            "200440117,Juan Perez,IC4301,4,15000.0"
   ------------------------------------------------------------------------ *)
fun formatoCSV (carnet, nombre, curso, creditos, costo) =
    carnet ^ "," ^ nombre ^ "," ^ curso ^ "," ^ Int.toString creditos ^ "," ^ Real.toString costo

(* ------------------------------------------------------------------------
   archivoTerminaEnNewline : string -> bool
   Verifica si el archivo dado termina con un salto de linea ('\n').
   Util para saber si al agregar un nuevo registro debemos insertar un
   salto de linea antes de escribir la nueva linea.
   Parametro: archivo - ruta del archivo a verificar.
   Retorna: true si el archivo termina con '\n' o esta vacio, false en otro caso.
   ------------------------------------------------------------------------ *)
fun archivoTerminaEnNewline archivo =
    let val ins = TextIO.openIn archivo
        val contenido = TextIO.inputAll ins
        val _ = TextIO.closeIn ins
    in if size contenido = 0 then true
       else String.sub(contenido, size contenido - 1) = #"\n"
    end

(* ------------------------------------------------------------------------
   agregarRegistro : string -> matricula -> unit
   Agrega un nuevo registro de matricula al final del archivo CSV.
   Si el archivo no termina con salto de linea, lo inserta antes del nuevo registro.
   Parametros:
     archivo : ruta del archivo CSV.
     mat     : tupla con los datos de la matricula.
   Efecto lateral: escribe la linea correspondiente al final del archivo.
   ------------------------------------------------------------------------ *)
fun agregarRegistro archivo (carnet, nombre, curso, creditos, costo) =
    let val necesitaNewline = not (archivoTerminaEnNewline archivo)
        val out = TextIO.openAppend archivo
        val linea = formatoCSV (carnet, nombre, curso, creditos, costo)
    in if necesitaNewline then TextIO.output (out, "\n" ^ linea ^ "\n")
       else TextIO.output (out, linea ^ "\n");
       TextIO.closeOut out
    end

(* ------------------------------------------------------------------------
   limpiarCatalogo : string -> unit
   Limpia completamente el archivo de matriculas y escribe unicamente la
   linea de cabecera: "carnet_estudiante,nombre,curso,creditos,costo_credito"
   Parametro: archivo - ruta del archivo CSV.
   Efecto lateral: sobrescribe el archivo con la cabecera.
   ------------------------------------------------------------------------ *)
fun limpiarCatalogo archivo =
    let val out = TextIO.openOut archivo
        val cabecera = "carnet_estudiante,nombre,curso,creditos,costo_credito\n"
    in TextIO.output (out, cabecera);
       TextIO.closeOut out
    end

(* ------------------------------------------------------------------------
   Funciones de entrada de datos con validacion.
   Cada funcion solicita un valor al usuario y lo convierte al tipo adecuado,
   repitiendo la solicitud en caso de error.
   ------------------------------------------------------------------------ *)

(* leerEntero : string -> int
   Solicita un numero entero, validando la entrada. *)
fun leerEntero prompt =
    (print prompt; case TextIO.inputLine TextIO.stdIn of
         SOME line => (case Int.fromString (String.substring (line, 0, size line - 1)) of
                           SOME i => i
                         | NONE => (print "Error: debe ingresar un numero entero.\n"; leerEntero prompt))
       | NONE => (print "Error de entrada.\n"; leerEntero prompt))

(* leerReal : string -> real
   Solicita un numero real, validando la entrada. *)
fun leerReal prompt =
    (print prompt; case TextIO.inputLine TextIO.stdIn of
         SOME line => (case Real.fromString (String.substring (line, 0, size line - 1)) of
                           SOME r => r
                         | NONE => (print "Error: debe ingresar un numero real.\n"; leerReal prompt))
       | NONE => (print "Error de entrada.\n"; leerReal prompt))

(* leerString : string -> string
   Solicita una cadena no vacia. *)
fun leerString prompt =
    (print prompt; case TextIO.inputLine TextIO.stdIn of
         SOME line => let val s = String.substring (line, 0, size line - 1)
                      in if s = "" then (print "No puede estar vacio.\n"; leerString prompt)
                         else s
                      end
       | NONE => (print "Error de entrada.\n"; leerString prompt))

(* ------------------------------------------------------------------------
   menuCreador : string -> unit
   Presenta un menu interactivo para administrar el catalogo.
   Opciones:
     1. Agregar nueva matricula: solicita todos los campos y los guarda.
     2. Limpiar todo el catalogo: borra todos los registros (solo cabecera).
     3. Salir.
   Parametro: archivo - ruta del archivo CSV a manipular.
   ------------------------------------------------------------------------ *)
fun menuCreador archivo =
    let fun loop () =
            (print "\n=== ADMINISTRACION DE MATRICULAS ===\n";
             print "1. Agregar nueva matricula\n";
             print "2. Limpiar todo el catalogo\n";
             print "3. Salir\n";
             print "Opcion: ";
             case TextIO.inputLine TextIO.stdIn of
                 SOME line => (case Int.fromString (String.substring (line, 0, size line - 1)) of
                                   SOME 1 => (let val carnet   = leerString "Carnet: "
                                                  val nombre   = leerString "Nombre: "
                                                  val curso    = leerString "Codigo del curso: "
                                                  val creditos = leerEntero "Creditos: "
                                                  val costo    = leerReal   "Costo por credito: "
                                              in agregarRegistro archivo (carnet, nombre, curso, creditos, costo);
                                                 print "Matricula agregada correctamente.\n";
                                                 loop()
                                              end)
                                 | SOME 2 => (limpiarCatalogo archivo;
                                              print "Catalogo limpiado (solo queda la cabecera).\n";
                                              loop())
                                 | SOME 3 => print "Saliendo del Creador...\n"
                                 | _ => (print "Opcion no valida.\n"; loop()))
               | NONE => (print "Error de lectura.\n"; loop()))
    in loop() end

(* ------------------------------------------------------------------------
   main : unit -> unit
   Funcion principal del programa Creador.
   Muestra un mensaje de bienvenida e inicia el menu interactivo utilizando
   el archivo por defecto "matricula.csv".
   ------------------------------------------------------------------------ *)
fun main () =
    (print "=== PROGRAMA CREADOR DE MATRICULAS ===\n";
     print ("Usando archivo por defecto: " ^ archivoPorDefecto ^ "\n");
     menuCreador archivoPorDefecto)
