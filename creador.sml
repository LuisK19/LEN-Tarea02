(* creador.sml
   Autor: Luis
   Descripcion: Administracion del catalogo de matriculas.
   Funciones principales: agregarRegistro, limpiarCatalogo, main
*)

type matricula = string * string * string * int * real

val archivoPorDefecto = "matricula.csv"

fun formatoCSV (carnet, nombre, curso, creditos, costo) =
    carnet ^ "," ^ nombre ^ "," ^ curso ^ "," ^ Int.toString creditos ^ "," ^ Real.toString costo

fun archivoTerminaEnNewline archivo =
    let val ins = TextIO.openIn archivo
        val contenido = TextIO.inputAll ins
        val _ = TextIO.closeIn ins
    in if size contenido = 0 then true
       else String.sub(contenido, size contenido - 1) = #"\n"
    end

fun agregarRegistro archivo (carnet, nombre, curso, creditos, costo) =
    let val necesitaNewline = not (archivoTerminaEnNewline archivo)
        val out = TextIO.openAppend archivo
        val linea = formatoCSV (carnet, nombre, curso, creditos, costo)
    in if necesitaNewline then TextIO.output (out, "\n" ^ linea ^ "\n")
       else TextIO.output (out, linea ^ "\n");
       TextIO.closeOut out
    end

fun limpiarCatalogo archivo =
    let val out = TextIO.openOut archivo
        val cabecera = "carnet_estudiante,nombre,curso,creditos,costo_credito\n"
    in TextIO.output (out, cabecera);
       TextIO.closeOut out
    end

fun leerEntero prompt =
    (print prompt; case TextIO.inputLine TextIO.stdIn of
         SOME line => (case Int.fromString (String.substring (line, 0, size line - 1)) of
                           SOME i => i
                         | NONE => (print "Error: debe ingresar un numero entero.\n"; leerEntero prompt))
       | NONE => (print "Error de entrada.\n"; leerEntero prompt))

fun leerReal prompt =
    (print prompt; case TextIO.inputLine TextIO.stdIn of
         SOME line => (case Real.fromString (String.substring (line, 0, size line - 1)) of
                           SOME r => r
                         | NONE => (print "Error: debe ingresar un numero real.\n"; leerReal prompt))
       | NONE => (print "Error de entrada.\n"; leerReal prompt))

fun leerString prompt =
    (print prompt; case TextIO.inputLine TextIO.stdIn of
         SOME line => let val s = String.substring (line, 0, size line - 1)
                      in if s = "" then (print "No puede estar vacio.\n"; leerString prompt)
                         else s
                      end
       | NONE => (print "Error de entrada.\n"; leerString prompt))

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

fun main () =
    (print "=== PROGRAMA CREADOR DE MATRICULAS ===\n";
     print ("Usando archivo por defecto: " ^ archivoPorDefecto ^ "\n");
     menuCreador archivoPorDefecto)
