(* analizador.sml
   Autor: Luis
   Descripcion: Analisis de matriculas cargadas desde un archivo CSV.
   Funciones principales: leerCSV, rankingCursos, cursosMas5Estudiantes,
                          buscarPorEstudiante, cursosPorCreditos, resumenGeneral, main
*)

type matricula = string * string * string * int * real

fun leerCSV archivo =
    let val ins = TextIO.openIn archivo
        fun leerLineas () =
            case TextIO.inputLine ins of
                NONE => []
              | SOME line =>
                let val limpia = if size line > 0 andalso String.sub(line, size line - 1) = #"\n"
                                 then String.substring(line, 0, size line - 1)
                                 else line
                in if String.isPrefix "carnet_estudiante" limpia then leerLineas()
                   else let val campos = String.tokens (fn c => c = #",") limpia
                         in case campos of
                                [carnet, nombre, curso, creditosStr, costoStr] =>
                                (case (Int.fromString creditosStr, Real.fromString costoStr) of
                                     (SOME cred, SOME costo) => (carnet, nombre, curso, cred, costo) :: leerLineas()
                                   | _ => (print ("Error al parsear: " ^ limpia ^ "\n"); leerLineas()))
                              | _ => (print ("Formato incorrecto: " ^ limpia ^ "\n"); leerLineas())
                         end
                end
        val datos = leerLineas ()
    in TextIO.closeIn ins; datos end

fun leerEntero prompt =
    (print prompt; case TextIO.inputLine TextIO.stdIn of
         SOME line => (case Int.fromString (String.substring (line, 0, size line - 1)) of
                           SOME i => i
                         | NONE => (print "Debe ser entero.\n"; leerEntero prompt))
       | NONE => (print "Error.\n"; leerEntero prompt))

fun leerReal prompt =
    (print prompt; case TextIO.inputLine TextIO.stdIn of
         SOME line => (case Real.fromString (String.substring (line, 0, size line - 1)) of
                           SOME r => r
                         | NONE => (print "Debe ser real.\n"; leerReal prompt))
       | NONE => (print "Error.\n"; leerReal prompt))

fun leerString prompt =
    (print prompt; case TextIO.inputLine TextIO.stdIn of
         SOME line => String.substring (line, 0, size line - 1)
       | NONE => "")

fun rankingCursos datos =
    let fun actualizar (curso, monto) [] = [(curso, monto)]
          | actualizar (curso, monto) ((c,t)::rest) =
            if c = curso then (c, t + monto) :: rest
            else (c,t) :: actualizar (curso, monto) rest
        val montosCurso = List.foldl (fn ((_,_,curso,cred,costo), acc) =>
                                         actualizar (curso, real(cred) * costo) acc) [] datos
        fun comparar ((_,t1),(_,t2)) = t1 < t2
        val ordenados = ListMergeSort.sort comparar montosCurso
    in print "\n--- Ranking de cursos por ingreso total ---\n";
       print "Ingrese monto minimo: ";
       let val minVal = leerReal ""
           val maxVal = leerReal "Ingrese monto maximo: "
           val filtrados = List.filter (fn (_,t) => t >= minVal andalso t <= maxVal) ordenados
       in if null filtrados then print "No hay cursos en ese rango.\n"
          else (List.app (fn (c,t) => print (c ^ " - " ^ Real.toString t ^ "\n")) filtrados;
                print "\n")
       end
    end

fun cursosMas5Estudiantes datos =
    let fun agregarEstudiante (curso, carnet) [] = [(curso, [carnet])]
          | agregarEstudiante (curso, carnet) ((c,estud)::rest) =
            if c = curso then
                if List.exists (fn e => e = carnet) estud then (c,estud)::rest
                else (c, carnet::estud) :: rest
            else (c,estud) :: agregarEstudiante (curso, carnet) rest
        val porCurso = List.foldl (fn ((carnet,_,curso,_,_), acc) =>
                                      agregarEstudiante (curso, carnet) acc) [] datos
        val filtrados = List.filter (fn (_,est) => length est > 5) porCurso
    in print "\n--- Cursos con mas de 5 estudiantes ---\n";
       if null filtrados then print "Ningun curso supera los 5 estudiantes.\n"
       else List.app (fn (c,est) => print (c ^ " -> " ^ Int.toString (length est) ^ " estudiantes\n")) filtrados;
       print "\n"
    end

fun buscarPorEstudiante datos =
    let val patron = leerString "\nIngrese carnet exacto o parte del nombre: "
        fun coincide (carnet, nombre, _, _, _) =
            carnet = patron orelse String.isSubstring patron nombre
        val resultados = List.filter coincide datos
    in if null resultados then print "No se encontraron matriculas.\n"
       else (print "\nResultados:\n";
             List.app (fn (carnet, nombre, curso, cred, costo) =>
                          print (carnet ^ " | " ^ nombre ^ " | " ^ curso ^ " | " ^
                                 Int.toString cred ^ " creditos | " ^ Real.toString costo ^ "/credito\n")) resultados;
             print "\n")
    end

fun cursosPorCreditos datos =
    let val creditosBusc = leerEntero "\nIngrese numero de creditos: "
        fun coincideCred (_,_,_,cred,_) = cred = creditosBusc
        val cursos = List.map (fn (_,_,curso,_,_) => curso) (List.filter coincideCred datos)
        fun unicos [] = []
          | unicos (x::xs) = if List.exists (fn y => y = x) xs then unicos xs else x :: unicos xs
        val distintos = unicos cursos
    in print ("\n--- Cursos con " ^ Int.toString creditosBusc ^ " creditos ---\n");
       if null distintos then print "Ninguno.\n"
       else List.app (fn c => print (c ^ "\n")) distintos;
       print "\n"
    end

fun resumenGeneral datos =
    let fun agregarEstCurso (curso, carnet) [] = [(curso, [carnet])]
          | agregarEstCurso (curso, carnet) ((c,est)::rest) =
            if c = curso then
                if List.exists (fn e => e = carnet) est then (c,est)::rest
                else (c, carnet::est)::rest
            else (c,est)::agregarEstCurso (curso, carnet) rest
        val estudiantesPorCurso = List.foldl (fn ((carnet,_,curso,_,_), acc) =>
                                                 agregarEstCurso (curso, carnet) acc) [] datos
        fun sumarCreditosEst (carnet, cred) [] = [(carnet, cred)]
          | sumarCreditosEst (carnet, cred) ((c,sum)::rest) =
            if c = carnet then (c, sum+cred)::rest else (c,sum)::sumarCreditosEst (carnet, cred) rest
        val creditosPorEst = List.foldl (fn ((carnet,_,_,cred,_), acc) =>
                                            sumarCreditosEst (carnet, cred) acc) [] datos
        val creditosSolo = List.map #2 creditosPorEst
        val mayorCreditos = List.foldl (fn (c1, c2) => if c1 > c2 then c1 else c2) 0 creditosSolo
        val menorCreditos = List.foldl (fn (c1, c2) => if c1 < c2 then c1 else c2) (valOf Int.maxInt) creditosSolo
        fun buscarEstPorCreditos creditos = List.find (fn (_,c) => c = creditos) creditosPorEst
        fun actualizarCursoMonto (curso, monto) [] = [(curso, monto)]
          | actualizarCursoMonto (curso, monto) ((c,t)::rest) =
            if c = curso then (c, t+monto)::rest else (c,t)::actualizarCursoMonto (curso, monto) rest
        val montosCurso = List.foldl (fn ((_,_,curso,cred,costo), acc) =>
                                         actualizarCursoMonto (curso, real(cred)*costo) acc) [] datos
        val (cursoMayorMonto, mayorMonto) =
            List.foldl (fn ((c,t), (cMax,tMax)) => if t > tMax then (c,t) else (cMax,tMax)) ("",0.0) montosCurso
        fun actualizarEstMonto (carnet, monto) [] = [(carnet, monto)]
          | actualizarEstMonto (carnet, monto) ((c,t)::rest) =
            if c = carnet then (c, t+monto)::rest else (c,t)::actualizarEstMonto (carnet, monto) rest
        val montosEst = List.foldl (fn ((carnet,_,_,cred,costo), acc) =>
                                       actualizarEstMonto (carnet, real(cred)*costo) acc) [] datos
        val (estMayorIngreso, mayorIngreso) =
            List.foldl (fn ((c,t), (cMax,tMax)) => if t > tMax then (c,t) else (cMax,tMax)) ("",0.0) montosEst
        val nombreMayorIngreso =
            case List.find (fn (carnet,_,_,_,_) => carnet = estMayorIngreso) datos of
                SOME (_,nombre,_,_,_) => nombre | NONE => ""
        fun mostrarEstudiante (carnet, cred) =
            case List.find (fn (c,_,_,_,_) => c = carnet) datos of
                SOME (_,nombre,_,_,_) => nombre ^ " (" ^ carnet ^ ")"
              | NONE => carnet
    in print "\n=== RESUMEN GENERAL ===\n";
       print "1. Cantidad de estudiantes por curso:\n";
       List.app (fn (c,est) => print ("   " ^ c ^ ": " ^ Int.toString (length est) ^ " estudiantes\n")) estudiantesPorCurso;
       case buscarEstPorCreditos mayorCreditos of
           SOME (carnet,cred) => print ("2. Estudiante con mayor creditos: " ^ mostrarEstudiante (carnet,cred) ^ " (" ^ Int.toString cred ^ " creditos)\n")
         | NONE => print "2. No hay datos.\n";
       case buscarEstPorCreditos menorCreditos of
           SOME (carnet,cred) => print ("3. Estudiante con menor creditos: " ^ mostrarEstudiante (carnet,cred) ^ " (" ^ Int.toString cred ^ " creditos)\n")
         | NONE => print "3. No hay datos.\n";
       print ("4. Curso con mayor ingreso total: " ^ cursoMayorMonto ^ " (" ^ Real.toString mayorMonto ^ ")\n");
       print ("5. Estudiante que genera mayor ingreso: " ^ nombreMayorIngreso ^ " (" ^ estMayorIngreso ^ ") con " ^ Real.toString mayorIngreso ^ "\n");
       print "\n"
    end

fun menuAnalisis datos =
    let fun loop () =
            (print "\n=== MENU DE ANALISIS ===\n";
             print "a) Ranking de cursos por ingreso (con filtro)\n";
             print "b) Cursos con mas de 5 estudiantes\n";
             print "c) Buscar matriculas por estudiante\n";
             print "d) Listar cursos por numero de creditos\n";
             print "e) Resumen general\n";
             print "s) Salir\n";
             print "Opcion: ";
             case TextIO.inputLine TextIO.stdIn of
                 SOME line => (case String.sub (String.map Char.toLower line, 0) of
                                   #"a" => (rankingCursos datos; loop())
                                 | #"b" => (cursosMas5Estudiantes datos; loop())
                                 | #"c" => (buscarPorEstudiante datos; loop())
                                 | #"d" => (cursosPorCreditos datos; loop())
                                 | #"e" => (resumenGeneral datos; loop())
                                 | #"s" => print "Saliendo del analizador...\n"
                                 | _ => (print "Opcion no valida.\n"; loop()))
               | NONE => (print "Error.\n"; loop()))
    in loop() end

fun main () =
    (print "=== ANALIZADOR DE MATRICULAS ===\n";
     print "Ruta del archivo CSV (ej. matricula.csv): ";
     let val ruta = leerString ""
         val datos = leerCSV ruta
     in if null datos then print "No se encontraron datos o el archivo esta vacio.\n"
        else (print ("Se cargaron " ^ Int.toString (length datos) ^ " matriculas.\n");
              menuAnalisis datos)
     end)
