(* ========================================================================
   analizador.sml
   Autor: Luis [Nombre completo del estudiante]
   Carnet: [Numero de carnet]
   Fecha: Abril 2026
   Descripcion:
       Programa que analiza un archivo CSV de matriculas universitarias.
       Ofrece varias consultas: ranking de cursos por ingreso, cursos con mas
       de 5 estudiantes, busqueda por estudiante, cursos por cantidad de creditos,
       y un resumen general. Se hace uso de funciones de orden superior:
       List.map, List.filter, List.foldl.
   ========================================================================= *)

(* Tipo que representa una matricula: (carnet, nombre, curso, creditos, costo) *)
type matricula = string * string * string * int * real

(* ------------------------------------------------------------------------
   leerCSV : string -> matricula list
   Lee un archivo CSV y lo convierte en una lista de matriculas.
   La primera linea (cabecera) se omite automaticamente.
   Formato esperado por linea:
       carnet,nombre,curso,creditos,costo_credito
   Si alguna linea no tiene el formato correcto o los numeros no son validos,
   se imprime un aviso y se omite esa linea.
   Parametro: archivo - ruta del archivo CSV.
   Retorna: lista de matriculas (puede ser vacia).
   ------------------------------------------------------------------------ *)
fun leerCSV archivo =
    let val ins = TextIO.openIn archivo
        (* Funcion recursiva que lee linea por linea *)
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

(* ------------------------------------------------------------------------
   Funciones auxiliares de entrada con validacion.
   ------------------------------------------------------------------------ *)

(* leerEntero : string -> int
   Solicita un numero entero, validando la entrada. *)
fun leerEntero prompt =
    (print prompt; case TextIO.inputLine TextIO.stdIn of
         SOME line => (case Int.fromString (String.substring (line, 0, size line - 1)) of
                           SOME i => i
                         | NONE => (print "Debe ser entero.\n"; leerEntero prompt))
       | NONE => (print "Error.\n"; leerEntero prompt))

(* leerReal : string -> real
   Solicita un numero real, validando la entrada. *)
fun leerReal prompt =
    (print prompt; case TextIO.inputLine TextIO.stdIn of
         SOME line => (case Real.fromString (String.substring (line, 0, size line - 1)) of
                           SOME r => r
                         | NONE => (print "Debe ser real.\n"; leerReal prompt))
       | NONE => (print "Error.\n"; leerReal prompt))

(* leerString : string -> string
   Solicita una cadena de texto. *)
fun leerString prompt =
    (print prompt; case TextIO.inputLine TextIO.stdIn of
         SOME line => String.substring (line, 0, size line - 1)
       | NONE => "")

(* ------------------------------------------------------------------------
   rankingCursos : matricula list -> unit
   a) Muestra un ranking de cursos ordenado descendentemente por monto total
      generado (creditos * costo_credito). El usuario filtra por rango min-max.
   Algoritmo:
       - List.foldl acumula en una lista asociativa (curso, montoTotal).
       - ListMergeSort.sort ordena de mayor a menor.
       - List.filter selecciona los cursos dentro del rango indicado.
   Parametro: datos - lista de matriculas.
   ------------------------------------------------------------------------ *)
fun rankingCursos datos =
    let (* actualizar: agrega o acumula el monto de un curso en la lista *)
        fun actualizar (curso, monto) [] = [(curso, monto)]
          | actualizar (curso, monto) ((c,t)::rest) =
            if c = curso then (c, t + monto) :: rest
            else (c,t) :: actualizar (curso, monto) rest
        val montosCurso = List.foldl (fn ((_,_,curso,cred,costo), acc) =>
                                         actualizar (curso, real(cred) * costo) acc) [] datos
        (* comparar: retorna true si t1 < t2, lo que produce orden descendente *)
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

(* ------------------------------------------------------------------------
   cursosMas5Estudiantes : matricula list -> unit
   b) Identifica los cursos que tienen mas de 5 estudiantes diferentes.
   Algoritmo:
       - List.foldl construye una lista (curso, lista de carnets) sin repetidos.
       - List.filter selecciona los cursos con mas de 5 entradas en esa lista.
   Parametro: datos - lista de matriculas.
   ------------------------------------------------------------------------ *)
fun cursosMas5Estudiantes datos =
    let (* agregarEstudiante: inserta un carnet en la lista del curso si no existe *)
        fun agregarEstudiante (curso, carnet) [] = [(curso, [carnet])]
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

(* ------------------------------------------------------------------------
   buscarPorEstudiante : matricula list -> unit
   c) Busca matriculas cuyo carnet coincida exactamente con el patron dado,
      o cuyo nombre contenga el patron (subcadena, sin distinguir mayusculas).
   Algoritmo:
       - List.filter con una funcion que verifica carnet = patron
         o bien isSubstring sobre ambos textos convertidos a minusculas.
   Parametro: datos - lista de matriculas.
   ------------------------------------------------------------------------ *)
fun buscarPorEstudiante datos =
    let val patron = leerString "\nIngrese carnet exacto o parte del nombre: "
        (* coincide: true si el carnet es exacto o el nombre contiene el patron *)
        fun coincide (carnet, nombre, _, _, _) =
            carnet = patron orelse
            String.isSubstring (String.map Char.toLower patron) (String.map Char.toLower nombre)
        val resultados = List.filter coincide datos
    in if null resultados then print "No se encontraron matriculas.\n"
       else (print "\nResultados:\n";
             List.app (fn (carnet, nombre, curso, cred, costo) =>
                          print (carnet ^ " | " ^ nombre ^ " | " ^ curso ^ " | " ^
                                 Int.toString cred ^ " creditos | " ^ Real.toString costo ^ "/credito\n")) resultados;
             print "\n")
    end

(* ------------------------------------------------------------------------
   cursosPorCreditos : matricula list -> unit
   d) Dado un numero de creditos, muestra los cursos (sin repetir) que tienen
      exactamente esa cantidad de creditos.
   Algoritmo:
       - List.filter selecciona las matriculas con los creditos buscados.
       - List.map extrae unicamente el codigo del curso.
       - La funcion auxiliar unicos elimina duplicados.
   Parametro: datos - lista de matriculas.
   ------------------------------------------------------------------------ *)
fun cursosPorCreditos datos =
    let val creditosBusc = leerEntero "\nIngrese numero de creditos: "
        fun coincideCred (_,_,_,cred,_) = cred = creditosBusc
        val cursos = List.map (fn (_,_,curso,_,_) => curso) (List.filter coincideCred datos)
        (* unicos: elimina elementos repetidos preservando la primera aparicion *)
        fun unicos [] = []
          | unicos (x::xs) = if List.exists (fn y => y = x) xs then unicos xs else x :: unicos xs
        val distintos = unicos cursos
    in print ("\n--- Cursos con " ^ Int.toString creditosBusc ^ " creditos ---\n");
       if null distintos then print "Ninguno.\n"
       else List.app (fn c => print (c ^ "\n")) distintos;
       print "\n"
    end

(* ------------------------------------------------------------------------
   resumenGeneral : matricula list -> unit
   e) Genera un informe completo con:
       1. Cantidad de estudiantes distintos por curso.
       2. Estudiante con mayor cantidad de creditos matriculados.
       3. Estudiante con menor cantidad de creditos matriculados.
       4. Curso con mayor ingreso total.
       5. Estudiante que genera mayor ingreso total.
   Algoritmo: multiples List.foldl para acumular datos por curso y por
   estudiante, List.map para extraer valores y calcular maximos/minimos.
   Parametro: datos - lista de matriculas.
   ------------------------------------------------------------------------ *)
fun resumenGeneral datos =
    let (* 1. Estudiantes distintos por curso *)
        fun agregarEstCurso (curso, carnet) [] = [(curso, [carnet])]
          | agregarEstCurso (curso, carnet) ((c,est)::rest) =
            if c = curso then
                if List.exists (fn e => e = carnet) est then (c,est)::rest
                else (c, carnet::est)::rest
            else (c,est)::agregarEstCurso (curso, carnet) rest
        val estudiantesPorCurso = List.foldl (fn ((carnet,_,curso,_,_), acc) =>
                                                 agregarEstCurso (curso, carnet) acc) [] datos

        (* 2 y 3. Creditos totales por estudiante *)
        fun sumarCreditosEst (carnet, cred) [] = [(carnet, cred)]
          | sumarCreditosEst (carnet, cred) ((c,sum)::rest) =
            if c = carnet then (c, sum+cred)::rest else (c,sum)::sumarCreditosEst (carnet, cred) rest
        val creditosPorEst = List.foldl (fn ((carnet,_,_,cred,_), acc) =>
                                            sumarCreditosEst (carnet, cred) acc) [] datos
        val creditosSolo = List.map #2 creditosPorEst
        val mayorCreditos = List.foldl (fn (c1, c2) => if c1 > c2 then c1 else c2) 0 creditosSolo
        val menorCreditos = List.foldl (fn (c1, c2) => if c1 < c2 then c1 else c2) (valOf Int.maxInt) creditosSolo
        fun buscarEstPorCreditos creditos = List.find (fn (_,c) => c = creditos) creditosPorEst

        (* 4. Curso con mayor ingreso total *)
        fun actualizarCursoMonto (curso, monto) [] = [(curso, monto)]
          | actualizarCursoMonto (curso, monto) ((c,t)::rest) =
            if c = curso then (c, t+monto)::rest else (c,t)::actualizarCursoMonto (curso, monto) rest
        val montosCurso = List.foldl (fn ((_,_,curso,cred,costo), acc) =>
                                         actualizarCursoMonto (curso, real(cred)*costo) acc) [] datos
        val (cursoMayorMonto, mayorMonto) =
            List.foldl (fn ((c,t), (cMax,tMax)) => if t > tMax then (c,t) else (cMax,tMax)) ("",0.0) montosCurso

        (* 5. Estudiante que genera mayor ingreso total *)
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

        (* mostrarEstudiante: dado un carnet retorna "Nombre (carnet)" *)
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

(* ------------------------------------------------------------------------
   menuAnalisis : matricula list -> unit
   Presenta un menu interactivo con todas las opciones de analisis.
   Permite seleccionar repetidamente hasta presionar 's' para salir.
   Parametro: datos - lista de matriculas ya cargada desde el archivo.
   ------------------------------------------------------------------------ *)
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

(* ------------------------------------------------------------------------
   main : unit -> unit
   Funcion principal del programa Analizador.
   Solicita la ruta del archivo CSV, lo carga y, si hay datos, inicia el menu.
   ------------------------------------------------------------------------ *)
fun main () =
    (print "=== ANALIZADOR DE MATRICULAS ===\n";
     print "Ruta del archivo CSV (ej. matricula.csv): ";
     let val ruta = leerString ""
         val datos = leerCSV ruta
     in if null datos then print "No se encontraron datos o el archivo esta vacio.\n"
        else (print ("Se cargaron " ^ Int.toString (length datos) ^ " matriculas.\n");
              menuAnalisis datos)
     end)
