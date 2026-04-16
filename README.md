# Tarea Corta 2 - Gestion de Matriculas en SML

## Requisitos
- SML of New Jersey (SML/NJ) instalado

## Estructura del proyecto
```
programa/
  creador.sml      - Agregar y limpiar matriculas
  analizador.sml   - Analizar el archivo CSV
  matricula.csv    - Archivo de datos
```

## Ejecucion del Creador

1. Abrir terminal en la carpeta del proyecto
2. Ejecutar:
```
sml
```
3. Dentro del REPL escribir:
```
use "creador.sml";
main();
```

### Opciones del Creador
- **1. Agregar nueva matricula**: Solicita carnet, nombre, codigo de curso, creditos y costo por credito. Guarda la matricula en matricula.csv.
- **2. Limpiar todo el catalogo**: Borra todos los registros y deja solo el encabezado del CSV.
- **3. Salir**: Cierra el programa.

## Ejecucion del Analizador

1. Abrir terminal en la carpeta del proyecto
2. Ejecutar:
```
sml
```
3. Dentro del REPL escribir:
```
use "analizador.sml";
main();
```
4. Ingresar la ruta del archivo CSV cuando se solicite (ej: `matricula.csv`)

### Opciones del Analizador
- **a) Ranking por ingreso**: Muestra cursos ordenados por monto total generado. Requiere ingresar un rango minimo y maximo.
- **b) Cursos con mas de 5 estudiantes**: Lista los cursos que tienen mas de 5 estudiantes distintos matriculados.
- **c) Buscar por estudiante**: Busca por carnet exacto o por parte del nombre.
- **d) Cursos por creditos**: Lista los cursos que tienen una cantidad especifica de creditos.
- **e) Resumen general**: Muestra estadisticas generales del sistema.
- **s) Salir**: Cierra el programa.

## Formato del archivo CSV
```
carnet_estudiante,nombre,curso,creditos,costo_credito
200440117,Juan Perez,IC4301,4,15000.0
```

## Repositorio
GitHub: 
