#Asignación, el objeto debe estar apuntado por la flecha <- 

x <- 10

5 -> x

usuario <- "Jesus Ruiz"
nchar(usuario) #metodo de String

#Operadores aritmeticos

x + 5

x/5

x-5

x*2

x**5

x%%2

#Operadores Logicos

x > 4

x==5

x<4

#Vectores

estudiantes <- c('Juan','Maria','Felipe','Aurora','Marco')
estudiantes
notas <- c('2.3','3.2','4','3','3.5')
notas

#Agregar elementos a vectores
estudiantes <- c(estudiantes, 'Jorge')
notas <- c(notas, 3.5)

length(estudiantes)
notas >=3.5


#data ->
year <- c('2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')
disney <- c(11, 13, 11, 8, 12, 11, 12, 8, 10)

plot(
  x = year,
  y = disney
)

plot(
  x = year,
  y = disney,
  main = 'Peliculas de Disney',
  col = 'cornflowerblue',
  pch = 16,
  panel.first = grid()
)



#Conceptos de POO

# Crear un objeto de clase S3
persona <- list(nombre = "Juan", edad = 25)
class(persona) <- "Persona"

# Definir un método específico para la clase
print.Persona <- function(obj) {
  cat("Nombre:", obj$nombre, "\n")
  cat("Edad:", obj$edad, "\n")
}

# Usar el objeto
print(persona)



#Crear una clase S4:
# Definir clase
setClass(
  "Persona",
  slots = list(nombre = "character", edad = "numeric")
)

persona <- new("Persona", nombre = "Juan", edad = 25)

# Acceso a atributos mediante @
print(persona@nombre)  
persona@edad <- 30 #Todavia se puede modificar el atributo directamente



# Definir una clase R6
#Encapsulamiento más estricto
Persona <- R6Class(
  "Persona",
  public = list(
    nombre = NULL,
    initialize = function(nombre, edad) {
      self$nombre <- nombre
      private$edad <- edad
    },
    getEdad = function() {
      return(private$edad)
    },
    setEdad = function(nuevaEdad) {
      if (nuevaEdad > 0) private$edad <- nuevaEdad
      else stop("La edad debe ser positiva")
    }
  ),
  private = list(
    edad = NULL  
  )
)

# Crear objeto
persona <- Persona$new(nombre = "Juan", edad = 25)

# Acceso mediante métodos
print(persona$getEdad())  
persona$setEdad(30)       
print(persona$getEdad())  

persona$edad
persona$nombre



#Herencia

#En S3
# Clase padre
persona <- list(nombre = "Juan", edad = 30)
class(persona) <- "Persona"

# Clase hija
estudiante <- list(nombre = "Ana", edad = 20, matricula = "12345")
class(estudiante) <- c("Estudiante", "Persona")

# Métodos
print.Persona <- function(obj) {
  cat("Persona:", obj$nombre, "Edad:", obj$edad, "\n")
}

print.Estudiante <- function(obj) {
  cat("Estudiante:", obj$nombre, "Edad:", obj$edad, "Matrícula:", obj$matricula, "\n")
}

# Uso
print(persona)    
print(estudiante)
  
#Soporta herencia múltiple mediante un vector de clases



#Herencia en R6
#En R6 la herencia es explícita y se define usando el parámetro inherit.

library(R6)

# Clase padre
Persona <- R6Class(
  "Persona",
  public = list(
    nombre = NULL,
    edad = NULL,
    initialize = function(nombre, edad) {
      self$nombre <- nombre
      self$edad <- edad
    },
    mostrar = function() {
      cat("Persona:", self$nombre, "Edad:", self$edad, "\n")
    }
  )
)

# Clase hija
Estudiante <- R6Class(
  "Estudiante",
  inherit = Persona,
  public = list(
    matricula = NULL,
    initialize = function(nombre, edad, matricula) {
      super$initialize(nombre, edad) 
      self$matricula <- matricula
    },
    mostrar = function() {
      cat("Estudiante:", self$nombre, "Edad:", self$edad, "Matrícula:", self$matricula, "\n")
    }
  )
)

# Uso
estudiante <- Estudiante$new(nombre = "Ana", edad = 20, matricula = "12345")
estudiante$mostrar()


#Agregación, composición y asociación

#Asociación
# Clase Profesor
Profesor <- R6Class(
  "Profesor",
  public = list(
    nombre = NULL,
    initialize = function(nombre) {
      self$nombre <- nombre
    },
    enseñar = function(curso) {
      cat(self$nombre, "enseña el curso de", curso$nombre, "\n")
    }
  )
)

# Clase Curso
Curso <- R6Class(
  "Curso",
  public = list(
    nombre = NULL,
    initialize = function(nombre) {
      self$nombre <- nombre
    }
  )
)

profesor <- Profesor$new("Dr. Pérez")
curso <- Curso$new("Matemáticas")

profesor$enseñar(curso)



#Agregación

# Clase Estudiante
Estudiante <- R6Class(
  "Estudiante",
  public = list(
    nombre = NULL,
    initialize = function(nombre) {
      self$nombre <- nombre
    }
  )
)

# Clase Curso
Curso <- R6Class(
  "Curso",
  public = list(
    nombre = NULL,
    estudiantes = list(),
    initialize = function(nombre) {
      self$nombre <- nombre
    },
    agregarEstudiante = function(estudiante) {
      self$estudiantes <- c(self$estudiantes, estudiante)
    }
  )
)

curso <- Curso$new("Física")
est1 <- Estudiante$new("Ana")
est2 <- Estudiante$new("Luis")


curso$agregarEstudiante(est1)
curso$agregarEstudiante(est2)


print(curso$nombre)  # Física
print(curso$estudiantes[[2]]$nombre)



#Ejemplo de Interfaz Gráfica


library(tcltk)
library(R6)


# Clase Reloj
Reloj <- R6Class("Reloj",
                 public = list(
                   segundos = 0,
                   minutos = 0,
                   horas = 0,
                   
                   initialize = function(segundos = 0, minutos = 0, horas = 0) {
                     self$segundos <- segundos
                     self$minutos <- minutos
                     self$horas <- horas
                   },
                   
                   moverSegundero = function() {
                     if (self$segundos == 59) {
                       self$segundos <- 0
                       self$moverMinutero()
                     } else {
                       self$segundos <- self$segundos + 1
                     }
                   },
                   
                   moverMinutero = function() {
                     if (self$minutos == 59) {
                       self$minutos <- 0
                       self$moverHorario()
                     } else {
                       self$minutos <- self$minutos + 1
                     }
                   },
                   
                   moverHorario = function() {
                     if (self$horas == 12) {
                       self$horas <- 1
                     } else {
                       self$horas <- self$horas + 1
                     }
                   },
                   
                   obtenerHora = function() {
                     sprintf(self$horas, self$minutos, self$segundos)
                   }
                 )
)



# Crear la interfaz gráfica
ventana <- tktoplevel()
tkwm.title(ventana, "Reloj")

# Etiqueta para mostrar la hora
etiquetaHora <- tklabel(ventana, text = miReloj$obtenerHora(), font = "Helvetica 24")
tkpack(etiquetaHora, pady = 20)

# Función para actualizar la etiqueta de la hora
actualizarHora <- function() {
  tkconfigure(etiquetaHora, text = miReloj$obtenerHora())
}

# Botón para avanzar el segundero
botonSegundero <- tkbutton(ventana, text = "Avanzar Segundero", 
                           command = function() {
                             miReloj$moverSegundero()
                             actualizarHora()
                           })
tkpack(botonSegundero, pady = 10)

# Botón para avanzar el minutero
botonMinutero <- tkbutton(ventana, text = "Avanzar Minutero", 
                          command = function() {
                            miReloj$moverMinutero()
                            actualizarHora()
                          })
tkpack(botonMinutero, pady = 10)

# Botón para avanzar el horario
botonHorario <- tkbutton(ventana, text = "Avanzar Horario", 
                         command = function() {
                           miReloj$moverHorario()
                           actualizarHora()
                         })
tkpack(botonHorario, pady = 10)

# Mantener la interfaz abierta
tkfocus(ventana)



