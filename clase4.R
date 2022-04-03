#Ejercicio 1------------------------------------------------------

# llevar a R para que trabaje en la ubicación en la que están los datos. Por ejemplo
setwd("/home/mamigo/Downloads/")

# importar los datos que están en 'data.csv'
datos <- read.csv(file="datos_guia4.csv", header=TRUE, sep=",")

install.packages("Rcpp")
install.packages("dplyr")
library(dplyr)

u.nombre <- unique(datos$nombre)
N        <- length(u.nombre)
ranking  <- data.frame( u.nombre, Pc = rep(NA, N) )

for (i in 1:N){
  # usamos filter para seleccionar los trials del participante "i"
  d             <- filter(datos, nombre == u.nombre[i]) 
  
  # calculamos el % de aciertos de ese participante
  ranking$Pc[i] <- mean(d$stim == d$resp)
}

# para ordenarlos por % de aciertos se puede usar la función arrange
ranking <- arrange(ranking, Pc)

## opción alternativa utilizando la potencialidad de la libreria dplyr

# primero le agrego la columna "correct" a datos
datos$correct <- datos$stim == datos$resp

# creo ranking2 que agrupa datos por nombre y le agrega el promedio para cada nombre
ranking2 <- datos %>%
  group_by(nombre) %>%
  summarize(pc = mean(correct))

# por ultimo lo ordeno 
ranking2 <- arrange(ranking2, pc)

# Pueden verificar que ranking y ranking2 son iguales

#C------

# guardo los niveles de coherencia utilizados en el experimento.
u.coherencia <- sort( unique(datos$coherencia) )

# para cada nivel de coherencia, hago un histograma de los tiempos de respuesta.
d1 <- filter(datos, coherencia == u.coherencia[1])
plot(density(d1$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(1,0,0), 
     lwd=3, main='distribuciones de RT', xlab="RT (s)")

d2 <- filter(datos, coherencia == u.coherencia[2])
lines(density(d2$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(0.8,0,0), lwd=3)

d3 <- filter(datos, coherencia == u.coherencia[3])
lines(density(d3$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(0.6,0,0), lwd=3)

d4 <- filter(datos, coherencia == u.coherencia[4])
lines(density(d4$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(0.4,0,0), lwd=3)

d5 <- filter(datos, coherencia == u.coherencia[5])
lines(density(d5$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(0.2,0,0), lwd=3)

legend(3, 1.5, legend = c('1','2','3','4','5'), lwd=3, 
       col=c())

# tiempo de respuesta en función de la coherencia
all.rt <- rep(NA, length(u.coherencia))
all.rt[1] <- mean(d1$rt)
all.rt[2] <- mean(d2$rt)
all.rt[3] <- mean(d3$rt)
all.rt[4] <- mean(d4$rt)
all.rt[5] <- mean(d5$rt)
plot(u.coherencia, all.rt, lwd=2, ylab = "RT (sec)")

# % de respuestas correctas en función de la coherencia
all.pc <- rep(NA, length(u.coherencia))
all.pc[1] <- mean(d1$correct)
all.pc[2] <- mean(d2$correct)
all.pc[3] <- mean(d3$correct)
all.pc[4] <- mean(d4$correct)
all.pc[5] <- mean(d5$correct)
plot(u.coherencia, all.pc, lwd=2, ylab = "Pc")

# respuestas correctas
dc <- filter(datos, stim==resp )
plot(density(dc$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(1,0,0), lwd=3, 
     main='distribuciones de RT', xlab="RT (s)")

# errores
di <- filter(datos, stim!=resp )
lines(density(di$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(0,0,1), lwd=3)

legend("topright", legend=c('rtas. correctas','rtas. incorrectas'), 
       lwd=3, col = c('red','blue'))

#Ejercicio 2----------------------------------------------------

n_pasos <- 200
drift   <- 0.0
umbral  <- 10
x_ini   <- umbral/2

x    <- rep(NA, n_pasos)
x[1] <- x_ini
i    <- 1
while( i < n_pasos & x[i] < umbral & x[i] > 0){
  x[i+1] <- x[i] + rnorm(1, mean = 0)
  i      <- i + 1  
}

x[i] <- ifelse( x[i] > umbral, umbral, 0) 

plot(x, type = 'l', xlim = c(1,n_pasos), ylim = c(-1,umbral+1), lwd=3, col = "black")
abline(umbral, 0, lwd=3)
abline(0, 0, lwd=3)

#Ejercicio 3-------------------------------------------------------

rw_decisiones <- function(n_pasos=1000, drift=0, sd_rw=1, umbral=10,  x_ini=umbral/2){
  x    <- x_ini + cumsum(rnorm(n_pasos, mean = 0))
  rt   <- which(x>umbral | x<0)[1]
  resp <- sign(x[rt])
  out  <- c(rt, resp)
  return(out)
}

coso <- rw_decisiones()
coso
