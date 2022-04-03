# La función z está en el paquete 'lestat'. con la siguiente línea cargamos el paquete
library(lestat)

# Si obtienen un error, es porque el paquete no está instalado. Para eso, hacen (por única vez)
install.packages("lestat")


fun_basicsdt <- function(h,f){
  # La función 'invcdf(normal(), p)' calcula z(p) 
  d <- invcdf(normal(), h) -  invcdf(normal(), f)
  
  c <- - 0.5 * ( invcdf(normal(), h) +  invcdf(normal(), f))
  
  res <- c(d,c)
  
  return(res)
}

#Ejercicio 4

fun_basicsdt(0.92, 0.48) #Espiritista

fun_basicsdt(0.58, 0.09) #Escéptico

#Ejercicio 5

fun_basicsdt((17/28), (14/22))#Vidente
