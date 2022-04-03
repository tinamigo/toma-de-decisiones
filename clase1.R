#EJERCICIO 1-----------------------------------
set.seed(420)

suma_dados <- function(N){
  todos     <- sample(c(1,2,3,4,5,6), N, replace = TRUE)
  respuesta <- sum(todos) 
  return(respuesta)
}

N <- 50

Nrep   <- 10000
output <- rep(NA, Nrep)  # Inicializo el vector que va a guardar los datos

# Repito el proceso suma_dados 10000 veces
for(i in 1:Nrep){
  output[i] <- suma_dados(N)
}

hist(output, plot = TRUE)


#EJERCICIO 2-----------------------------------------------------------------------------
set.seed(420)

pcumples <- function(N, Nrep){  
  
  # variable que va a contar las coincidencias
  n_coincidencias <- 0
  
  # simula Nrep grupos de N personas y verifica si hubo coincidencias o no
  for(i in 1:Nrep){
    cumples <- sample(seq(1,365,1), N, replace = TRUE)
    if(length(unique(cumples)) < N){
      n_coincidencias <- n_coincidencias + 1
    } 
  }
  
  # calcula la probabilidad estimada de coincidencias y la imprime en la consola
  p_coincidencias <- n_coincidencias / Nrep
  return(p_coincidencias)
}

 
# Define el vector con los tamaños de los grupos.
Nvec <- 1:80
p_c <- rep(NA, length(Nvec))

# ejecuta la función para cada elemento del vector anterior
for (i in 1:length(Nvec)){
  p_c[i] <- pbirthday(Nvec[i])
}

plot(Nvec, p_c)


#EJERCICIO 3---------------------------------------------------

n_votantes_cris <- 505
n_votantes_brich <- 495
#n_votantes_fit <- 1


votos <- c(rep(1, n_votantes_cris), rep(-1,n_votantes_brich))

cuentaCris <- 0
cuentaBrich <- 0
Nrep <- 10000

for(j in 1:Nrep){  
  # realizar un escrutinio y evaluar si A se mantuvo al frente durante todo el escrutinio.
  # (puede servir usar la funcion cumsum. Que hace?)
  escrutinio <- sample(votos, n_votantes_cris + n_votantes_brich, replace = FALSE)
  resultado  <- cumsum(escrutinio)
  
  resultado <- resultado[1:800]
  
  # evaluar si A se mantuvo siempre al frente
  cris_gana_siempre <- min(resultado) > 0
  brich_gana_siempre <- max(resultado) < 0

  if(cris_gana_siempre){
    cuentaCris <- cuentaCris + 1
  } else if (brich_gana_siempre){
    cuentaBrich <- cuentaBrich + 1
  }
}

pCris <- cuentaCris/Nrep
print(pCris)


pBrich <- cuentaBrich/Nrep
print(pBrich)

#EJERCICIO 5-------------------------------------------------------------------------------------------------------------------



  