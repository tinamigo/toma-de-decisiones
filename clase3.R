#Ejercicio 1------------------------------------------------------------

#muS    <- 1.5
#muN    <- 0
#sigma  <- 1

#criterio <- seq(muN-3*sigma, muS+3*sigma, length.out = 100)

respCorr <- function(muS, muN, sigma, pRuido, pSenial, criterio){
  Pc <- rep(NA, length(criterio))
  for (i in 1:length(criterio)){
    hits <- 1 - pnorm(criterio[i], muS, sigma)  # tasa de hits
    cr   <- pnorm(criterio[i], muN, sigma)  # tasa de rechazos correctos
    Pc[i] <- (hits * pSenial + cr * pRuido) # % de respuestas correctas
  }
  return(Pc)
}

pc <- respCorr(1.5, 0, 1, 0.5, 0.5, seq(-3, 1.5+3, length.out = 100))
plot(pc, type='l')


#Ejercicio 2-----------------------------------------------------
library(lestat)
fun_basicsdt <- function(h,f){     #traído de la clase pasada
  # La función 'invcdf(normal(), p)' calcula z(p) 
  d <- invcdf(normal(), h) -  invcdf(normal(), f)
  
  c <- - 0.5 * ( invcdf(normal(), h) +  invcdf(normal(), f))
  
  logB <- 0.5 * (invcdf(normal(), f)^2 - invcdf(normal(), h)^2)
  
  res <- c(d,c, logB)
  
  return(res)
}

fun_basicsdt(496/600, 73/400)


muS      <- 1.84695334
muN      <- 0
sigma    <- 1
criterio <- 0.9058788

x  <- seq(from = muN-3*sigma, to = muS+3*sigma, len = 300)
yN <- dnorm(x, muN, sigma)
yS <- dnorm(x, muS, sigma)

plot(x, yN, type = "l", lwd = 3, col="red", xlab = "respuesta interna")
lines(x, yS, lwd = 4, col="blue")
abline(v=criterio, lwd=3)
legend(+3, 0.3, legend = c('ruido','señal'), col = c("red", "blue"), lty = 1,  lwd = 3)

pc <- respCorr(muS, muN, sigma, 0.4, 0.6, x)
maximo <- max(pc)

for(i in 1:length(x)){
  if (pc[i] == maximo){
    max_crit = x[i]
  }  
}
max_crit
abline(v=max_crit, lwd=3)

c <- max_crit - (muS/2)
c

logB <- c * muS
logB

logB_calc <- log(400/600)
logB_calc

c_calc <- logB_calc / muS
c_calc


#Ejercicio 3-----------------------------------------------------------






















