library(tidyverse)
library(fitdistrplus)
library(lubridate)

summary(llegadas$Minutos)
Minutos<-llegadas$Minutos
hist(Minutos, prob = T,breaks = 9, main = " Histograma Llegadas")

ks.test(llegadas$Minutos, "punif", 0, 1416)
n <- length(llegadas$Hora1)
llegadas$Tentre <- rep(NA, length(llegadas$Hora1))
llegadas$Tentre[2:n] <- diff(llegadas$Minutos)
entre_llegadas <- fitdist(llegadas$Entre[2:n], "exp")
intensidad <- entre_llegadas$estimate
Tiempo_Entre_llegadas<-llegadas$Entre
hist(Tiempo_Entre_llegadas, breaks = 10, probability = T,
     main = "Histograma tiempo entre llegadas")
lines(seq(0, 70, 0.001),dexp(seq(0, 70, 0.001), intensidad))
ks.test(llegadas$Entre, "pexp", tasa)
