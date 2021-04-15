#######################################
########### Ejercicio 10.15 ###########
#######################################

library(tidyverse)
library(broom)

setwd("~/Camilo/R/personal/montecarlo")

# Data frames

table = read_csv("data10_12.csv")
attach(table)

##############################################################################
############################ Simulacion de monte carlo #######################
##############################################################################

# Parametros para la simulacion
b = c(10, 0.4, 0.6)
sigma2 = 0.0625

################
##### Simulacion 
################

# La idea del programa es escoger un modelo mediante el criterio AIC 
# La formula correcta para la simulaciÃ³n es y ~ 10 x1 + 0.4 x2 + 0.6 x3 + e 
# x1 es un vector de 1s

set.seed(12345)

formulas = c(Ysim ~ x2 + x3, Ysim ~ x2 + x3 + x4, Ysim ~ x2 + x3 + x5, Ysim ~ x2 + x3 + x6, Ysim ~ x2 + x3 + x4 + x5, Ysim ~ x2 + x3 + x4 + x6, Ysim ~ x2 + x3 + x5 + x6)

sim_10_15 = function(df, n_sim, formulas){
  # la simulación va a tener un total de 3 dfs:
  ## df: df con la información de entrada 
  ## dfram1: df de valores lógicos. se usa sus columnas para completa dframe2
  ## dfram2: df que cuenta cuantas veces se seleccionó cada modelo por criterio AIC
  dframe1 = data.frame(mod1 = double(), mod2 = double(), mod3 = double(), mod4 = double(), mod5 = double(), mod6 = double(), mod7 = double())
  for(i in 1:n_sim){
    adj_vec = c()     # va a ver un vector de AIC por cada simulación 
    e = rnorm(nrow(table),mean = 0,sd = sqrt(sigma2))
    df$Ysim = b[1] + b[2] * x2 + b[3] * x3 + e  # Se calculan los Ys correctas dado los params de la sim
    for(j in 1:length(formulas)){
      mod = lm(formulas[[j]], df)
      adj = glance(mod)$AIC            # va a tener un AIC por cada modelo
      adj_vec = cbind(adj_vec, adj)    # adj_vec al final del for va a tener longitud "length(formulas) = 7" (cbind está teniendo el rol de un append)
    }
    min_aic = min(adj_vec)
    dframe1[i,] = (min_aic == adj_vec)   # lleno dframe1 con valores lógicos 
  }
  dframe2 = data.frame(count = double())
  for(i in (1:ncol(dframe1))){
    dframe2[i, ] = sum(dframe1[[i]])    # sumo los valores lógicos de las columnas de dframe1
  }
  return(dframe2)
}

conteo = sim_10_15(table, 100, formulas)
conteo