##______________________________________________________________________________
##______________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                              ECONOMETRIA II 
#                 Simulación de Monte Carlo : Regresión lineal simple 
##______________________________________________________________________________
##______________________________________________________________________________


################################### Descripción de la simulación #################################
# El script simulara las propiedades muestrales del estimador de MCO
# Modelo: y_i ~ beta_0 + beta_1 x_i + e_i
# Se simulara con:
#### beta_0 = 7.3832
#### beta_1 = 0.2323
#### sigma2 = 46.853
#### T = 40 (tamaño de cada muestra)
#### e_i (error normal)

# Todos los anteriores parámetros de la simulación se pueden modificar fácilmente al ser variables
# de las funciones de Monte Carlo definida abajo
###################################################################################################

library(tidyverse)
library(broom)  # Es un paquete fundamental para interactuar con diferentes objetos de regresión
# Para saber que objetos son compatibles con broom: vignette("available-methods") en la consola
# El paquete broom provee las funciones: tidy y glance (ambas retornan objetos df de la regresión específicada)

setwd("~/Camilo/R/personal/montecarlo")

data = read_csv("Tabla5-2.csv")

################################### Código detrás de la simulación #################################

set.seed(12345)

random_sampling = function(sample_size, params_vect, x){
  # sample_size: The size of the sample that will be considered
  # params_vect: vector of given parameters: In this case they are beta_0 = 7.3832, beta_1 = 0.2323, sigma2 = 46.853
  # x: It is a regressor (a vector) 
  # el error se simulara como una variable normal con media cero y varianza # params_vect[3] es sigma2 = 46.853
  error = rnorm(n = sample_size, mean = 0, sd = sqrt(params_vect[3])) # params_vect[1] es beta_0 y params_vect[2] es beta_1
  dependent = params_vect[1] + params_vect[2] * x + error    # simulación de la variable dependiente
  return(dependent) # la función retorna la variable dependiente simulada 
}

ols_regression = function(formula, df){
  # formula: regression formula
  # df: data frame
  # mod_df: data frame with the coefficients of the applied regression
  mod = lm(formula, data = df)
  mod_df = tidy(mod)
  return(mod_df)
}

ols_population_variance = function(formula, df){
  # formula: regression formula
  # df: data frame
  # mod_df: data frame with additional information about the regression such the pop variance
  mod = lm(formula, data = df)
  mod_df = glance(mod)
  return(mod_df)
}

# La simulación de Monte Carlo simula los beta0, beta1, sigma, var(b0), var(b1)
monte_carlo = function(formula, df, params_vect, x, sample_size, n_sim){
  # La función monte carlo retorna: un df que contiene todos los beta0, beta1, sigma, var(b0), var(b1) simulados en cada iteracción de la simulación 
  # creo un df que va a contener las variables de interés para cada iteración de la simulación 
  param_df = data.frame(b0 = double(), b1 = double(), sigma2 = double(), varb1 = double(), varb2 = double())
  for (i in 1:n_sim){
    df$y = random_sampling(sample_size, params_vect, x) # simulo por cada iteración una variable dependiente y dados los paramétros 
    estimate_b = ols_regression(formula, df)$estimate   # realizo la simulación por OLS para encontrar beta_0 y beta_1 por muestra simulada
    sigma = ols_population_variance(formula, df)$sigma  # realizo la simulación por OLS para encontrar sigma por muestra simulada
    sd_b = ols_regression(formula, df)$std.error        # realizo la simulación por OLS para encontrar las estimaciones de var(b0), var(b1) por muestra
    param_df[i,] = c(estimate_b[2], estimate_b[1], sigma^2, (sd_b[2])^2, (sd_b[1])^2)  # voy lleno cada fila por cada iteracción de la simulación 
  }
  return(param_df)
}


###################################################################################################
############################ Acá solo se aplica la función monte_carlo y ya #######################

# experimento monte carlo para explorar las propiedades del estimador de MCO para una regresión simple

params_vect = c(7.3832, 0.2323, 46.853)  # vector de parametros donde beta_0 = 7.3832, beta_1 = 0.2323, beta_ 2 = 46.853
income = data$income   # regresor para la simulación 

#######___________________________________________
### La función Monte Carlo hace toda la simulación 

# Para que la simulación funcione siempre la formula debe empezar con y. Es decir, y ~ regrestor (Monte Carlo está programado así)
# La simulación de monte carlo retorna un data frame 
ols_sim = monte_carlo(y ~ income, data, params_vect, income, 40, 1000)  # se simularan 1000 muestras de tamaño 40  
glimpse(ols_sim)

# Histograma que demuestra que el estimador de MCO es una VA normal bajo los supuestos 
# del modelo clásico lineal 

# Solo es necesario especificar la columna del df retornado por la función monte carlo
# para ver el histograma del parámetro estimado de interés
hist_sim = ols_sim %>% 
  ggplot(aes(x = b1)) +
  geom_histogram(fill = 'orange') +
  theme_light()

hist_sim