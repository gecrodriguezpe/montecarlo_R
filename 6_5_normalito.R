##______________________________________________________________________________
##______________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECON�MICAS
#                              ECONOMETRIA II 
#                 Simulaci�n de Monte Carlo : Regresi�n lineal simple 
##______________________________________________________________________________
##______________________________________________________________________________


################################### Descripci�n de la simulaci�n #################################
# El script simulara las propiedades muestrales del estimador de MCO
# Modelo: y_i ~ beta_0 + beta_1 x_i + e_i
# Se simulara con:
#### beta_0 = 7.3832
#### beta_1 = 0.2323
#### sigma2 = 46.853
#### T = 40 (tama�o de cada muestra)
#### e_i (error normal)

# Todos los anteriores par�metros de la simulaci�n se pueden modificar f�cilmente al ser variables
# de las funciones de Monte Carlo definida abajo
###################################################################################################

library(tidyverse)
library(broom)  # Es un paquete fundamental para interactuar con diferentes objetos de regresi�n
# Para saber que objetos son compatibles con broom: vignette("available-methods") en la consola
# El paquete broom provee las funciones: tidy y glance (ambas retornan objetos df de la regresi�n espec�ficada)

setwd("~/Camilo/R/personal/montecarlo")

data = read_csv("Tabla5-2.csv")

################################### C�digo detr�s de la simulaci�n #################################

set.seed(12345)

random_sampling = function(sample_size, params_vect, x){
  # sample_size: The size of the sample that will be considered
  # params_vect: vector of given parameters: In this case they are beta_0 = 7.3832, beta_1 = 0.2323, sigma2 = 46.853
  # x: It is a regressor (a vector) 
  # el error se simulara como una variable normal con media cero y varianza # params_vect[3] es sigma2 = 46.853
  error = rnorm(n = sample_size, mean = 0, sd = sqrt(params_vect[3])) # params_vect[1] es beta_0 y params_vect[2] es beta_1
  dependent = params_vect[1] + params_vect[2] * x + error    # simulaci�n de la variable dependiente
  return(dependent) # la funci�n retorna la variable dependiente simulada 
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

# La simulaci�n de Monte Carlo simula los beta0, beta1, sigma, var(b0), var(b1)
monte_carlo = function(formula, df, params_vect, x, sample_size, n_sim){
  # La funci�n monte carlo retorna: un df que contiene todos los beta0, beta1, sigma, var(b0), var(b1) simulados en cada iteracci�n de la simulaci�n 
  # creo un df que va a contener las variables de inter�s para cada iteraci�n de la simulaci�n 
  param_df = data.frame(b0 = double(), b1 = double(), sigma2 = double(), varb1 = double(), varb2 = double())
  for (i in 1:n_sim){
    df$y = random_sampling(sample_size, params_vect, x) # simulo por cada iteraci�n una variable dependiente y dados los param�tros 
    estimate_b = ols_regression(formula, df)$estimate   # realizo la simulaci�n por OLS para encontrar beta_0 y beta_1 por muestra simulada
    sigma = ols_population_variance(formula, df)$sigma  # realizo la simulaci�n por OLS para encontrar sigma por muestra simulada
    sd_b = ols_regression(formula, df)$std.error        # realizo la simulaci�n por OLS para encontrar las estimaciones de var(b0), var(b1) por muestra
    param_df[i,] = c(estimate_b[2], estimate_b[1], sigma^2, (sd_b[2])^2, (sd_b[1])^2)  # voy lleno cada fila por cada iteracci�n de la simulaci�n 
  }
  return(param_df)
}


###################################################################################################
############################ Ac� solo se aplica la funci�n monte_carlo y ya #######################

# experimento monte carlo para explorar las propiedades del estimador de MCO para una regresi�n simple

params_vect = c(7.3832, 0.2323, 46.853)  # vector de parametros donde beta_0 = 7.3832, beta_1 = 0.2323, beta_ 2 = 46.853
income = data$income   # regresor para la simulaci�n 

#######___________________________________________
### La funci�n Monte Carlo hace toda la simulaci�n 

# Para que la simulaci�n funcione siempre la formula debe empezar con y. Es decir, y ~ regrestor (Monte Carlo est� programado as�)
# La simulaci�n de monte carlo retorna un data frame 
ols_sim = monte_carlo(y ~ income, data, params_vect, income, 40, 1000)  # se simularan 1000 muestras de tama�o 40  
glimpse(ols_sim)

# Histograma que demuestra que el estimador de MCO es una VA normal bajo los supuestos 
# del modelo cl�sico lineal 

# Solo es necesario especificar la columna del df retornado por la funci�n monte carlo
# para ver el histograma del par�metro estimado de inter�s
hist_sim = ols_sim %>% 
  ggplot(aes(x = b1)) +
  geom_histogram(fill = 'orange') +
  theme_light()

hist_sim