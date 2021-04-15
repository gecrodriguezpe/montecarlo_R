##______________________________________________________________________________
##______________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                              ECONOMETRIA II 
#                 Simulación de Monte Carlo : Mínimos cuadrados ordinarios
##______________________________________________________________________________
##______________________________________________________________________________

# El script simulara las propiedades de los estimadores OLS para la media y la varianza poblacional
# Modelo: y_i ~ beta + e_i, donde beta es la media poblacional 
# Se simulara con:
## Beta = 20 (poblacional), un sigma = 10 (poblacional), un tamaño de muestra aleatoria T = 10 (por cada iteración de Monte Carlo)
## Un error e_i distribuido normal 
## Todos los anteriores parámetros de la simulación se pueden modificar fácilmente al ser variables
## de las funciones de Monte Carlo definidas abajo

library(tidyverse)

set.seed(12345)

random_sampling = function(sample_size, pop_mean, pop_var){
  # T is the sample size
  # pop_mean is the population mean
  # pop_variance is the population variance
  error = rnorm(n = sample_size, mean = 0, sd = sqrt(pop_var))
  dependent = pop_mean + error
  df = data.frame(e = error, y = dependent)
  return(dependent)
}

prueba_1 = random_sampling(10, 20, 10)

ols_population_mean = function(sample_size, dependent){
  b = sum(dependent) / sample_size
  return(b)
}

ols_population_variance = function(sample_size, dependent){
  b = ols_population_mean(sample_size, dependent)
  sigma = sum((dependent - b)^2) / (sample_size - 1)
  return(sigma)
}

monte_carlo_media_poblacional = function(sample_size, pop_mean, pop_var, n_sim){
  b_df = data.frame(b = double())
  for (i in 1:n_sim){
    y = random_sampling(sample_size, pop_mean, pop_var)
    b_df[i,] = ols_population_mean(sample_size, y)
  }
  media = mean(b_df[["b"]])  
  var = var(b_df[["b"]])
  print(paste("La media de los coeficientes OLS simulados por Monte Carlo para la media poblacional es", media))
  print(paste("La varianza de los coeficientes OLS simulados por Monte Carlo para la media poblacional es", var))
  return(b_df)
}

monte_carlo_varianza_poblacional = function(sample_size, pop_mean, pop_var, n_sim){
  sigma_df = data.frame(sigma = double())
  for (i in 1:n_sim){
    y = random_sampling(sample_size, pop_mean, pop_var)
    sigma_df[i,] = ols_population_variance(sample_size, y)
  }
  media = mean(sigma_df[["sigma"]])  
  var = var(sigma_df[["sigma"]])
  print(paste("La media de los coeficientes OLS simulados por Monte Carlo para la varianza poblacional es", media))
  print(paste("La varianza de los coeficientes OLS simulados por Monte Carlo para la varianza poblacional es", var))
  return(sigma_df)
}

# ols_media_poblacional y ols_varianza_poblacional son data frame que contienen todos los coeficientes 
# estimados por OLS para le media y la varianza poblacional respectivamente que surgen de la simulación de monte Carlo

# experimento monte carlo para media poblacional

ols_media_poblacional = monte_carlo_media_poblacional(10, 20, 10, 1000)
glimpse(ols_media_poblacional)

hist_media_poblacional = ols_media_poblacional %>% 
  ggplot(aes(x = b)) +
  geom_histogram(fill = 'blue')

# histograma que demuestra el estimador OLS de la media poblacional es normal
hist_media_poblacional  

# experimento monte carlo para varianza poblacional

ols_varianza_poblacional = monte_carlo_varianza_poblacional(6, 20, 10, 1000)
glimpse(ols_varianza_poblacional)

hist_var_poblacional = ols_varianza_poblacional %>% 
  ggplot(aes(x = sigma)) +
  geom_histogram(fill = 'red')

# histograma que demuestra el estimador OLS de la varianza poblacional es chi-cuadrado
hist_var_poblacional