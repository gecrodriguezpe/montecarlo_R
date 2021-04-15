##______________________________________________________________________________
##______________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                              ECONOMETRIA II 
#                 Simulación de Monte Carlo : Datos Panel
##______________________________________________________________________________
##______________________________________________________________________________


################################### Descripción de la simulación ###########################################
# El script simulara las propiedades muestrales de los estimadores para datos panel (EF, PD, pooled OLS, EA)
# Modelo: y_it ~ beta_0 + beta_1 x_it1 + a_i + u_it   
##### Se va considerar un solo regresor para facilitar la exposición sin perdida de generalidad 
# Se simulara con:
#### beta_0 = 7.3832
#### beta_1 = 0.2323
#### sigmau2 = 46.853
#### T = 40 (tamaño de cada muestra/panel)
#### n = 4  (número de paneles a simular)
#### xit: se va a simular como una variable aleatoria normal con las sigs. características:
###### mean_x = 15
###### sigma_x = 9

# Modelamiento del error:
#### a_i (efecto fijo, se va a modelar como una variable aleatoria que no se observa y que es cte en el t)
###### sigma_a = 18.45
###### mean_a = beta_0 = 7.3832
#### u_it error idiosincrático
###### sigma_u2 = 46.853
###### mean_u2 = 0

# La simulación va a tener en cuenta dos estructuras de correlación posibel:
### 1. 

### 2. 

# Todos los anteriores parámetros de la simulación se pueden modificar fácilmente al ser variables
# de la función de Monte Carlo definida abajo
###################################################################################################

library(R6) # Biblioteca que se utiliza para manejar Objetos en R 
library(tidyverse) # Colección de blibiotecas útiles en R (dplyr, ggplot2)
library(broom)  # Es un paquete fundamental para interactuar con diferentes objetos de regresión
# Para saber que objetos son compatibles con broom: vignette("available-methods") 
# El paquete broom provee las funciones: tidy y glance (ambas retornan objetos df de la regresión específicada)

################################### Código detrás de la simulación #################################

set.seed(12345) # Semilla para reproducibilidad del código 

# Objeto que va a contener toda la funcionalidad detras de la simulación 
# Es decir contendra todo el procesa de simular x, y, las estructuras de correlación, los paneles y demás
DF_Panel = R6Class(
  "DataFramePanel", 
  private = list(
    ..sample_size = 40,
    ..num_panel = 4, 
    ..ai = double(), 
    ..uit = double(),
    ..params_beta = c(7.3832, 0.2323),
    ..params_ai = c(7.3832, 18.45),
    ..params_uit = c(0, 46.853),
    ..xit = rnorm(40, 15, 9)
  ), 
  public = list(
    # When creating the instance of the class
    initialize = function(sample_size, num_panel){
      if (!missing(sample_size)){
        private$..sample_size = sample_size
      } 
      if (!missing(num_panel)){
        private$..num_panel = num_panel
      }
    },
    # Creando la muestra aleatoria para cada panel 
    regressor = function(sample_size, params_vect){
      
    }
    time_demeaned = function(vect){
      
    }
    random_sampling = function(sample_size, params_vect, x){
      # sample_size: The size of the sample that will be considered
      # params_vect: vector of given parameters: In this case they are beta_0 = 7.3832, beta_1 = 0.2323, sigma2 = 46.853
      # x: It is a regressor (a vector) 
      error = rnorm(n = sample_size, mean = 0, sd = sqrt(params_vect[3]))
      dependent = params_vect[1] + params_vect[2] * x + error
      return(dependent) # la función retorna la variable dependiente simulada 
    }
  ), 
  active = list(
    xit = function(value){
      if (missing(value)){
        private$..xit
      }else{
        private$..xit = value
      }
    }
  )
)

panel_prueba = DF_Panel$new(30, 5, rnorm(30 * 5, 15, 9))

# ols_regression = function(formula, df){
#   mod = lm(formula, data = df)
#   mod_df = tidy(mod)
#   return(mod_df)
# }
# 
# ols_population_variance = function(formula, df){
#   mod = lm(formula, data = df)
#   mod_df = glance(mod)
#   return(mod_df)
# }
# 
# # La simulación de Monte Carlo simula los beta1, beta2, sigma, var(b1), var(b2)
# monte_carlo = function(formula, df, params_vect, x, sample_size, n_sim){
#   param_df = data.frame(b0 = double(), b1 = double(), sigma2 = double(), varb1 = double(), varb2 = double())
#   for (i in 1:n_sim){
#     df$y = random_sampling(sample_size, params_vect, x)
#     estimate_b = ols_regression(formula, df)$estimate
#     sigma = ols_population_variance(formula, df)$sigma
#     sd_b = ols_regression(formula, df)$std.error
#     param_df[i,] = c(estimate_b[2], estimate_b[1], sigma^2, (sd_b[2])^2, (sd_b[1])^2)
#   }
#   return(param_df)
# }
# 
# 
# ###################################################################################################
# 
# ############################ Acá solo se aplica la función monte_carlo y ya #######################
# 
# # experimento monte carlo para explorar las propiedades del estimador de MCO para una regresión simple
# 
# params_vect = c(7.3832, 0.2323, 46.853)
# income = data$income
# 
# #######___________________________________________
# ### La función Monte Carlo hace toda la simulación 
# 
# # Para que la simulación funcione siempre la formula debe empezar con y (Monte Carlo está programado así)
# # La simulación de monte carlo retorna un data frame 
# ols_sim = monte_carlo(y ~ income, data, params_vect, income, 40, 1000) 
# glimpse(ols_sim)
# 
# # Histograma que demuestra que el estimador de MCO es una VA normal bajo los supuestos 
# # del modelo clásico lineal 
# 
# # Solo es necesario especificar la columna del df retornado por la función monte carlo
# # para ver el histograma del parámetro estimado de interés
# hist_sim = ols_sim %>% 
#   ggplot(aes(x = b1)) +
#   geom_histogram(fill = 'orange') +
#   theme_light()
# 
# hist_sim