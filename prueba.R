library(tidyverse)
library(plm)
library(broom)

#set.seed(12345)

################################################
# Set of necessary functions for the simmulation 

# Simulación de un proceso AR1 (Esta función no se necesita pero ayuda a entender la función)
ar1 = function(sample_size, a0, a1, sigma){
  y = c()
  y[1] = a0/(1-a1)
  for (i in 2:sample_size){
    y[i] = a0 + a1 * y[i - 1] + rnorm(1, 0, sqrt(sigma))
  }
  return(y)
}

ar1_regressor = ar1(40, 10, 0.5, 5)

# # Simulación de un regresor para datos panel con comportamiento serial AR1
# ar1_panel = function(){
#   sample_size, num_periods, a0, a1, sigma2
# }


### El regresor x se simulara como un proceso 
regressors_indexes = function(sample_size, num_periods, a0, a1, sigma2){
  total = sample_size * num_periods
  df = data.frame(x = double(), id = double(), time = double())
  df[1:sample_size, 1] = rnorm(sample_size, 0, sqrt(sigma2)) # tiene sentido porque los valores iniciales de x son arbitrarios 
  df[1:sample_size, 2] = 1:sample_size
  df[1:sample_size, 3] = rep(1, sample_size)
  for (t in 2:num_periods){
    t1 = (t-1) * sample_size + 1
    t2 = t * sample_size
    t3 = (t - 2) * sample_size + 1
    t4 = (t-1) * sample_size
    df[t1:t2 , 1] = a0 + a1 * df[t3:t4 , 1] + rnorm(sample_size, 0, sqrt(sigma2))
    df[t1:t2 , 2] = df[1:sample_size, 2]
    df[t1:t2, 3] = rep(t, sample_size)
  }
  df[["x1"]] = rnorm(total, mean = 1, sd = 2)
  df[["x2"]] = rnorm(total, mean = 3, sd = 7)
  df[["x3"]] = rnorm(total, mean = 5, sd = 4)
  df2 = arrange(df, id)
  return(df2)
}

df1 = regressors_indexes(40, 4, 10, 0.5, 5)

# Time_demeaned function for a panel data regressor 
time_demeaned = function(name_x, df){
    df$..x = df[[name_x]]
    df2 = df %>% 
    group_by(id) %>% 
    summarise(x_mean = mean(..x)) 
    df = df %>%  
      select(-c(..x))
    return(df2)
}

df2 = time_demeaned("x", df1)

# efecto fijo: para simular la correlacion con el regresor se usará ai ~ a0 + delta xi_prom + ri
efecto_fijo = function(samples_size, name_x, df, a0, delta, sigma2){
  error_fijo = a0 + delta * df[[name_x]] + rnorm(samples_size, 0, sqrt(sigma2))
  return(error_fijo)
}

e_fijo = efecto_fijo(40, "x_mean", df2, 2, 3, 9)

# formula: yit = b0 + gamma xit + b1 xit1 + b2 xit2 + b3 xit3 + ai + uit
creating_y = function(sample_size, num_periods, u0, u1, sigma2_r, df_reg, param_vec, e_fijo){
  # sample_size: The size of the sample that will be considered
  # params_vect: vector of given parameters
  # el parametro de interes es gamma = param_vec[2] que es el que corresponde a x(regresor correlaciondo con a)
  # x: It is a regressor (a vector) 
  # el error se simulara como una variable normal con media cero y varianza # params_vect[3] es sigma2 = 46.853
  total = sample_size * num_periods
  df_total = arrange(df_reg, time)
  df_prov = data.frame(u = double(), a = double(), y = double()) # data frame provisional para los errores a y u y para la variable dependiente 
  df_prov[1:sample_size, 1] = rnorm(sample_size, 0, sqrt(sigma2_r)) # tiene sentido porque los valores iniciales de u son arbitrarios 
  df_prov[["a"]][1:sample_size] = e_fijo
  df_prov[["y"]][1:sample_size] = param_vec[1] + param_vec[2] * df_total$x[1:sample_size] + param_vec[3] * df_total$x1[1:sample_size] + param_vec[4] * df_total$x2[1:sample_size] + param_vec[5] * df_total$x3[1:sample_size] + df_prov$a + df_prov$u
  for (t in 2:num_periods){
    t1 = (t-1) * sample_size + 1
    t2 = t * sample_size
    t3 = (t - 2) * sample_size + 1
    t4 = (t-1) * sample_size
    df_prov[t1:t2, 1] = u0 + u1 * df_prov[["u"]][t3:t4] + rnorm(sample_size, 0, sqrt(sigma2_r))
    df_prov[["a"]][t1:t2] = e_fijo 
    df_prov[["y"]][t1:t2] = param_vec[1] + param_vec[2] * df_total$x[t3:t4] + param_vec[3] * df_total$x1[t3:t4] + param_vec[4] * df_total$x2[t3:t4] + param_vec[5] * df_total$x3[t3:t4] + df_prov$a[t3:t4] + df_prov$u[t3:t4]
  }
  df_total[["u"]] = df_prov[["u"]]
  df_total[["a"]] = df_prov[["a"]]
  df_total[["y"]] = df_prov[["y"]]
  df_total2 = arrange(df_total, id)
  return(df_total2)
}

param_vec = c(15, 5, 7, 4, 12) # Provides a vector with the beta and the gamma parameters for the regressor required to compute yit 
# El parámetro de interés es param_vec[2] = 5 (Que es el que acompaña al regresor x que está correlacionado con el arror ai)

# data frame con los datos panel construidos
df_completo = creating_y(40, 4, 2, 1, 2, df1, param_vec, e_fijo)
