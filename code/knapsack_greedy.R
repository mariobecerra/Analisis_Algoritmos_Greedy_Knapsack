library(tidyverse)
library(Rcpp)

# Funciones que calculan knapsack fraccional, y función auxiliar para knapsack 0/1 greedy
sourceCpp("knapsack_greedy.cpp")
# weight, profit capacity

# Función que calcula knapsack 0/1 greedy usando la heurística de tomar el objeto de mayor valor en cada paso
knapsack_1_0 <- function(weight, profit, capacity) {
  n = length(weight)
  datos <- tibble(
    ix = 1:n,
    weight = weight,
    profit = profit
  ) %>% 
    arrange(desc(profit))
  
  vector_aux <- knapsack_1_0_aux(as.matrix(datos), capacity)

  vector_final = vector_aux[vector_aux > 0]
  valor_final = sum(datos[vector_final,]$profit)
  
  return(list(
    profit = valor_final,
    result = vector_final
  ))
}

#Probar las funciones

knapsack_1_0(c(2, 3, 5, 7, 1, 4, 1), c(10, 5, 15, 7, 6, 18, 3), 15)
knapsack_1_0(c(5, 20, 10, 12), c(50, 140, 60, 60), 30)

knapsack_fract(c(2, 3, 5, 7, 1, 4, 1), c(10, 5, 15, 7, 6, 18, 3), 15)
knapsack_fract(c(5, 20, 10, 12), c(50, 140, 60, 60), 30)

