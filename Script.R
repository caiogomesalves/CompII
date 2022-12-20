library(tidyverse)

####----Trabalho 1----

# Distribuição 1:
## Função densidade de probabilidade:

f1 <- function(x){
  return(0.5 * sin(x))
}

## Função densidade acumulada

f1_acum <- function(x){
  F_1 <- (1 - cos(x))/2
  return(F_1)
}

## Função densidade inversa:

f1_inv <- function(n){
  u <- runif(n)
  x <- acos(1 - 2 * u)
  return(x)
}

# Gráfico da acumulada empírica:

set.seed(5050)

amostra_1 <- f1_inv(10000)

comp_1 <- as_tibble(amostra_1) %>% 
  ggplot(aes(x = value))+
    stat_ecdf(aes(color = "Empírica"))+
    geom_function(fun = function(x)f1_acum(x), aes(color = "Teórica"))+
    scale_colour_manual(values = c("#000000","#FF1100"))+
    labs(x = "x", y = "Probabilidade Acumulada", color = "Acumuladas")

comp_1

# Distribuição 2:

## Função de probabilidade acumulada:

f2_acum <- function(x,theta1,theta2){
  F_2 <- (x/theta1)*(1-theta2 * (1-(x/theta1)))^(-1/theta2)
  return(F_2)
}

## Função densidade de probabilidade:

f2_dens <- function(x,theta1,theta2){
  f <- (1/theta1)*((1-theta2*(1- x/theta1))^(-1/theta2) - 
                     (theta1^(1/theta2)) * x *(theta1 - theta2 * (theta1-x))^((-1-theta2)/theta2))
  return(f)
}

## Gráficos da variação de theta2:

ggplot()+
  xlim(0,1)+
  geom_function(fun = function(x)f2_dens(x,theta1 = 1, theta2 = 0.5), colour = "black")+
  geom_function(fun = function(x)f2_dens(x,theta1 = 1, theta2 = 0.2), colour = "red")+
  geom_function(fun = function(x)f2_dens(x,theta1 = 1, theta2 = 0.1), colour = "green")+
  labs(x = "x", y = "f(x)")

## Gráficos para variação de theta1:

ggplot()+
  xlim(0,2)+
  ylim(0,6.5)+
  geom_function(fun = function(x)f2_dens(x,theta1 = 2, theta2 = 0.2), colour = "black")+
  geom_function(fun = function(x)f2_dens(x,theta1 = 1, theta2 = 0.2), colour = "red")+
  geom_function(fun = function(x)f2_dens(x,theta1 = 0.5, theta2 = 0.2), colour = "green")+
  labs(x = "x", y = "f(x)")

## Encontrando uma função que cubra completamente a densidade no espaço paramétrico

x <- seq(0,1, by = 0.01)
grid_x <- seq(from = 1, to = 2, by = 0.001)

comp <- ifelse(all(f2_dens(x,theta1 = 1,theta2 = 0.5) < grid_x[1] * dexp(x,f2_dens(0,1,0.5))),1,0)

i <- 1
while(comp == 0){
  i <- i+1
  comp <- ifelse(all(f2_dens(x,theta1 = 1,theta2 = 0.5) < grid_x[i] * dexp(x,f2_dens(0,1,0.5))),1,0)
}

# i será o índice do menor multiplicador
# para que a função dexp cubra completamente f2_dens

M <- grid_x[i]

curve(f2_dens(x,theta1 = 1,theta2 = 0.5), from = 0, to = 1)
curve(M * dexp(x, f2_dens(0,theta1 = 1, theta2 = 0.5)), add = T, col = "purple")

## Amostragem por aceitação-rejeição:
aleatorios_2 <- function(n){
  M <- 1.095
  amostra <- numeric(n)
  i <- 1
  while(i <= n){
    y <- rexp(1, rate = f2_dens(0, theta1 = 1, theta2 = 0.5))
    u <- runif(1)
    razao <- f2_dens(y, theta1 = 1, theta2 = 0.5)/(M * dexp(y, rate = f2_dens(0, theta1 = 1, theta2 = 0.5)))
    if(u < razao){
      amostra[i] <- y
      i <- i + 1
    }
  }
  return(amostra)
}

## Seed para reprodutibilidade:
set.seed(5050)
amostra_2 <- aleatorios_2(10000)

## Gráficos de Comparação por Rejeição-Aceitação:

comp_acum_2 <- as_tibble(amostra_2) %>% 
  ggplot(aes(x = value))+
    stat_ecdf(aes(color = "Empírica"))+
    geom_function(fun = function(x)(x/1)*(1-0.5 * (1-(x/1)))^(-1/0.5), aes(color = "Teórica"))+
    scale_colour_manual(values = c("#000000","#FF1100"))+
    labs(x = "x", y = "Probabilidade Acumulada", color = "Acumuladas")

comp_acum_2

comp_hist_2 <- as_tibble(amostra_2) %>% 
  ggplot(aes(x = value))+
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30)+
  geom_function(fun = function(x)(1/1)*((1-0.5*(1- x/1))^(-1/0.5) -
                         (1^(1/0.5)) * x *(1 - 0.5 * (1-x))^((-1-0.5)/0.5)), colour = "red")+
  lims(x = c(0,1))

comp_hist_2

comp_dens_2 <- comp_hist_2 <- as_tibble(amostra_2) %>% 
  ggplot(aes(x = value))+
  geom_density()+
  geom_function(fun = function(x)(1/1)*((1-0.5*(1- x/1))^(-1/0.5) -
                                          (1^(1/0.5)) * x *(1 - 0.5 * (1-x))^((-1-0.5)/0.5)), colour = "red")+
  lims(x = c(0,1))

comp_dens_2

# Distribuição 3:

f3_acum <- function(x){
  F_3 <- 1 - (x-1)^2
  return(F_3)
}

f3_inv <- function(n){
  u <- runif(n)
  F_inv <- 1 - sqrt(1-u)
  return(F_inv)
}

amostra_3 <- f3_inv(10000)

comp_3 <- as_tibble(amostra_3) %>% 
  ggplot(aes(x = value))+
  stat_ecdf(aes(color = "Empírica"))+
  geom_function(fun = function(x)1 - (x-1)^2, aes(color = "Teórica"))+
  scale_colour_manual(values = c("#000000","#FF1100"))+
  labs(x = "x", y = "Probabilidade Acumulada", color = "Acumuladas")

comp_3

####----Amanda----

F1 <- function(n,theta1,theta2){
  u <- runif(n)
  f <- (log(-log(u))-theta1)/theta2
  return(f)
}

set.seed(123)
amostra <- F1(10000,1,-1)
theta1 <- 1
theta2 <- -1

x11()
plot(ecdf(amostra), xlim = c(-10,10), col = "green", main = "Distribuição Acumulada Empírica")
curve(exp(-exp(theta1 + theta2 * x)), add = T, col = "red")
