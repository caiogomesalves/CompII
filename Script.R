####----Trabalho 1----

# Distribuição 1:
## Função densidade de probabilidade:

f1 <- function(x){
  if(x <= pi & x >= 0){
    return(0.5 * sin(x))
  }
  else{
    return(0)
  }
}  

## Função densidade acumulada

f1_acum <- function(x){
  if(x >= 0 & x <= pi){
    x <- (1 - cos(x))/2
    return(x)
  }
  if(x > pi){
    return(1)
  }
  else{
    return(0)
  }
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
plot(ecdf(amostra_1), xlim = c(0,pi), main = "Distribuição Empírica x Teórica")
curve((1 - cos(x))/2, add = TRUE, col = "red")
legend("rigth",c("Empírica","Teórica"))

comp_1 <- as_tibble(amostra_1) %>% 
  ggplot(aes(x = value))+
    stat_ecdf(aes(color = "Empírica"))+
    geom_function(fun = function(x) (1 - cos(x))/2, aes(color = "Teórica"))+
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

curve(f2_dens(x,theta1 = 1,theta2 = 0.5), from = 0, to = 1)
curve(f2_dens(x,theta1 = 1,theta2 = 0.2), add = T, col = "red")
curve(f2_dens(x,theta1 = 1,theta2 = 0.1), add = T, col = "green")

## Gráficos para variação de theta1:

curve(f2_dens(x,theta1 = 2,theta2 = 0.2), from = 0, to = 2, ylim = c(0,5))
curve(f2_dens(x,theta1 = 1,theta2 = 0.2), from = 0, to = 1, add = T, col = "red")
curve(f2_dens(x,theta1 = 0.5,theta2 = 0.2), from = 0, to = 0.5, add = T, col = "green")

## Encontrando uma função que cubra completamente a densidade no espaço paramétrico

curve(f2_dens(x,theta1 = 1,theta2 = 0.5), from = 0, to = 1)
curve(1.2 * dexp(x, f2_dens(0,theta1 = 1, theta2 = 0.5)), add = T, col = "purple")

## Amostragem por aceitação-rejeição:
aleatorios_2 <- function(n){
  M <- 1.2
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

plot(ecdf(amostra), xlim = c(-10,10), col = "green", main = "Distribuição Acumulada Empírica")
curve(exp(-exp(theta1 + theta2 * x)), add = T, col = "red")
