#install.packages("nloptr")
#install.packages("mmpf")
#install.packages("randomForest")
#install.packages("reshape2")
#install.packages("forecast")
#install.packages("zoo")
library(forecast)
library(mmpf)
library(randomForest)
library(nloptr)
library(ggplot2)
library(reshape2)
library(quantmod)

rm(list = ls())
set.seed(NULL)

# Carregar dados
bd_cbio <- read.csv("C:/Users/pedro.sampaio/Downloads/Banco de dados GED.csv", head = T, sep = ";", dec = ",")
bd_cbio_main <- na.omit(bd_cbio)
bd_cbio_main$Data <- as.Date(bd_cbio_main$Data, format = "%d/%m/%Y")
prices <- data.frame(Data = bd_cbio_main$Data, Preco.Medio = bd_cbio_main$Preço.Médio)
colnames(prices)[1] <- "Time" # alterando nome da coluna

# Agrupar os dados por mês e calcular a média dos preços
prices_monthly <- aggregate(bd_cbio_main$Preço.Médio, by = list(format(bd_cbio_main$Data, "%Y-%m")), FUN = mean)

# Converter a série temporal em um objeto "ts"
price_ts <- ts(prices_monthly$x, start = c(2020, 6), frequency = 12)

# Ajustar um modelo auto-ARIMA à série temporal
#model <- auto.arima(price_ts)
prices <- data.frame(bd_cbio_main$Preço.Médio)
model <- forecast::Arima(prices, order = c(3, 0, 1), seasonal = list(order = c(0, 1, 0), period = 12), method = "ML")

# Fazer uma previsão de 5 meses
forecast <- forecast(model, h = 5)

# Plotar o gráfico da previsão
plot(forecast, main = "Previsão de Preço Médio para os Próximos 5 Meses (Frequência Mensal)")

# Agrupar os dados por mês e calcular a média dos preços
prices_monthly <- aggregate(bd_cbio_main$Preço.Médio, by = list(format(bd_cbio_main$Data, "%Y-%m")), FUN = mean)

# Converter a série temporal em um objeto "ts"
price_ts <- ts(prices_monthly$x, start = c(2020, 6), frequency = 12)

# Ajustar um modelo auto-ARIMA à série temporal
model <- forecast::Arima(price_ts, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 0), period = 12), method = "ML")

# Calculate the mean of the last period
last_period_mean <- mean(prices$bd_cbio_main.Preço.Médio[nrow(prices)-3:nrow(prices)])

# Fazer uma previsão de 5 meses usando a média do último período
forecast_mean <- forecast::forecast(model, h = 5, xreg = rep(last_period_mean, 5))

# Plotar o gráfico da previsão com a média do último período
plot(forecast_mean, main = "Previsão de Preço Médio para os Próximos 5 Meses com Média do Último Período")

#COMEÇA A OTIMIZAÇÃO
prices <- data.frame(bd_cbio_main$Preço.Médio)

#Convert the prices data frame into a vector
prices_vector = unlist(prices)

#Apply the lag function to the vector
plag = lag(prices_vector)

#Remove the first element of the vector
plag = plag[-1]
prices = prices[-1,]
datas = data.frame(bd_cbio_main$Data)
datas = datas[-1,]

ret = ROC(prices, type="discrete")
ret[is.na(ret)] = 0
horizon <- 5   # horizonte em meses
janela <- 22
pini <- 68
pfim <- 103
mediaRet <- (pfim/pini)^(1/(horizon*janela))-1 #movimento Browniano da geração de cenários
#mediaRet <- 0.0005
#mediaRet <- mean(ret)
desvpadRet <- sd(ret) #Muito alto o valor
desvpadRet <- 0.0001
tendencia = (102/90)^(1/110)-1 #102 é a minhra previsão de preço para os próixmos 5 meses
                                #90 é o meu preço inicial
                                #tendencia é o incremento diário no preço




# Define janela, mediaRetJan, and desvpadRetJan
mediaRetJan <- (1+mediaRet)^janela - 1
desvpadRetJan <- desvpadRet * janela^0.5

MC_iter <- 100   #número de iterações do loop

K <- 27887    # define o orçamento máximo disponível.
norm_factors <- runif(horizon, 2323.92, 2559.92)    # gera um vetor de fatores de normalização aleatórios, um para cada período de tempo, com valores entre 2323.92 e 2559.92.
#norm_factors <- c(rep(K/horizon,horizon))

set.seed(0)

precosGerados <- matrix(0,MC_iter, horizon)
precosGerados[1:MC_iter,1] <- pini * exp(mediaRetJan -desvpadRetJan^2/2+ rnorm(1, mean=0, sd = desvpadRetJan))  # atribui o valor à primeira coluna (representando o primeiro mês) de todas as linhas da matriz

# objective function
eval_f0 <- function(quantities, prices) {
  # call the eval_f0 function with the current quantities from the loop
  return(sum(quantities*prices))
}

# constraint function
eval_g0 <- function(quantities,K){
  return(K-sum(quantities))
}
# define parameters
#x0 <- c(200,200, 100, 200, 200, 100)  # initial values for optimization variables
#lb <- c(0,0,0,0,0,0)                  # lower bounds for optimization variables
#ub <- c(300,300, 300,300,300,300)     # upper bounds for optimization variables
#K <- 1000                             # maximum budget

# define parameters
lb <- rep(0, horizon)    # define os limites inferiores para cada período de tempo. Nesse caso, o limite inferior é zero para todos os períodos
ub <- rep(K/horizon*3, horizon)    # define os limites superiores para cada período de tempo. Nesse caso, o limite superior é o orçamento máximo para todos os períodos.
x0 <- norm_factors / sum(norm_factors) * K    # calcula o vetor inicial de valores de decisão para o problema de otimização. Os valores são calculados dividindo cada fator de normalização pela soma de todos os fatores e multiplicando o resultado pelo orçamento máximo disponível.
print("norm_factors")
print(norm_factors)

quantidades <- data.frame()
custos <- data.frame()

for (i in 1:MC_iter){
  for (j in 2: horizon){
    precosGerados[i,j] <- precosGerados[i,j-1] * exp(mediaRetJan + rnorm(1, mean=0, sd = desvpadRetJan))
  }
  
  pricesAux <- precosGerados[i,]
  #print(pricesAux)
  
  
  # Solve using NLOPT_LN_COBYLA without gradient information
  res1 <- nloptr(x0 = x0,
                 eval_f = eval_f0,
                 lb = lb,
                 ub = ub,
                 eval_g_ineq = function(quantities, prices) eval_g0(quantities, K),  #if does exist a constraint function
                 opts = list("algorithm" = "NLOPT_LN_COBYLA",    #Local solver for the derivative-free approach
                             "xtol_rel" = 1e-04),
                 prices=pricesAux)      #stopping criterion for relative change reached
  
  solution <- res1$solution
  
  
  quantidades <- rbind(quantidades, solution)
  custos <- rbind(custos,res1$objective)
  
}
#custos da otimização
colnames(custos) <- c("Custo")
print("Custo da otimização")
print(mean(custos$Custo))
finalPrices <- data.frame(Precos=precosGerados[,horizon])



ggplot(custos, aes(x=Custo))+geom_histogram()

ggplot(finalPrices,aes(x=Precos))+geom_histogram()

#estratégia manual
norm_factorsManual <- c(rep(K/horizon,horizon))
quantidadesEstrat <- norm_factorsManual
custosEstrat = quantidadesEstrat %*%t(precosGerados)
print("Custo da estratégia manual")
print(mean(custosEstrat))

#média das colunas
mediaPrecos <- colMeans(precosGerados, dim = 1)
mediaCustos <- colMeans(custos, dim = 1)
mediaAloc <- colMeans(quantidades, dim = 1)

print("quantidades compradas")
print(mediaAloc)

plot(mediaPrecos)

# Aplica a transformação logarítmica aos dados da média de preços
logMediaPrecos <- log(mediaPrecos)

# Plota o gráfico da média de preços transformada
plot(logMediaPrecos)

