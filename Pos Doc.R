#rotina otimização de carteira
#Autor: Pedro &amp; Vivi
#data 23/09/2020
#entrada de dados: Preços Mexico
#Pacotes
# install.packages(&quot;quadprog&quot;)
# install.packages(&quot;PerformanceAnalytics&quot;)
# install.packages(&quot;IntroCompFinR&quot;, repos=&quot;http://R-Forge.R-project.org&quot;)
# install.packages(&#39;mnormt&#39;)
# install.packages(&#39;fAssets&#39;)
# install.packages(&quot;fPortfolio&quot;)
# install.packages(&quot;tidyquant&quot;)
# install.packages(&quot;dplyr&quot;)
# install.packages(&#39;DEoptmin&#39;)
# install.packages(&#39;ROI.plugin.glpk&#39;)
# install.packages(&#39;ROI.plugin.quadprog&#39;)
# install.packages(&#39;mvtnorm&#39;)
# install.packages(&quot;PortfolioAnalytics&quot;, repos=&quot;http://R-Forge.R-project.org&quot;)
#Biblioteca
library(IntroCompFinR)
library(tidyquant)
library(quadprog)
library(dplyr)
library(stringr)
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)
library(mvtnorm)
library(ggcorrplot)
#library(fAssets)
#library(fPortfolio)
#Leitura de dados
#Limpa as variaveis do ambiente
rm(list = ls())
#Carrega a base de dados
#Carrega a base de dados
entBD = read.csv(&quot;C:/Users/User/Desktop/Article/dbvivianev4.csv&quot;, head = T,
                 sep = &quot;;&quot;, dec = &quot;,&quot;)
tickers =c(&quot;AUS3&quot;,&quot;CAD3&quot;, &quot;CHI3&quot;, &quot;FRA3&quot;, &quot;GER3&quot;, &quot;GOLJ&quot;, &quot;JAP3&quot;, &quot;SWI3&quot;,
           &quot;UK3&quot;,&quot;USA3&quot;)
# tickers =c(&quot;AUS2&quot;,&quot;CAD2&quot;, &quot;CHI2&quot;, &quot;FRA2&quot;, &quot;GER2&quot;, &quot;GOLJ&quot;, &quot;JAP2&quot;, &quot;SWI2&quot;,
&quot;UK2&quot;,&quot;USA2&quot;)
# tickers =c(&quot;AUS5&quot;,&quot;CAD5&quot;, &quot;CHI5&quot;, &quot;FRA5&quot;, &quot;GER5&quot;, &quot;GOLJ&quot;, &quot;JAP5&quot;, &quot;SWI5&quot;,
&quot;UK5&quot;,&quot;USA5&quot;)

moedas =c(&quot;AUDUSD&quot;,&quot;CADUSD&quot;, &quot;CNYUSD&quot;, &quot;EURUSD&quot;, &quot;EURUSD&quot;,
          &quot;GOL&quot;, &quot;JPYUSD&quot;, &quot;CHFUSD&quot;, &quot;GBPUSD&quot;,&quot;USDUSD&quot;)
#CASO BASE MEXICO
minAloc = c(0.00,0.01,0.01,0.00,0.00,0.01,0.01,0.00,0.00,0.90)
maxAloc = c(0.01,0.01,0.01,0.00,0.00,0.01,0.01,0.00,0.00,0.99)
#CASO BASE BRASIL
minAloc = c(0.00,0.00,0.00,0.04,0.04,0.01,0.02,0.00,0.02,0.84)
maxAloc = c(0.00,0.00,0.00,0.04,0.04,0.01,0.02,0.00,0.02,0.91)
#CASO BASE PERU
minAloc = c(0.00,0.00,0.00,0.00,0.00,0.04,0.00,0.00,0.00,0.84)
maxAloc = c(0.00,0.00,0.00,0.00,0.00,0.05,0.00,0.00,0.00,0.98)
#CASO BASE CHILE
minAloc = c(0.08,0.07,0.00,0.05,0.05,0.00,0.00,0.00,0.00,0.50)
maxAloc = c(0.1,0.10,0.00,0.06,0.06,0.00,0.00,0.00,0.00,0.68)
#CASO BASE COSTA RICA
minAloc = c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.94)
maxAloc = c(0.00,0.00,0.00,0.01,0.01,0.00,0.00,0.00,0.00,1.00)
#CASO BASE JAMAICA
minAloc = c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.01,0.89)
maxAloc = c(0.00,0.00,0.01,0.01,0.01,0.00,0.00,0.00,0.01,1.00)
#SIM 1 CHILE
# minAloc = c(0.0479,0.0446,0.00,0.0328,0.0328,0.00,0.00,0.00,0.00,0.47)
# maxAloc = c(0.1079,0.1046,0.03,0.0628,0.0628,0.03,0.03,0.03,0.03,0.53)
#SIM 2 CHILE
minAloc = c(0.08,0.07,0.00,0.05,0.05,0.00,0.00,0.00,0.00,0.50)
maxAloc = c(0.11,0.10,0.01,0.07,0.07,0.01,0.01,0.01,0.01,0.69)
#SIM 3 CHILE
minAloc = c(0.08,0.07,0.00,0.05,0.05,0.00,0.00,0.00,0.00,0.50)
maxAloc = c(0.12,0.11,0.02,0.07,0.07,0.01,0.01,0.01,0.01,0.69)
# SIM 1 MEXICO
minAloc = c(0.00,0.01,0.01,0.00,0.00,0.01,0.01,0.00,0.00,0.90)
maxAloc = c(0.01,0.02,0.02,0.02,0.02,0.02,0.02,0.01,0.01,0.99)
# SIM 1 BRASIL
minAloc = c(0.00,0.00,0.00,0.04,0.04,0.01,0.02,0.00,0.02,0.84)
maxAloc = c(0.01,0.01,0.01,0.04,0.04,0.02,0.03,0.01,0.03,0.91)
# SIM 1 PERU
minAloc = c(0.00,0.00,0.00,0.00,0.00,0.04,0.00,0.00,0.00,0.84)
maxAloc = c(0.01,0.01,0.01,0.01,0.01,0.05,0.01,0.01,0.01,0.98)
# SIM 1 COSTA RICA
minAloc = c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.94)
maxAloc = c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,1.00)
# SIM 1 JAMAICA
minAloc = c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.01,0.89)
maxAloc = c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,1.00)

#minAloc = c(0.0, 0.00,0.00,0.00,0.0,0.00)
#maxAloc = c(1,1,1,1,1,1)
#Numero de ativos com comando length
nativos = length(tickers)
#========================================================
#Filtrar na Base de Dados os tickers
ret_juros = entBD %&gt;% select(tickers[1])
forex = entBD %&gt;% select(moedas[1])
nlinhas = nrow(ret_juros)
for (i in 2:nativos) {
  ret_jurosAux = entBD %&gt;% select(tickers[i])
  ret_juros = bind_cols(ret_juros, ret_jurosAux)
  
  forexAux = entBD %&gt;% select(moedas[i])
  forex = bind_cols(forex, forexAux)
}
#ret_juros[!is.na(ret_juros)]

retornos = matrix(0,nlinhas,nativos)
for (i in 1: nlinhas){
  for(j in 1:nativos){
    retornos[i,j] = as.numeric(ret_juros[i,j]) - as.numeric(forex[i,j])
  }
}
#========================Fim do filtro de
ativos======================
  
  #Permite a selecao de usar apenas parte dos dados
  janEst = 60

#Calcula media dos retornos
medRet = matrix(0,nativos,1)
#medRet_ForaJan = matrix(0,nativos,1)
for (i in 1:nativos){
  medRet[i,1] = mean(retornos[1:janEst,i])
  #  medRet_ForaJan[i,1] = mean(retornos[janEst:nlinhas,i])
}
rownames(medRet) = tickers
#Calcula matriz covariancia retornos historicos
covmatrix = cov(retornos[1:janEst,1:nativos])
#covmatrix_ForaJan = cov(retornos[janEst:nlinhas,1:nativos])

#--------Inicio da otimização------------------------------------------------------
#==================IntroCompFinR
# short_selling = FALSE
# carteira_min_risco &lt;- globalMin.portfolio(medRet, covmatrix, shorts =
short_selling)
# carteira_min_risco
# 
# fronteira = efficient.frontier(medRet, covmatrix, nport = 40, shorts =
short_selling)
# fronteira
# attributes(fronteira)
# 
# # Visualizaçao da saida
# 
# plot(fronteira, plot.assets=TRUE, col=&quot;blue&quot;, pch=16)
# 
# target.return = 0.7 * max(medRet)
# 
# #target.risk = 0.05834095
# 
# a = efficient.portfolio(medRet,covmatrix, target.return, shorts = FALSE)
# 
# 
# 
#==================
#Usando quadprog
# A.Equality =  matrix(1,nrow=nativos,ncol=1)
# Amat &lt;- cbind(A.Equality, medRet, diag(nativos), -diag(nativos))
# bvec &lt;- c(1, target.return, rep(0, nativos), rep(-1, nativos))
# qp &lt;- solve.QP(covmatrix, medRet, Amat, bvec, meq=2)
# qp$solution
# 
# portReturn = qp$solution %*% medRet
# portRisk = (qp$solution %*% covmatrix %*% qp$solution)^0.5
# 
# show(&#39;Port Return&#39;)
# show(portReturn)
# show(&#39;portRisk&#39;)
# show(portRisk)
# 
covmatrixOrder = covmatrix[order(tickers), order(tickers)]
#covmatrixOrder = covmatrix
medRetOrder = medRet[order(tickers)]
chaveReAmost=TRUE

#===========================================================
========
  #Usando Resampling
  if (chaveReAmost==TRUE){
    #semente das simulacoes
    set.seed(0)
    #numero de iteracoes de Monte Carlo
    MC_iter = 100
    #numero de portfolios para a fronteira eficiente
    numPort = 1
    #tamanho da amostra para o procedimento de reamostragem
    #tamAmostra = 50
    tamAmostra = 100
    #restricoes de maximo e minimo na otimizacao
    #minAloc =0.0
    #maxAloc = 1
    #grau maximo de aversao a risco
    maxAversaoRisco = 1
    
    rs_pesos = data.frame()
    
    #Cria portfolio para a biblioteca PortfolioAnalytics
    #Cria portfolio e define nome dos ativos
    rs_nomes.ativos = tickers
    #Define o portfolio
    rs_pspec = portfolio.spec(assets=rs_nomes.ativos)
    
    #Adiciona restrições
    rs_pspec = add.constraint(portfolio = rs_pspec, type =
                                &quot;weight_sum&quot;,min_sum=1, max_sum =1)
    rs_pspec = add.constraint(portfolio = rs_pspec, type = &quot;box&quot;, min=minAloc,
                              max = maxAloc)
    
    #rs_pspec = add.constraint(portfolio = rs_pspec,type=&quot;group&quot;,
    groups=list(groupA=assetsClassGroupA), group_min = c(0),
    group_max=c(0.05))

#Inicializa resampling

#Vetor de aversao a risco
rAversion = matrix(0, numPort, 1)
for (j in 1:numPort){
  rAversion[j,1] = j*maxAversaoRisco /numPort
}

#Reamostragem
for (i in 1: MC_iter){
  
  #Distribuicao normal multivariada
  mRetSC = rmvnorm(tamAmostra, medRet, covmatrix)
  
  mc_R = ts(mRetSC)
  
  for (k in 1:numPort)
  {
    rs_qu = list()
    #Maximiza utilidade (quadratica) usando pacote ROI
    rs_qu = add.objective(portfolio=rs_pspec, type = &quot;return&quot;, name=&quot;mean&quot;)
    rs_qu = add.objective(portfolio = rs_qu, type = &quot;risk&quot;, name=&quot;var&quot;,
                          risk_aversion = rAversion[k,1])
    
    rs_opt_qu = optimize.portfolio(R=mc_R, portfolio = rs_qu,
                                   optimize_method = &quot;ROI&quot;,
                                   trace=TRUE)
    rs_pesos = rbind(rs_pesos, data.frame(iter=i, risk_aver=rAversion[k,1],
                                          nomes= rs_nomes.ativos, pesos=rs_opt_qu$weights))
  }
}
numiter = MC_iter*numPort

df_frontMC2 = rs_pesos %&gt;% group_by(iter,risk_aver) %&gt;%
  summarize(risco=pesos%*%covmatrix%*%pesos,
            retorno=pesos%*%medRet, ativos = rs_nomes.ativos,pesos = pesos) %&gt;%
  ungroup()

df_RSoutESG2 = df_frontMC2

df_outMed = df_RSoutESG2 %&gt;% group_by(risk_aver, ativos) %&gt;%
  summarise(pesos = mean (pesos,na.rm = TRUE)) %&gt;% ungroup()

dfRet = data.frame(ativo=rs_nomes.ativos, retmedio=medRet)



saida = df_outMed %&gt;% group_by(risk_aver) %&gt;%
  summarize(risco=(pesos%*%covmatrixOrder%*%pesos)^(1/2),
            retorno=dfRet$retmedio %*% pesos)
saida = saida[-c(1)]

sdaretornos = df_outMed %&gt;% group_by(risk_aver) %&gt;%
  summarize(portretornos = retornos %*% pesos  )
  }
#---------------------------Fim do processo de otimizacao
#Plota fronteira eficiente
reg2 = lm(saida$retorno ~ saida$risco + I(saida$risco^2))

retmod = reg2$coefficients[1] + reg2$coefficients[2] * saida$risco +
  reg2$coefficients[3] * saida$risco^2
# plot(saida$risco,retmod, type=&quot;l&quot;, col=&quot;blue&quot;,xlab=&quot;Risk&quot;, ylab=&quot;Return&quot;)
# lines(saida$risco,retmod, type=&quot;p&quot;, col=&quot;blue&quot;)
# title(main =&quot;Risk Return Inside Sample&quot;)
#--------------------------------------------
#Seleciona um portfolio da fronteira eficiente com base no grau de aversao a
risco
sdaretornos1 = sdaretornos %&gt;% filter(risk_aver==1) %&gt;% select(portretornos)
correlMXN=cor(sdaretornos1$portretornos,as.numeric(entBD$MXNUSD))
show(&#39;Correlação MXN&#39;)
       show(correlMXN)
     #SOJA
     correlSOJA = cor(sdaretornos1$portretornos,as.numeric(entBD$SOJ))
     show(&#39;Correlação Soja&#39;)
            show(correlSOJA)
          #média dos retornos
          portretmed = mean(sdaretornos1$portretornos)
          show(&#39;portretmed&#39;)
                 show(portretmed)
               #DesvPad dos retornos
               portdesvpad = sd(sdaretornos1$portretornos)
               show(&#39;portdesvpad&#39;)
                      show(portdesvpad)
                    df_retAtivos = data.frame(retornos)
                    colnames(df_retAtivos) = tickers
                    df_retAtivos$SOJ = as.numeric(entBD$SOJ)
                    #ALUMINIO
                    correlALU = cor(sdaretornos1$portretornos,as.numeric(entBD$ALU))
                    show(&#39;Correlação Aluminio&#39;)
                           show(correlALU)
                         #média dos retornos
                         portretmed = mean(sdaretornos1$portretornos)
                         show(&#39;portretmed&#39;)
                                show(portretmed)
                              #DesvPad dos retornos
                              portdesvpad = sd(sdaretornos1$portretornos)
                              show(&#39;portdesvpad&#39;)
                                     show(portdesvpad)
                                   
                                   df_retAtivos = data.frame(retornos)
                                   colnames(df_retAtivos) = tickers
                                   df_retAtivos$ALU = as.numeric(entBD$ALU)
                                   #COBRE
                                   correlCOB = cor(sdaretornos1$portretornos,as.numeric(entBD$COB))
                                   show(&#39;Correlação Cobre&#39;)
                                          show(correlCOB)
                                        #média dos retornos
                                        portretmed = mean(sdaretornos1$portretornos)
                                        show(&#39;portretmed&#39;)
                                               show(portretmed)
                                             #DesvPad dos retornos
                                             portdesvpad = sd(sdaretornos1$portretornos)
                                             show(&#39;portdesvpad&#39;)
                                                    show(portdesvpad)
                                                  df_retAtivos = data.frame(retornos)
                                                  colnames(df_retAtivos) = tickers
                                                  df_retAtivos$COB = as.numeric(entBD$COB)
                                                  #PETROLEUM
                                                  correlPET = cor(sdaretornos1$portretornos,as.numeric(entBD$PET))
                                                  show(&#39;Correlação Petroleum&#39;)
                                                         show(correlPET)
                                                       #média dos retornos
                                                       portretmed = mean(sdaretornos1$portretornos)
                                                       show(&#39;portretmed&#39;)
                                                              show(portretmed)
                                                            #DesvPad dos retornos
                                                            portdesvpad = sd(sdaretornos1$portretornos)
                                                            show(&#39;portdesvpad&#39;)
                                                                   show(portdesvpad)
                                                                 df_retAtivos = data.frame(retornos)
                                                                 colnames(df_retAtivos) = tickers
                                                                 df_retAtivos$PET = as.numeric(entBD$PET)
                                                                 # correlUSD=cor(sdaretornos1$portretornos,as.numeric(entBD$USDUSD))
                                                                 # show(&#39;Correlação USD&#39;)
                                                                 # show(correlUSD)
                                                                 #SOJA
                                                                 correlSOJA = cor(sdaretornos1$portretornos,as.numeric(entBD$SOJ))
                                                                 show(&#39;Correlação Soja&#39;)
                                                                        
                                                                        show(correlSOJA)
                                                                      #média dos retornos
                                                                      portretmed = mean(sdaretornos1$portretornos)
                                                                      show(&#39;portretmed&#39;)
                                                                             show(portretmed)
                                                                           #DesvPad dos retornos
                                                                           portdesvpad = sd(sdaretornos1$portretornos)
                                                                           show(&#39;portdesvpad&#39;)
                                                                                  show(portdesvpad)
                                                                                df_retAtivos = data.frame(retornos)
                                                                                colnames(df_retAtivos) = tickers
                                                                                df_retAtivos$SOJ = as.numeric(entBD$SOJ)
                                                                                #ALUMINIO
                                                                                correlALU = cor(sdaretornos1$portretornos,as.numeric(entBD$ALU))
                                                                                show(&#39;Correlação Aluminio&#39;)
                                                                                       show(correlALU)
                                                                                     #média dos retornos
                                                                                     portretmed = mean(sdaretornos1$portretornos)
                                                                                     show(&#39;portretmed&#39;)
                                                                                            show(portretmed)
                                                                                          #DesvPad dos retornos
                                                                                          portdesvpad = sd(sdaretornos1$portretornos)
                                                                                          show(&#39;portdesvpad&#39;)
                                                                                                 show(portdesvpad)
                                                                                               df_retAtivos = data.frame(retornos)
                                                                                               colnames(df_retAtivos) = tickers
                                                                                               df_retAtivos$ALU = as.numeric(entBD$ALU)
                                                                                               #COBRE
                                                                                               correlCOB = cor(sdaretornos1$portretornos,as.numeric(entBD$COB))
                                                                                               show(&#39;Correlação Cobre&#39;)
                                                                                                      show(correlCOB)
                                                                                                    #média dos retornos
                                                                                                    portretmed = mean(sdaretornos1$portretornos)
                                                                                                    show(&#39;portretmed&#39;)
                                                                                                           show(portretmed)
                                                                                                         #DesvPad dos retornos
                                                                                                         portdesvpad = sd(sdaretornos1$portretornos)
                                                                                                         show(&#39;portdesvpad&#39;)
                                                                                                                show(portdesvpad)
                                                                                                              
                                                                                                              df_retAtivos = data.frame(retornos)
                                                                                                              colnames(df_retAtivos) = tickers
                                                                                                              df_retAtivos$COB = as.numeric(entBD$COB)
                                                                                                              #PETROLEUM
                                                                                                              correlPET = cor(sdaretornos1$portretornos,as.numeric(entBD$PET))
                                                                                                              show(&#39;Correlação Petroleum&#39;)
                                                                                                                     show(correlPET)
                                                                                                                   #média dos retornos
                                                                                                                   portretmed = mean(sdaretornos1$portretornos)
                                                                                                                   show(&#39;portretmed&#39;)
                                                                                                                          show(portretmed)
                                                                                                                        #DesvPad dos retornos
                                                                                                                        portdesvpad = sd(sdaretornos1$portretornos)
                                                                                                                        show(&#39;portdesvpad&#39;)
                                                                                                                               show(portdesvpad)
                                                                                                                             df_retAtivos = data.frame(retornos)
                                                                                                                             colnames(df_retAtivos) = tickers
                                                                                                                             df_retAtivos$PET = as.numeric(entBD$PET)
                                                                                                                             df_retAtivos$COB = as.numeric(entBD$COB)
                                                                                                                             cor.res = round(cor(df_retAtivos),3)
                                                                                                                             g_corr = ggcorrplot(cor.res, type=&quot;lower&quot;, colors = c(&quot;red&quot;,&quot;white&quot;,&quot;blue&quot;),
                                                                                                                                                 hc.order=TRUE,lab=TRUE, lab_size=4,
                                                                                                                                                 title=&quot;Matriz de correlação&quot;, ggtheme = theme_bw)
                                                                                                                             print(g_corr)
                                                                                                                             df_corCart =
                                                                                                                               data.frame(sdaretornos1$portretornos,as.numeric(entBD$ALU),as.numeric(ent
                                                                                                                                                                                                     BD$COB),as.numeric(entBD$PET),as.numeric(entBD$SOJ),as.numeric(entBD
                                                                                                                                                                                                                                                                    $GOL))
                                                                                                                             colnames(df_corCart) = c(&quot;Retornos&quot;,&quot;ALU&quot;,&quot;COB&quot;,&quot;PET&quot;,&quot;SOJ&quot;,&quot;GOL&quot;)
                                                                                                                             cor.res1= round(cor(df_corCart),3)
                                                                                                                             g_corr1 = ggcorrplot(cor.res1, type=&quot;lower&quot;, colors = c(&quot;red&quot;,&quot;white&quot;,&quot;blue&quot;),
                                                                                                                                                  hc.order=TRUE,lab=TRUE, lab_size=4,
                                                                                                                                                  title=&quot;Matriz de correlação&quot;, ggtheme = theme_bw)
                                                                                                                             print(g_corr1)
                                                                                                                             
                                                                                                                             write.csv(df_outMed, &quot;C:/Users/user/Desktop/Article/pesos.csv&quot;)