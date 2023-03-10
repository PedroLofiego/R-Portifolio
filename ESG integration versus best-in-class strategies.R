#rotina otimização de carteira
#Autor: Pedro
#data 23/09/2020
#entrada de dados: Preços

#Pacotes
#install.packages("quadprog")
# install.packages("PerformanceAnalytics")
# install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
# install.packages('mnormt')
# install.packages('fAssets')
# install.packages("fPortfolio")
#install.packages("tidyquant")
#install.packages("dplyr")
# install.packages('DEoptmin')
# install.packages('ROI.plugin.glpk')
# install.packages('ROI.plugin.quadprog')
# install.packages('mvtnorm')
#install.packages("PortfolioAnalytics", repos="http://R-Forge.R-project.org")

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


#library(fAssets)
#library(fPortfolio)
#Leitura de dados
rm(list = ls())

runBiC = TRUE
#selecBDInd = 1 (ISE); selecBDInd = 2 (CO2); selecBDInd = 1 (ISE+ICO2)
selecBDInd = 1
#=====*************************************************************************
maxESGScore2 = 1
maxEsGScore3 = 0.4
#====================5=========================================================
#Define janela de dados para estimar retornos esperados e matriz de covariância
#Janela definida como uma fração dos dados totais
PrimPartJanela = 0.995
#restricoes de maximo e minimo na otimizacao
minAloc =0.0
maxAloc = 0.1
#=============================================================================

# tickers = c("WEGE3", "PETR4", "BRFS3", "SUZB3", "MGLU3", "VALE3",
#            "BRKM5")

tickers = c("ABEV3","B3SA3","BBAS3","BBDC3","BBSE3","BPAC11",
            "BRAP4","BRDT3","BRFS3","BRKM5","BRML3","BTOW3","CCRO3","CIEL3","CMIG4",
            "COGN3","CPFE3","CRFB3","CSAN3","EGIE3",
            #     "ELET3","EMBR3","ENGI11","EQTL3","GGBR4",
            "ELET3","EMBR3", "EQTL3","GGBR4",
            "GNDI3","GOAU4","HYPE3","IRBR3","ITSA4",
            "ITUB4","JBSS3","KLBN11","LAME4","LREN3","MGLU3","MULT3",
            "PCAR3","PETR3","RADL3","RAIL3","RENT3",
            "SANB11","SBSP3","SULA11","SUZB3","UGPA3",
            "VALE3","WEGE3")


nativosYahoo = length(tickers)

tickers_SA = tickers

for (i in 1:nativosYahoo) {
  tickers_SA[i] = str_c(tickers_SA[i], ".SA")
}

data_ini = "2019-01-01"
data_end = "2020-12-31"

getYahoo = FALSE

if(getYahoo==TRUE){
  
  getSymbols("^BVSP",
             from = data_ini,
             to = data_end)
  pricesIbov <- tq_get("^BVSP",
                       from = data_ini,
                       to = data_end,
                       get = "stock.prices")
  
  getSymbols(tickers_SA,
             from = data_ini,
             to = data_end)
  pricesYahoo <- tq_get(tickers_SA,
                        from = data_ini,
                        to = data_end,
                        get = "stock.prices")
  
  #SALVE DATAFRAME em R
  #pricesYahoo SAVE
  save(pricesYahoo,file="C:\\Afranc\\Prospectos\\PIBICPIBITI\\2020\\pricesYahoo.Rda")
  
  save(pricesIbov,file="C:\\Afranc\\Prospectos\\PIBICPIBITI\\2020\\pricesIbov.Rda")
  
} else {
  load("C:\\Afranc\\Prospectos\\PIBICPIBITI\\2020\\pricesYahoo.Rda")
  load("C:\\Afranc\\Prospectos\\PIBICPIBITI\\2020\\pricesIbov.Rda")
}

#pricesYahoo LOAD


pricesYahoo$date = as.Date(pricesYahoo$date,"%d/%m/%Y")
pricesYahooTab = pricesYahoo %>% filter(pricesYahoo$symbol == tickers_SA[1]) %>% select(date, adjusted)

for (i in 2:nativosYahoo) {
  pricesYahooTabAux = pricesYahoo %>% filter(pricesYahoo$symbol == tickers_SA[i]) %>% select(adjusted)
  pricesYahooTab = bind_cols(pricesYahooTab, pricesYahooTabAux)
}

nchangenames = nativosYahoo + 1
names(pricesYahooTab)[2:nchangenames] = tickers
pricesYahooTab = na.omit(pricesYahooTab)

pricesIbov$date = as.Date(pricesIbov$date,"%d/%m/%Y")
nlinhas1 = length(pricesYahooTab$date)
pricesIbov1 = pricesIbov %>% filter(pricesIbov$date == pricesYahooTab$date[1]) %>% select(date, adjusted)
for (i in 2:nlinhas1){
  pricesIbov1aux = pricesIbov %>% filter(pricesIbov$date == pricesYahooTab$date[i]) %>% select(date, adjusted)
  pricesIbov1 = bind_rows(pricesIbov1, pricesIbov1aux)
}

#entPrecos = read.csv("C:\\Afranc\\Prospectos\\PIBICPIBITI\\2020\\DadosTeste.csv", head = T, sep <- ";", dec = ",")
#entPrecos = read.csv("D:\\Precos.csv", head = T, sep <- ";", dec = ",")

entBD_Indices = read.csv("C:\\Afranc\\Prospectos\\PIBICPIBITI\\2020\\BD_IndicesV3.csv", head = T, sep <- ";", dec = ",")
entIndTickers = entBD_Indices %>% filter(entBD_Indices$Codigo == tickers[1])

for (i in 2:nativosYahoo) {
  entIndTickersAux = entBD_Indices %>% filter(entBD_Indices$Codigo == tickers[i])
  entIndTickers = bind_rows(entIndTickers, entIndTickersAux)
}

scorSust = as.numeric(entIndTickers$ScoreSustainalytics)
maxSustainalytics = max(scorSust)
minSustainalytics = min(scorSust)
#maxSustainalytics = 10
#minSustainalytics = 0
rangeScoreSust = maxSustainalytics - minSustainalytics

esg_score = matrix(0,nativosYahoo, 1)
sust_score = matrix(0,nativosYahoo, 1)
for (i in 1:nativosYahoo) {
  #  esg_score[i] = (entIndTickers$ICO2[i]+entIndTickers$ISE[i])/2
  # normalização
  sust_score[i] = (as.numeric(entIndTickers$ScoreSustainalytics[i])-minSustainalytics)/rangeScoreSust
}
esg_score = sust_score

entPrecos = pricesYahooTab

#Inicializa matriz
nlinhas = length(entPrecos$date)-1
#nlinhas = length(entPrecos$ï..Data)-1
#nlinhas = length(entPrecos$Data)-1
nativos = length(entPrecos)-1

if (selecBDInd == 1){
  entIndTickersBiC = entBD_Indices %>% filter(entBD_Indices$ISE == 1)
} else{
  if (selecBDInd == 2){
    entIndTickersBiC = entBD_Indices %>% filter(entBD_Indices$ICO2 == 1)
  } else{
    entIndTickersBiC = entBD_Indices %>% filter(entBD_Indices$ISE == 1 | entBD_Indices$ICO2 == 1)
  }
}

num_ind = length(entIndTickersBiC$Codigo)

tickersBiC = c('')
for (i in 1:num_ind){
  for (k in 1 : nativos){
    if ( entIndTickersBiC$Codigo[i] == tickers[k]){
      tickersBiC = c(tickersBiC,tickers[k])
    }
  }
}
tickersBiC = tickersBiC[-c(1)]
nativosBiC = length(tickersBiC)

esg_scoreBiC = esg_score
rownames(esg_scoreBiC) = tickers
esg_scoreBiC = esg_scoreBiC[tickersBiC,1]

janEst = floor(PrimPartJanela * nlinhas)
#==============================================================================

#Calcula retornos

retornos = matrix(0,nlinhas, nativos)

df_precos = entPrecos[-c(1)]
precos = as.matrix(df_precos)

#Calcula retornos
for (i in 1:nlinhas) {
  aux = i+1
  for (j in 1:nativos){
    retornos[i,j] = as.numeric(precos[aux,j]) / as.numeric(precos[i,j]) - 1
  }
}

retornosBiC = retornos
colnames(retornosBiC) = tickers
retornosBiC = retornosBiC[,tickersBiC]

retIbov = matrix(0,nlinhas,1)
df_ibov = pricesIbov1[-c(1)]
ibov_val = as.matrix(df_ibov)

for (i in 1:nlinhas) {
  aux = i+1
  retIbov[i] = as.numeric(ibov_val[aux]) / as.numeric(ibov_val[i]) - 1
}


x = retIbov[1:janEst,1]

coef_si = matrix(0,nativos,2)
resid_si = matrix(0,janEst,nativos)

for (i in 1:nativos){
  y = retornos[1:janEst,i]
  df_reg = bind_cols(as.data.frame(x), as.data.frame(y))
  r_si = lm(df_reg$y ~ df_reg$x)
  coef_si[i,1] = r_si$coefficients[1]
  coef_si[i,2] = r_si$coefficients[2]
  resid_si[1:janEst,i] = r_si$residuals
}


#Calcula media dos retornos
medRet = matrix(0,nativos,1)
medRet_ForaJan = matrix(0,nativos,1)
medRetBiC = matrix(0,nativosBiC,1)

for (i in 1:nativos){
  medRet[i,1] = mean(retornos[1:janEst,i])
  medRet_ForaJan[i,1] = mean(retornos[janEst:nlinhas,i])
}

for (i in 1:nativosBiC){
  
  medRetBiC[i,1] = mean(retornosBiC[1:janEst,i])
}


#Calcula matriz covariancia retornos historicos
covmatrix = cov(retornos[1:janEst,1:nativos])
covmatrix_ForaJan = cov(retornos[janEst:nlinhas,1:nativos])
covmatrixBiC = cov(retornosBiC[1:janEst,1:nativosBiC])

#Calcula retornos e matriz de covariancia com modelo single index
resp_ibov = mean(retIbov[1:janEst,1])
resp_si = matrix(0,nativos,1)
for (i in 1:nativos){
  resp_si[i,1] = coef_si[i,1] + coef_si[i,2]*resp_ibov
}
covmatrix_si = cov(resid_si)

resp_siBiC = resp_si
rownames(resp_siBiC) = tickers
resp_siBiC = resp_siBiC[tickersBiC,]

covmatrix_siBiC = covmatrix_si
colnames(covmatrix_siBiC) = tickers
rownames(covmatrix_siBiC) = tickers
covmatrix_siBiC = covmatrix_siBiC[tickersBiC,tickersBiC]


#Usa modelo single-index se chave_si=true
#chave_si = "false"
chave_si = "true"

if (chave_si == "true"){
  medRet = resp_si
  covmatrix = covmatrix_si
  covmatrixBiC = covmatrix_siBiC
}



#==================IntroCompFinR
short_selling = FALSE
carteira_min_risco <- globalMin.portfolio(medRet, covmatrix, shorts = short_selling)
carteira_min_risco

fronteira = efficient.frontier(medRet, covmatrix, nport = 40, shorts = short_selling)
fronteira
attributes(fronteira)

# Visualizaçao da saida

plot(fronteira$sd, fronteira$er, type="l", col="blue",xlab="Risk", ylab="Return")
#plot(fronteira, plot.assets=TRUE, col="blue", pch=16)

target.return = 0.7 * max(medRet)

#target.risk = 0.05834095

a = efficient.portfolio(medRet,covmatrix, target.return, shorts = FALSE)



#==================
#Usando quadprog
A.Equality =  matrix(1,nrow=nativos,ncol=1)
Amat <- cbind(A.Equality, medRet, diag(nativos), -diag(nativos))
bvec <- c(1, target.return, rep(0, nativos), rep(-1, nativos))
qp <- solve.QP(covmatrix, medRet, Amat, bvec, meq=2)
qp$solution

portReturn = qp$solution %*% medRet
portRisk = (qp$solution %*% covmatrix %*% qp$solution)^0.5

show('Port Return')
show(portReturn)
show('portRisk')
show(portRisk)

chaveReAmost=TRUE



#===================================================================
#Usando Resampling
if (chaveReAmost==TRUE){
  #semente das simulacoes
  set.seed(0)
  #numero de iteracoes de Monte Carlo
  MC_iter = 100
  #numero de portfolios para a fronteira eficiente
  numPort = 10
  #tamanho da amostra para o procedimento de reamostragem
  #tamAmostra = 50
  tamAmostra = 10*nativos
  #grau maximo de aversao a risco
  maxAversaoRisco = 20
  
  rs_pesos = data.frame()
  rs_pesosBiC = data.frame()
  
  #Cria portfolio para a biblioteca PortfolioAnalytics
  #Cria portfolio e define nome dos ativos
  rs_nomes.ativos = tickers
  rs_nomes.ativosBiC = tickersBiC
  #Define o portfolio
  rs_pspec = portfolio.spec(assets=rs_nomes.ativos)
  rs_pspecBiC = portfolio.spec(assets=rs_nomes.ativosBiC)
  
  #Adiciona restrições
  rs_pspec = add.constraint(portfolio = rs_pspec, type = "weight_sum",min_sum=1, max_sum =1)
  rs_pspec = add.constraint(portfolio = rs_pspec, type = "box", min=minAloc, max = maxAloc)
  
  rs_pspecBiC = add.constraint(portfolio = rs_pspecBiC, type = "weight_sum",min_sum=1, max_sum =1)
  rs_pspecBiC = add.constraint(portfolio = rs_pspecBiC, type = "box", min=minAloc, max = maxAloc)
  
  
  #rs_pspec = add.constraint(portfolio = rs_pspec,type="group", groups=list(groupA=assetsClassGroupA), group_min = c(0), group_max=c(0.05))
  
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
      rs_qu = add.objective(portfolio=rs_pspec, type = "return", name="mean")
      rs_qu = add.objective(portfolio = rs_qu, type = "risk", name="var", risk_aversion = rAversion[k,1])
      
      rs_opt_qu = optimize.portfolio(R=mc_R, portfolio = rs_qu,
                                     optimize_method = "ROI",
                                     trace=TRUE)
      rs_pesos = rbind(rs_pesos, data.frame(iter=i, risk_aver=rAversion[k,1], nomes= rs_nomes.ativos, pesos=rs_opt_qu$weights))
    }
    
  }
  
  #---Estrategia best in class(BiC)
  if (runBiC == TRUE){
    for (i in 1: MC_iter){
      mRetSCBiC = rmvnorm(tamAmostra, medRetBiC, covmatrixBiC)
      mc_RBiC = ts(mRetSCBiC)
      for (k in 1:numPort){
        rs_quBiC = list()
        #Maximiza utilidade (quadratica) usando pacote ROI
        rs_quBiC = add.objective(portfolio=rs_pspecBiC, type = "return", name="mean")
        rs_quBiC = add.objective(portfolio = rs_quBiC, type = "risk", name="var", risk_aversion = rAversion[k,1])
        
        rs_opt_quBiC = optimize.portfolio(R=mc_RBiC, portfolio = rs_quBiC,
                                          optimize_method = "ROI",
                                          trace=TRUE)
        
        rs_pesosBiC = rbind(rs_pesosBiC, data.frame(iter=i, risk_aver=rAversion[k,1], nomes= rs_nomes.ativosBiC, pesos=rs_opt_quBiC$weights))
      }
    }
  }
  
  #numiter = MC_iter*numPort
  
  # df_frontMC = rs_pesos %>% group_by(iter,risk_aver) %>%
  #   summarize(risco=pesos%*%covmatrix%*%pesos, retorno=pesos%*%medRet, portESG_Score = pesos%*%esg_score) %>% ungroup()
  #
  # df_frontMC$ids =c(1:numiter)
  
  #hist(df_frontMC$portESG_Score)
  
  #####df_frontMC1 = filter(df_frontMC, portESG_Score >= minESGScore)
  
  #Confere
  #=======================================================================
  # auxM = matrix(NA,1,length(rs_nomes.ativos))
  # groupPesos = data.frame(auxM)
  # colnames(groupPesos) = rs_nomes.ativos
  #
  # val_aux = matrix(0,1,nativos)
  # colnames(val_aux) = rs_nomes.ativos
  # for (i in 1:MC_iter ){
  #   for (j in 1:numPort){
  #     for (k in 1: nativos){
  #       val_aux[1,k] = as.numeric(filter(rs_pesos, nomes==rs_nomes.ativos[k] & iter==i & risk_aver==rAversion[j]) %>% select(pesos))
  #     }
  #     groupPesos = rbind(groupPesos,val_aux)
  #   }
  # }
  #
  # gPesos = groupPesos[-c(1),]
  # gPesos$ids = ids=c(1:numiter)
  #
  # p_retornos = matrix(0,numiter,1)
  # p_riscos = matrix(0,numiter,1)
  # p_ESG = matrix(0,numiter,1)
  # for (i in 1:numiter){
  #   auxPesos=as.numeric(gPesos[i,1:nativos])
  #   p_retornos[i]=auxPesos%*%medRet
  #   p_riscos[i] = auxPesos %*% covmatrix %*% auxPesos
  #   p_ESG[i] = auxPesos%*%esg_score
  # }
  #
  # rs_out = data.frame(id=c(1:numiter), riscos = p_riscos, retornos = p_retornos, p_ESG_Score = p_ESG)
  # Fim de confere========================================================
  
  #Une dados
  #df_RSout = df_frontMC %>% left_join(gPesos) %>% group_by(ids)
  #df_RSoutESG = filter(df_RSout, portESG_Score >= minESGScore)
  
  df_frontMC2 = rs_pesos %>% group_by(iter,risk_aver) %>%
    summarize(risco=pesos%*%covmatrix%*%pesos, retorno=pesos%*%medRet, portESG_Score = pesos%*%esg_score, ativos = rs_nomes.ativos,pesos = pesos) %>% ungroup()
  
  hist(df_frontMC2$portESG_Score, main = "",xlab="ESG Ratio Risk")
  title(main ="No ESG strategy")
  
  #covmatrixOrder = covmatrix[order(rs_nomes.ativos), order(rs_nomes.ativos)]
  #covmatrix_ForaJanOrder = covmatrix_ForaJan[order(rs_nomes.ativos), order(rs_nomes.ativos)]
  
  df_RSoutESG2 = filter(df_frontMC2, portESG_Score <= maxESGScore2)
  
  df_outMed2 = df_RSoutESG2 %>% group_by(risk_aver, ativos) %>% summarise(pesos = mean (pesos,na.rm = TRUE)) %>% ungroup()
  
  dfRet = data.frame(ativo=rs_nomes.ativos, retmedio=medRet)
  dfESG = data.frame(ativo=rs_nomes.ativos, esg_score=esg_score)
  saida2 = df_outMed2 %>% group_by(risk_aver) %>% summarize(risco=(pesos%*%covmatrix%*%pesos)^(1/2), retorno=dfRet$retmedio %*% pesos, esg_sco = dfESG$esg_score %*% pesos )
  saida2 = saida2[-c(1)]
  #retrisk_ratio = mean(saida$retorno / (saida$risco))
  
  
  #Fora da amostra
  dfRet_ForaJan = data.frame(ativo=rs_nomes.ativos, retmedio=medRet_ForaJan)
  saida_ForaJan2 = df_outMed2 %>% group_by(risk_aver) %>% summarize(risco=(pesos%*%covmatrix_ForaJan%*%pesos)^(1/2) , retorno=dfRet_ForaJan$retmedio %*% pesos, esg_sco = dfESG$esg_score %*% pesos )
  saida_ForaJan2 = saida_ForaJan2[-c(1)]
  
  #retrisk_ratio_ForaJan = mean(saida_ForaJan$retorno / saida_ForaJan$risco)
  
  df_RSoutESG3 = filter(df_frontMC2, portESG_Score <= maxEsGScore3)
  hist(df_RSoutESG3$portESG_Score, main = "",xlab="ESG Ratio Risk")
  title(main ="Integration strategy")
  df_outMed3 = df_RSoutESG3 %>% group_by(risk_aver, ativos) %>% summarise(pesos = mean (pesos,na.rm = TRUE)) %>% ungroup()
  
  dfRet3 = data.frame(ativo=rs_nomes.ativos, retmedio=medRet)
  dfESG3 = data.frame(ativo=rs_nomes.ativos, esg_score=esg_score)
  saida3 = df_outMed3 %>% group_by(risk_aver) %>% summarize(risco=(pesos%*%covmatrix%*%pesos)^(1/2), retorno=dfRet3$retmedio %*% pesos, esg_sco = dfESG3$esg_score %*% pesos )
  saida3 = saida3[-c(1)]
  # retrisk_ratio3 = mean(saida3$retorno / (saida3$risco))
  
  
  #Fora da amostra
  dfRet_ForaJan3 = data.frame(ativo=rs_nomes.ativos, retmedio=medRet_ForaJan)
  saida_ForaJan3 = df_outMed3 %>% group_by(risk_aver) %>% summarize(risco=(pesos%*%covmatrix_ForaJan%*%pesos)^(1/2) , retorno=dfRet_ForaJan3$retmedio %*% pesos, esg_sco = dfESG3$esg_score %*% pesos )
  saida_ForaJan3 = saida_ForaJan3[-c(1)]
  
  # retrisk_ratio_ForaJan3 = mean(saida_ForaJan3$retorno / saida_ForaJan3$risco)
  
  if (runBiC==TRUE) {
    df_frontMCBiC = rs_pesosBiC %>% group_by(iter,risk_aver) %>%
      summarize(risco=pesos%*%covmatrixBiC%*%pesos, retorno=pesos%*%medRetBiC, portESG_Score = pesos%*%esg_scoreBiC, ativos = rs_nomes.ativosBiC,pesos = pesos) %>% ungroup()
    
    if (selecBDInd == 1){
      tithist = 'Best in Class ISE'
    } else{
      if (selecBDInd==2){
        tithist = 'Best in Class ICO2'
      }else{
        tithist = 'Best in Class ISE or ICO2'
      }
    }
    
    hist(df_frontMCBiC$portESG_Score, main = "",xlab="ESG Ratio Risk")
    title(main =tithist)
    
    #df_RSoutESGBiC = filter(df_frontMCBiC, portESG_Score <= maxESGScoreBiC)
    df_RSoutESGBiC = df_frontMCBiC
    df_outMedBiC = df_RSoutESGBiC %>% group_by(risk_aver, ativos) %>% summarise(pesos = mean (pesos,na.rm = TRUE)) %>% ungroup()
    
    dfRetBiC = data.frame(ativo=rs_nomes.ativosBiC, retmedio=medRetBiC)
    dfESGBiC = data.frame(ativo=rs_nomes.ativosBiC, esg_score=esg_scoreBiC)
    saidaBiC = df_outMedBiC %>% group_by(risk_aver) %>% summarize(risco=(pesos%*%covmatrixBiC%*%pesos)^(1/2), retorno=dfRetBiC$retmedio %*% pesos, esg_sco = dfESGBiC$esg_score %*% pesos )
    saidaBiC = saidaBiC[-c(1)]
  }
  
  
  
}

#show('retrisk_ratio')
#show(retrisk_ratio)

#show('retrisk_ratio_ForaJan')
#show(retrisk_ratio_ForaJan)


#reg1 = lm(saida$retorno ~ log(saida$risco))
#retmod = reg1$coefficients[1] + reg1$coefficients[2] * log(saida$risco)

reg2 = lm(saida2$retorno ~ saida2$risco + I(saida2$risco^2))
retmod2 = reg2$coefficients[1] + reg2$coefficients[2] * saida2$risco + reg2$coefficients[3] * saida2$risco^2

reg3 = lm(saida3$retorno ~ saida3$risco + I(saida3$risco^2))
retmod3 = reg3$coefficients[1] + reg3$coefficients[2] * saida3$risco + reg3$coefficients[3] * saida3$risco^2

if (runBiC==TRUE){
  regBiC = lm(saidaBiC$retorno ~ saidaBiC$risco + I(saidaBiC$risco^2))
  retmodBiC = regBiC$coefficients[1] + regBiC$coefficients[2] * saidaBiC$risco + regBiC$coefficients[3] * saidaBiC$risco^2
}

#plot(saida3$risco,saida3$retorno)
#lines(saida3$risco,retmod3, type="l", col="blue")

if(runBiC == TRUE)
{  
  minrisco = min(saida2$risco, saida3$risco, saidaBiC$risco)
  maxrisco = max(saida2$risco, saida3$risco, saidaBiC$risco)
  minret = min(saida2$retorno, saida3$retorno, saidaBiC$retorno)
  maxret = max(saida2$retorno, saida3$retorno, saidaBiC$retorno)
} else{
  minrisco = min(saida2$risco, saida3$risco)
  maxrisco = max(saida2$risco, saida3$risco)
  minret = min(saida2$retorno, saida3$retorno)
  maxret = max(saida2$retorno, saida3$retorno)
}
minrisco = 0.004
maxrisco = 0.010
minret = 0.0010
maxret = 0.0022

if (runBiC==TRUE){
  plot(saida2$risco,retmod2, type="p", col="blue",pch = 19, xlab="Risk", ylab="Return", xlim=c(minrisco,maxrisco), ylim=c(minret, maxret))
  lines(saida3$risco,retmod3, type="p", col="red", pch = 17)
  lines(saidaBiC$risco, retmodBiC, type="p", col="green", pch = 15 )
  title(main =tithist)
  legend("bottomright",legend=c("No ESG", "Integration", "Best in Class"), col=c("blue", "red", "green"),  cex = 0.6, pch= c(19,17, 15))
  
}else {
  plot(saida2$risco,retmod2, type="l", col="blue",xlab="Risk", ylab="Return", xlim=c(minrisco,maxrisco), ylim=c(minret, maxret))
  lines(saida3$risco,retmod3, type="l", col="red")
  #  lines(saidaBiC$risco, retmodBiC, type="l", col="green" )  
  title(main ="")
}

minriscoForaJan = min(saida_ForaJan2$risco, saida_ForaJan3$risco)
maxriscoForaJan = max(saida_ForaJan2$risco, saida_ForaJan3$risco)
minretForaJan = min(saida_ForaJan2$retorno, saida_ForaJan3$retorno)
maxretForaJan = max(saida_ForaJan2$retorno, saida_ForaJan3$retorno)

plot(saida_ForaJan2$risco,saida_ForaJan2$retorno, type="p", col = "blue", pch = 19, xlab="Risk", ylab="Return", xlim=c(minriscoForaJan,maxriscoForaJan), ylim=c(minretForaJan, maxretForaJan))
lines(saida_ForaJan3$risco,saida_ForaJan3$retorno, type="p", col="red", pch =17)
legend("bottomright",legend=c("No ESG filter", "ESG filter"), col=c("blue", "red"),  cex = 0.8, pch= c(19,17))
title(main = "")

write.csv(df_outMed2, "C:\\Afranc\\Prospectos\\PIBICPIBITI\\2020\\pesos.csv")

#rrcusto = rbind(saida, saida3)
#write.csv(rrcusto, "C:\\Afranc\\Prospectos\\PIBICPIBITI\\2020\\rrcusto.csv")

riscoCusto = mean(saida2$risco)
retBase = reg2$coefficients[1] + reg2$coefficients[2] * riscoCusto + reg2$coefficients[3] * riscoCusto^2
retCusto = reg3$coefficients[1] + reg3$coefficients[2] * riscoCusto + reg3$coefficients[3] * riscoCusto^2

efficCusto = as.numeric(retCusto / retBase)
show('efficCusto')
show(efficCusto)

if (runBiC==TRUE){
  retCustoBiC = regBiC$coefficients[1] + regBiC$coefficients[2] * riscoCusto + regBiC$coefficients[3] * riscoCusto^2
  efficCustoBiC = as.numeric(retCustoBiC/retBase)
  show(efficCustoBiC)
}



# corrPesosPort = cor(df_outMed2$pesos,df_outMed3$pesos)
# show('corrPesosPort')
# show(corrPesosPort)

#  if (runBiC==TRUE){
#    corrPesosPortBiC = cor(df_outMed2$pesos,df_outMedBiC$pesos)
#    show(corrPesosPortBiC)
# }

esgBase=mean(saida2$esg_sco)
esgFilt=mean(saida3$esg_sco)
efficESG = 1-esgFilt/esgBase
show('efficESG')
show(efficESG)

if (runBiC==TRUE){
  esgFiltBiC=mean(saidaBiC$esg_sco)
  efficESG_BiC = 1-esgFiltBiC/esgBase
  show(efficESG_BiC)
}

sda1 = c(maxEsGScore3,selecBDInd, minAloc,maxAloc, efficCusto, efficCustoBiC, efficESG, efficESG_BiC)
show(sda1)
