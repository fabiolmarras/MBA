#'##############################################################################
#'  Data:     7/18/2022
#'  Projeto:  Análise de dados históricos do mercado de ações para
#'            tomada de decisões
#'  Aluno:    Fabio Luis Marras
#'  Entidade: Universidade de São Paulo
#'            Escola Superior de Agricultura Luiz de Queiroz
#'            MBA Data Sciense & Analitycs Turma 211
#'##############################################################################
#' Carrega as bibliotecas necessárias
#' Library                            # Required by
#'##################################  ##########################################
library('TTR')                        # 
library('quantmod')                   # 
library('ggplot2')                    # 
library('ggthemes')                   # themes (ggplot)
library('forecast')                   # 
library('tseries')                    # 
library('rugarch')                    # 
library('prophet')                    # 
library('tsfknn')                     # 
library('timetk')                     # 
library('reshape2')                   # melt
library('PerformanceAnalytics')       # chart.Correlation
library('tsfknn')                     # predknn
#'##############################################################################
#' Define a lista de todas as ações a serem utilizadas e os intervalos de 
#' datas a serem carregados no banco de dados
#'##############################################################################
set.seed(1)
lista_acoes <- c('IBM', 'BAC','MSFT','WMT','AMZN','ITUB4.SA', 'WEGE3.SA', 'VALE3.SA', 'PETR3.SA')
acao_selecionada <- 7
data_final <- as.Date(Sys.Date())
# data_final <- "2022-03-01"
data_inicial <- as.Date(data_final - months(6))
# data_inicial <- "2020-12-01"
#'##############################################################################
#' Efetua a carga dos dados diretamente do Yahoo!
#'##############################################################################
acao_download <-as.data.frame(getSymbols(lista_acoes[acao_selecionada],
                                         src="yahoo", 
                                         auto.assign=FALSE, 
                                         from= data_inicial,
                                         to = data_final))
#'##############################################################################
rm(data_final, data_inicial)

# Para preservar os dados originais na variável "acao_download"
acao <- acao_download
acao <- na.omit(acao)
# nrow(acao)

#'##############################################################################
#' Visualiza os dados carregados no data frame
#' 
chartSeries(acao,name = lista_acoes[acao_selecionada], 
            theme = chartTheme("white"),
            TA=c(addVo()))

chartSeries(acao,name = lista_acoes[acao_selecionada], 
            theme = chartTheme("white"),
            TA=c(addBBands(),
                 addMACD(),
                 addADX(),
                 addSMI(n=12,slow=26,fast=2,signal=9,ma.type="EMA")))

#' Gera o gráfico reduzido com as últimas 100 amostras
chartSeries(tail(acao, n=100L),name = lista_acoes[acao_selecionada], 
            theme = chartTheme("white"),
            TA=c(addSMI(n=12,slow=26,fast=2,signal=9,ma.type="EMA"),
                 addMACD(), 
                 addRSI(),addCCI()))

#'##############################################################################
#' Acerta colunas do dataframe recebido e efetua limpezas necessárias
#' 
acao$Data = as.Date(row.names(acao))
colnames(acao) = c("Abertura", "Maxima", "Minima", "Fechamento", "Volume", "Ajuste", "Data")
acao = acao[c("Data", "Abertura", "Maxima", "Minima", "Fechamento", "Volume", "Ajuste")]
row.names(acao) <- NULL
acao$Ajuste <- NULL

#'##############################################################################
#' Análise RSI
#'##############################################################################
acao_analise <- acao
acao$recomendacao_rsi_index <- RSI(acao_analise$Fechamento, n=14, maType="EMA", wilder = TRUE, wts=acao_analise["Volume"])

#' Recomendação RSI
#' O Índice de Força Relativa (RSI) calcula uma proporção dos movimentos ascendentes
#' recentes dos preços para o movimento absoluto dos preços. 
#' O RSI é interpretado como um indicador de sobrecompra/sobrevenda quando o valor 
#' está acima de 70 / abaixo de 30. Se o preço estiver fazendo novas altas/baixas 
#' e o RSI não estiver, isso indica uma reversão.
#' Se o Indicador RSI for inferior a 30, isso significa que o mercado está sobrevendido
#'e que o preço pode eventualmente aumentar.
acao$recomendacao_rsi <- ifelse (acao$recomendacao_rsi_index > 65, "desvalorização", "estabilidade")
acao$recomendacao_rsi <- ifelse (acao$recomendacao_rsi_index < 35, "valorização", acao$recomendacao_rsi)
##########
# Gera gráfico RSI com as últimas 100 amostras
acao_resumida <- tail(acao, n=100L)
# Define os limite do eixo Y primário
ylim.prim <- c(0, max(acao_resumida$recomendacao_rsi_index))
# Define os limite do eixo Y secundário
ylim.sec <- c(min(acao_resumida$Fechamento), max(acao_resumida$Fechamento))
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - (b*ylim.sec[1])

ggplot(acao_resumida, aes(x = Data, y = recomendacao_rsi_index)) +
  geom_line(aes(color = "Índice RSI")) +
  geom_line(aes(y = a + Fechamento*b, color = "Fechamento")) +
  scale_y_continuous("Índice RSI", sec.axis = sec_axis(~ (. - a)/b, name = "Valor de Fechamento")) +
  labs(x = "Data", y = "Indice", color = "Legenda") +
  theme_base() +
  labs(title = "Análise RSI x Valor de Fechamento")

rm(acao_analise, acao_resumida)

#'##############################################################################
#' Análise SMI
#'##############################################################################
acao_analise <- acao
acao_analise$Data <- NULL
acao_analise$Abertura <- NULL
acao_analise$Volume <- NULL
acao_analise$analise_rsi <- NULL
acao_analise$recomendacao_rsi_index <- NULL
acao_analise$recomendacao_rsi <- NULL

#' necessário converter para inglês e limpar o data frame para usar a função SMI
colnames(acao_analise) = c("High", "Low", "Close")
#'Efetua a análise
smi_result <- as.data.frame(SMI(acao_analise[,c("High","Low","Close")], n = 13, nFast = 2, nSlow = 25, nSig = 9, maType = "EMA", bounded = TRUE))
acao$analise_smi <- smi_result$SMI
acao$analise_smi_signal <- smi_result$signal

# par(mfrow = c(2, 1))
# hist(acao$analise_smi, main = ("Histograma do SMI"), col = "0")
# hist(acao$analise_smi_signal, main = ("Histograma do Sinal SMI"), col = "0")
# par(mfrow = c(1, 1))

#' Recomendação SMI
#' Quando a linha SMI cruza acima da linha de sinal, é gerado um sinal de compra.
acao$recomendacao_smi_index <- (acao$analise_smi - acao$analise_smi_signal) * 1 
acao$recomendacao_smi <- ifelse (acao$analise_smi > acao$analise_smi_signal, "valorização", "desvalorização")

#'Gráfico SMI com as últimas 100 amostras
ggplot(tail(acao, n=100L), aes(x = Data, y = analise_smi)) +
  geom_line(aes(color = "Análise SMI")) +
  geom_line(aes(y = analise_smi_signal, color = "Sinal SMI")) +
  labs(x = "Data", y = "Indice", color = "Legenda") +
  theme_base() +
  labs(title = "Análise SMI")


rm(smi_result, acao_analise)

#'##############################################################################
#' Análise MACD - Divergência de convergência média móvel (MACD)
#' 
#' Moving Average Convergence Divergence (MACD) é um indicador de impulso de 
#' acompanhamento de tendência que mostra a relação entre duas médias móveis do 
#' preço de um título. O MACD é calculado subtraindo a Média Móvel Exponencial 
#' (EMA) de 26 períodos da EMA de 12 períodos.
#'
acao_analise <- acao
macd_result <- as.data.frame(MACD(acao_analise$Fechamento, nFast = 12, nSlow = 26, nSig = 9, maType="EMA"))
acao$analise_macd <- macd_result$macd
acao$analise_macd_signal <- macd_result$signal

#' Quando a linha MACD cruza acima da linha de sinal, é gerado um sinal de compra.
acao$recomendacao_macd_index <- (acao$analise_macd - acao$analise_macd_signal) * 100
acao$recomendacao_macd <- ifelse (acao$analise_macd > acao$analise_macd_signal, "valorização", "desvalorização")

# Gera gráfico MACD com as últimas 100 amostras

ggplot(tail(acao, n=100L), aes(x = Data, y = analise_macd)) +
  geom_line(aes(color = "Análise MACD")) +
  geom_line(aes(y = analise_macd_signal, color = "Sinal MACD")) +
  labs(x = "Data", y = "Indice", color = "Legenda") +
  theme_base() +
  labs(title = "Análise MACD")

# par(mfrow = c(2, 1))
# hist(acao_resumida$analise_macd, main = ("Histograma do MACD"), col = "0")
# hist(acao_resumida$analise_macd_signal, main = ("Histograma do Sinal MACD"), col = "0")
# par(mfrow = c(1, 1))

rm(macd_result, acao_analise)

#'##############################################################################
#' Análise ADX - Average Direction Index
#' Um sinal de compra/venda é gerado quando o +/-DI cruza o -/+DI, quando o DX/ADX
#' sinaliza uma forte tendência. Um DX alto/baixo sinaliza uma tendência 
#' forte/fraca. O DX geralmente é suavizado com uma média móvel (ou seja, o ADX).
#' 
#' Os valores variam de 0 a 100, mas raramente ultrapassam 60. Para interpretar 
#' o ADX, considere: 
#' - um número acima de 40 => tendência forte de valorização
#' - um número acima de 25 => tendência de fortalecimento
#' - um numero abaixo de 25 => tendência fraca de 

acao_analise <- acao
adx_result <- as.data.frame(ADX(acao_analise[,c("Maxima","Minima","Fechamento")], n=12, maType="EMA", wilder=TRUE))
acao$analise_dip <- adx_result$DIp
acao$analise_din <- adx_result$DIn
acao$analise_dx <- adx_result$DX
acao$analise_adx <- adx_result$ADX
acao$recomendacao_adx <- 0

#' Quando a linha MACD cruza acima da linha de sinal, é gerado um sinal de compra.
acao$recomendacao_adx_index <- acao$analise_dip - acao$analise_din

for(i in 1:nrow(acao)) 
{
  if (!is.na(acao$analise_adx[i]))
  {
    if (acao$analise_dip[i] > acao$analise_din[i] & acao$analise_adx[i] >= 25) 
    {
      acao$recomendacao_adx[i] <- "valorização"
    } else 
    {
      if (acao$analise_dip[i] < acao$analise_din[i] & acao$analise_adx[i] >= 25) 
      {
        acao$recomendacao_adx[i] <- "desvalorização"
      } else 
      {
        acao$recomendacao_adx[i] <- "estabilidade"
      }
    }
  }
}

# acao$recomendacao_adx <- ifelse , "valorização", "desvalorização")
# acao$recomendacao_adx <- ifelse (acao$analise_dip < acao$analise_din & acao$analise_adx >=25, "desvalorização", "")

# Gera gráfico ADX com as últimas 100 a amostras
acao_resumida <- tail(acao, n=100L)

gfg_data <- data.frame(Data = acao_resumida$Data
                       , DIp = acao_resumida$analise_dip
                       , DIn = acao_resumida$analise_din
                       #, DX = acao_resumida$analise_dx
                       , ADX = acao_resumida$analise_adx
)
data_long <- melt(gfg_data, id = "Data")
ggplot(data_long,            
       aes(x = Data,
           y = value,
           color = variable)) + 
  geom_line() +
  labs(x = "Data", y = "Indice", color = "Legenda") +
  theme_base() +
  labs(title = "Análise Average Directional Movement Index")

rm(gfg_data, data_long, adx_result, acao_analise, acao_resumida)

#'##############################################################################
#' CCI
#' 
#' O CCI é projetado para detectar tendências de mercado iniciais e finais. 
#' O intervalo de 100 a -100 é o intervalo normal de negociação. Os valores de 
#' CCI fora deste intervalo indicam condições de sobrecompra ou sobrevenda. 
#' 
#' Você também pode procurar divergência de preços no CCI. Se o preço estiver 
#' alcançando novos máximos e o CCI não estiver, é provável que haja uma 
#' correção de preço.
#' 
#' O Commodity Channel Index foi desenvolvido por Donald Lambert e é descrito 
#' em seu artigo na edição de outubro de 1980 da revista Commodities
#' 
#' https://www.rdocumentation.org/packages/TTR/versions/0.24.3/topics/CCI
#' https://www.fmlabs.com/reference/default.htm?url=CCI.htm
#'##############################################################################

acao_analise <- acao
acao_analise$Data <- NULL
acao_analise$Abertura <- NULL
acao_analise$Volume <- NULL

#' necessário converter para inglês e limpar o data frame para usar a função SMI
colnames(acao_analise) = c("High", "Low", "Close")
acao$recomendacao_cci_index <- (CCI(acao_analise[,c("High","Low","Close")], n=12, c = 0.015))

#################

for(i in 1:nrow(acao)) 
{
  if (!is.na(acao$recomendacao_cci_index[i]))
  {
    acao$recomendacao_cci[i] <- "estabilidade"
    if (acao$recomendacao_cci_index[i] > 130) 
    {
      acao$recomendacao_cci[i] <- "valorização"
    } else 
    {
      if (acao$recomendacao_cci_index[i] < -130) 
      {
        acao$recomendacao_cci[i] <- "desvalorização"
      }
    }
  }
}




#################

# Gera gráfico CCI com as últimas 100 amostras
acao_resumida <- tail(acao, n=100L)
# Define os limite do eixo Y primário
ylim.prim <- c(0, max(acao_resumida$recomendacao_cci_index))
# Define os limite do eixo Y secundário
ylim.sec <- c(min(acao_resumida$Fechamento), max(acao_resumida$Fechamento))
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - (b*ylim.sec[1])

ggplot(acao_resumida, aes(x = Data, y = recomendacao_cci_index)) +
  geom_line(aes(color = "Índice CCI")) +
  # geom_line(aes(y = a + Fechamento*b, color = "Fechamento")) +
  # scale_y_continuous("Índice CCI", sec.axis = sec_axis(~ (. - a)/b, name = "Valor de Fechamento")) +
  labs(x = "Data", y = "Indice", color = "Legenda") +
  theme_base() +
  labs(title = "Análise CCI x Valor de Fechamento")

#'##############################################################################
#' Consolidação das variáveis de recomendações e tratamentos
#'##############################################################################
acao_recomendacao <- na.omit(acao)
acao_recomendacao$Volume <- NULL
acao_recomendacao$analise_smi <- NULL
acao_recomendacao$analise_smi_signal <- NULL
acao_recomendacao$analise_rsi <- NULL
acao_recomendacao$analise_macd <- NULL
acao_recomendacao$analise_macd_signal <- NULL
acao_recomendacao$analise_dip <- NULL
acao_recomendacao$analise_din <- NULL
acao_recomendacao$analise_dx <- NULL
acao_recomendacao$analise_adx <- NULL
acao_recomendacao$recomendacao_rsi <- NULL

summary(acao_recomendacao)
# acao_recomendacao <- acao_recomendacao[c("Data",
#                                         "Abertura",
#                                          "Maxima",
#                                         "Minima",
#                                         "Fechamento",
#                                         "recomendacao_smi_index",
#                                         "recomendacao_macd_index",
#                                         "recomendacao_adx_index", 
#                                         "recomendacao_smi", 
#                                         "recomendacao_macd", 
#                                         "recomendacao_adx")]
#
#'##############################################################################
#' Normaliza as variáveis de recomendação através de zScore
#'##############################################################################

# acao_recomendacao$recomendacao_rsi_index = scale(acao_recomendacao$recomendacao_rsi_index, center = T))[,1]
acao_recomendacao$recomendacao_smi_index = (scale(acao_recomendacao$recomendacao_smi_index, center = T))[,1]
acao_recomendacao$recomendacao_macd_index = (scale(acao_recomendacao$recomendacao_macd_index, center = T))[,1]
acao_recomendacao$recomendacao_adx_index = (scale(acao_recomendacao$recomendacao_adx_index, center = T))[,1]
acao_recomendacao$recomendacao_cci_index = (scale(acao_recomendacao$recomendacao_cci_index, center = T))[,1]


acao_recomendacao$geral <- (acao_recomendacao$recomendacao_smi_index + 
                              acao_recomendacao$recomendacao_macd_index +
                              acao_recomendacao$recomendacao_adx_index +
                              acao_recomendacao$recomendacao_cci_index) /4

# hist(acao_recomendacao$geral, main = ("Histograma da recomendação Geral"), col = "0")


cat(
  " =======================================\n",
  "   Recomendações futuras:              \n",
  "=======================================\n",
  "   SMI Index:  ",round(acao_recomendacao$recomendacao_smi_index[nrow(acao_recomendacao)], digits = 4),acao_recomendacao$recomendacao_smi[nrow(acao_recomendacao)],"\n",
  "   MACD Index: ",round(acao_recomendacao$recomendacao_macd_index[nrow(acao_recomendacao)], digits = 4),acao_recomendacao$recomendacao_macd[nrow(acao_recomendacao)],"\n",
  "   ADX Index:  ",round(acao_recomendacao$recomendacao_adx_index[nrow(acao_recomendacao)], digits = 4),acao_recomendacao$recomendacao_adx[nrow(acao_recomendacao)],"\n",
  "   CCI Index:  ",round(acao_recomendacao$recomendacao_cci_index[nrow(acao_recomendacao)], digits = 4),acao_recomendacao$recomendacao_adx[nrow(acao_recomendacao)],"\n",
  
   "   Média geral:",round(acao_recomendacao$geral[nrow(acao_recomendacao)], digits = 4),"\n",
  "=======================================\n")

acao_recomendacao_resumida <- tail(acao_recomendacao, n=100L)

gfg_data <- data.frame(Data = acao_recomendacao_resumida$Data
                       , SMI = acao_recomendacao_resumida$recomendacao_smi_index
                       , MACD = acao_recomendacao_resumida$recomendacao_macd_index
                       , ADX = acao_recomendacao_resumida$recomendacao_adx_index
                       , ICC = acao_recomendacao_resumida$recomendacao_cci_index
                       # , geral = acao_recomendacao_resumida$geral
)

data_long <- melt(gfg_data, id = "Data")
ggplot(data_long,            
       aes(x = Data,
           y = value,
           color = variable)) +
  geom_line() +
  labs(x = "Data", y = "Indice", color = "Legenda") +
  theme_base() +
  labs(title = "Análise dos indicadores")


# hist(acao_recomendacao_resumida$geral, main = ("Histograma da recomendação Geral"), col = "0")

ggplot(acao_recomendacao_resumida, aes(x = Data, y = geral)) +
  geom_line(aes(color = "Indice Geral")) +
  labs(x = "Data", y = "Indice", color = "Legenda") +
  theme_base() +
  labs(title = "Análise índice médio de recomendação")

rm(data_long, gfg_data, acao_recomendacao_resumida)

#'##############################################################################
#' Efetua a análise de correlação entre os indicadores/modelos matemáticos
#' e os valores de fechamento D+0 e D+1
#'##############################################################################
acao_correl <- data.frame(acao_recomendacao$Fechamento)
colnames(acao_correl) = c("Fechamento")

for(i in 2:nrow(acao_correl))
{
  acao_correl$Fechamento_D1[i-1] <- acao_correl$Fechamento[i]
}

# Complementa o dataframe para executar a correlação entre as varíáveis
acao_correl$recomendacao_smi_index <- acao_recomendacao$recomendacao_smi_index
acao_correl$recomendacao_macd_index <- acao_recomendacao$recomendacao_macd_index
acao_correl$recomendacao_adx_index <- acao_recomendacao$recomendacao_adx_index
acao_correl$recomendacao_cci_index <- acao_recomendacao$recomendacao_cci_index

# Elimina a ultima linha somente para fazer a análise de Correlação de Pearson
acao_correl <- head(acao_correl, n=nrow(acao_correl)-1)

#'
#' Reduz a amostra para ser mais significativo o resultado
#' efetua a correlação entre variáveis
#'

acao_correl <- tail(acao_correl, n=21L)
correl <- cor(acao_correl)

cor.test(acao_correl$Fechamento,acao_correl$recomendacao_smi_index,alternative="two.sided",conf.level = 0.80)
cor.test(acao_correl$Fechamento,acao_correl$recomendacao_macd_index,alternative="two.sided",conf.level = 0.80)         
cor.test(acao_correl$Fechamento,acao_correl$recomendacao_adx_index,alternative="two.sided",conf.level = 0.80)
cor.test(acao_correl$Fechamento,acao_correl$recomendacao_cci_index,alternative="two.sided",conf.level = 0.80)

cor.test(acao_correl$Fechamento_D1,acao_correl$recomendacao_smi_index,alternative="two.sided",conf.level = 0.80)
cor.test(acao_correl$Fechamento_D1,acao_correl$recomendacao_macd_index,alternative="two.sided",conf.level = 0.80)         
cor.test(acao_correl$Fechamento_D1,acao_correl$recomendacao_adx_index,alternative="two.sided",conf.level = 0.80)
cor.test(acao_correl$Fechamento_D1,acao_correl$recomendacao_cci_index,alternative="two.sided",conf.level = 0.80)

#' 
#' cria o gráfico de correlações entre variáveis
#' quanto mais estrelas vermelhas, maior o nível de significancia
#' quanto maior o número, maior a correlação
#' 
chart.Correlation(acao_correl, histogram=TRUE, method = "pearson", pch="+")

#' Apresenta os resultados da correlação na tela
#' 
cat(
  " =======================================\n",
  "        Analise de correlação \n",
  "=======================================\n",
  "Indice   Fechamento D+0  Fechamento D+1\n",
  "======   ==============  ==============\n",
  " SMI   : ",correl[1,3],"     ",correl[2,3],"\n",
  " MACD  : ",correl[1,4],"     ",correl[2,4],"\n",
  " ADX   : ",correl[1,5],"     ",correl[2,5],"\n",
  " ADX   : ",correl[1,6],"     ",correl[2,6],"\n",
  "=======================================\n")

rm(acao_correl, correl, i)

#'##############################################################################
# Cria a série temporal baseada nos dados de fechamento da ação analizada
# ts_acao = ts(na.remove(acao$Fechamento), frequency= 1)

frequencia <- 251
acao_recomendacao_resumida <- tail(acao, n=(frequencia+1))

substr(acao_recomendacao_resumida$Data[1], 1, 4)
acao_recomendacao_resumida$Data[frequencia+1]

# ano_ini <- substr(acao_recomendacao_resumida$Data[1], 1, 4)
# mes_ini <- substr(acao_recomendacao_resumida$Data[1], 6, 7)
# dia_ini <- substr(acao_recomendacao_resumida$Data[1], 9, 10)
# 
# ano_fim <- substr(acao_recomendacao_resumida$Data[frequencia+1], 1, 4)
# mes_fim <- substr(acao_recomendacao_resumida$Data[frequencia+1], 6, 7)
# dia_fim <- substr(acao_recomendacao_resumida$Data[frequencia+1], 9, 10)

ts_acao <- ts(data = acao_recomendacao_resumida$Fechamento)

# ts_acao <- ts(data = acao_recomendacao_resumida$Fechamento, 
#                      start = c(ano_ini,mes_ini,dia_ini), 
#                      end = c(ano_fim,mes_fim,dia_fim), 
#                      frequency = 251)

plot(ts_acao, 
     main = paste("Ação:", lista_acoes[acao_selecionada]),
     xlab = "Amostra",
     ylab = "Valor"
)


rm(frequencia, acao_recomendacao_resumida)
#'##############################################################################
#' Efetua o Augmented Dickey–Fuller Test
#' =====================================
#' Teste de Dickey-Fuller aumentado é um teste de raiz unitária em séries
#' temporais.
#' A estatística ADF, usada no teste, é um número negativo, e quanto
#' mais negativo, mais indicativo o teste se torna de rejeitar a hipótese
#' nula de que existe raiz unitária na série.
#' 
#' Etapa: Testar e garantir a estacionaridade 
#' Para modelar uma série temporal com a abordagem Box-Jenkins, a série
#' deve ser estacionária. Uma série temporal estacionária significa uma
#' série temporal sem tendência, com média e variância constantes ao
#' longo do tempo, o que facilita a previsão de valores.
#' 
#' Teste de estacionaridade
#' Testamos a estacionariedade usando o teste de raiz unitária de
#' Dickey-Fuller aumentado:
#' Resultados analisados:  
#' p-value < 0.05 série temporal seja estacionária.
#' p-value > 0.05 conclui que a série temporal possui raiz unitária,
#' o que significa que é um processo não estacionário.
#' 
#' 
cat ('o p.value da série temporal é: ',(adf.test(ts_acao))$p.value)

#' Caso a série seja não estacionária, quantas diferenciações são necessárias
#' para tornar a série estacionária?

ifelse ((adf.test(ts_acao))$p.value > 0.01 , 
        paste("Série não é estacionária e necessita de",ndiffs(ts_acao),"diferenciação(ões)"), 
        "Série é estacionária e não necessita diferenciação")

#'##############################################################################
#' Aplicamos as funções ACF (função de autocorrelação)
#' PACF (função de autocorrelação parcial) ao conjunto de dados
#' 
#' Autocorrelação refere-se a quão correlacionada uma série temporal 
#' está com seus valores passados. Como sabemos nos modelos AR, o ACF 
#' diminuirá exponencialmente. O ACF é o gráfico usado para ver a 
#' correlação entre os pontos, até e incluindo as unidades de defasagem.
#' Podemos ver que a autocorrelação é significativa para grande número
#' de defasagens, mas talvez a autocorrelação nas defasagens posteriores 
#' seja meramente devido à propagação da autocorrelação nas primeiras 
#' defasagens.
#' Usamos o gráfico ACF e PACF para identificar a ordem (q) e o PACF 
#' diminuirá exponencialmente. Se podemos notar que é um pico 
#' significativo apenas nas primeiras defasagens, significa que toda a 
#' autocorrelação de ordem superior é efetivamente explicada pela 
#' autocorrelação de primeira defasagem.
#'##############################################################################
#'Gerar gráficos do ACF e do PACF
#'
#' https://support.minitab.com/en-us/minitab/19/help-and-how-to/statistical-modeling/time-series/how-to/autocorrelation/interpret-the-results/autocorrelation-function-acf/
par(mfrow = c(1, 2)) 
acf(ts_acao) 
pacf(ts_acao) 
par(mfrow = c(1, 1))

#'##############################################################################
#' Aplicar o auto.arima aos dados de fechamento da ação selecionada
#' 
modelo_arima <- auto.arima(ts_acao, 
                           lambda = "auto", 
                           trace = TRUE,
                           max.order = 3,
                           ic = c("aicc", "aic", "bic"),
                           stationary = FALSE,
                           seasonal = TRUE,
                           stepwise=FALSE,
                           seasonal.test = c("seas", "ocsb", "hegy", "ch"),
                           approximation=FALSE)
accuracy(modelo_arima)

#'Análise dos resultados do modelo baseado em auto.arima
#'
#' Analisar os resíduos do modelo com os parâmetros ARIMA selecionados
#' 
#' Os “resíduos” em um modelo de série temporal são as sobras após o
#' ajuste de um modelo. Na maioria dos modelos de séries temporais, 
#' os resíduos são iguais à diferença entre as observações e os valores
#' ajustados

forecast_modelo <- forecast(modelo_arima,h=10)

plot(resid(modelo_arima), 
     main=paste((lista_acoes[acao_selecionada]),"- Resíduos (Arima) X Tempo"),
     xlab = "Dia",
     ylab = "Resíduos")

plot(forecast_modelo, 
     main=paste((lista_acoes[acao_selecionada]),"- Forecast usando", forecast_modelo$method),
     xlab = "Dia",
     ylab = "Valor")

#' Como fizemos alguns testes de correlação com nosso conjunto de dados,
#' agora verificamos nossos resíduos em uma curva normal.

# Histogram of Residuals & Normality Assumption

dados <- resid(modelo_arima)
hist(dados,
     col = "white",
     freq = F,
     main = paste("Histograma dos Resíduos:",(lista_acoes[acao_selecionada])),
     xlab = "Resíduos",
     ylim = c(0,6),
     breaks = 15)
curve(dnorm(x, mean=mean(dados), sd=sd(dados)), add= T, col = "red")

dados = data.frame(residuos=resid(modelo_arima))
ggplot (dados) +
  aes(x = residuos) +
  labs(x = "Resíduos", y = "Densidade") +
  geom_histogram(fill = "white",
                 col = "black",
                 alpha = 1,
                 bins = 10,
                 aes(y=..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(dados$residuos), sd = sd(dados$residuos)), col = "red") +
  theme_bw() +
  labs(title = paste("Histograma dos Resíduos e Nomalidade", (lista_acoes[acao_selecionada])))

# Diagnostics for Arima
tsdiag(modelo_arima)

# Box test for lag=2
Box.test(modelo_arima$residuals, lag= 2, type="Ljung-Box")
Box.test(modelo_arima$residuals, type="Ljung-Box")

plot(as.ts(ts_acao,),
     col="red",
     main=paste((lista_acoes[acao_selecionada]),"- Valor Reais x Resíduos -", forecast_modelo$method),
     xlab = "Amostra",
     ylab = "Valor") +
  lines(modelo_arima$fitted,col="black", type="l")

#Dataset forecasting  for the  next  3  days
# price_forecast <- forecast(modelo_arima,h=30)
price_forecast <- forecast(modelo_arima,h=5)

plot(price_forecast,
     xlab = "Amostra",
     ylab = "Valor",
     type = "l",col="black",
     main = (paste(lista_acoes[acao_selecionada],"usando forcast", price_forecast$method)))

head(price_forecast$mean)
head(price_forecast$upper)
head(price_forecast$lower)

#Dividing the data into train & test sets , applying the model
N <- length (acao$Fechamento)
n <- 0.9*N
train <- as.data.frame(acao$Fechamento[1:n])
test <- as.data.frame(acao$Fechamento[(n+1):N])
colnames(train) = c("Fechamento")
colnames(test) = c("Fechamento")

ts_train <- ts(train, frequency= 1)
train_arimafit <- auto.arima(ts_train, lambda= "auto", stationary = TRUE, ic = c("aicc", "aic", "bic"), trace = TRUE)
summary(train_arimafit)

predlen <- nrow(test)
trainarima_fit <- forecast(train_arimafit, h=predlen)


#Plotting mean predicted  values vs real data
meanvalues<- as.vector(trainarima_fit$mean)
precios <- as.vector(test$Fechamento)

max_precios <- max(precios)
min_precios <- min(precios)
max_meanvalues <- max(meanvalues)
min_meanvalues <- min(meanvalues)
min_y <- (ifelse(min_meanvalues > min_precios, min_precios, min_meanvalues))
max_y <- (ifelse(max_meanvalues > max_precios, max_meanvalues, max_precios))

plot(precios, 
     ylim=c(min_y, max_y), 
     xlab = "Amostra",
     ylab = "Valor",
     type = "l",col="red",
     main = paste("Ação:", lista_acoes[acao_selecionada]),) +
  lines(meanvalues, type = "l", col = "blue")
#dev.off()

#'##############################################################################
#KNN regression Time Series Forcasting 

#Dataframe creation and model application
# predknn <- knn_forecasting(ts_acao, h = 30, lags = 1:1, k = 1, msas = "MIMO")
predknn <- knn_forecasting(ts_acao, h = 30, lags = NULL, k = c(3, 5, 7),  cf = c("mean", "median", "weighted"), msas = "MIMO")

# predknn <- knn_forecasting(ts_acao, h = 30, lags = 1:1, k = c(1, 2, 3), msas = "MIMO")
predknn <- knn_forecasting(ts_acao, h = 10, msas = "MIMO")

# knn_forecasting(ts_acao, h = 30 , lags = NULL, k = c(3, 5, 7), msas = c("recursive", "MIMO"), cf = c("mean", "median", "weighted"), transform = c("additive", "multiplicative", "none"))

#Train set model accuracy
ro <- rolling_origin(predknn)
print(ro$global_accu)
plot(predknn, main = "Teste", type = "l",col="red",)

