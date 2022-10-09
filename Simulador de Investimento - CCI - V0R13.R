#'##############################################################################
acao_investimento <- tail(acao_recomendacao, n=200L)
acao_investimento$recomendacao_rsi_index <- NULL
acao_investimento$recomendacao_smi_index <- NULL
# acao_investimento$recomendacao_macd_index <- NULL
# acao_investimento$recomendacao_adx_index <- NULL
# acao_investimento$recomendacao_cci_index <- NULL
acao_investimento$recomendacao_smi <- NULL
acao_investimento$recomendacao_macd <- NULL
#acao_investimento$recomendacao_adx <- NULL
acao_investimento$geral <- NULL
#'
#' valor_medio >> valor justo de compra definido pelo preço média mínima e 
#' máxima do dia
#' 

acao_investimento$valor_medio <- (acao_investimento$Abertura +
                                    acao_investimento$Maxima +
                                    acao_investimento$Minima +
                                    acao_investimento$Fechamento)/4

### define parametros iniciais 
acao_investimento$eventos <- 0  # valor disponível em conta corrente
acao_investimento$conta_valor_inicial <- 0  # valor disponível em conta corrente
acao_investimento$conta_valor_final <- 0  # valor disponível em conta corrente
acao_investimento$acoes_qtd <- 0          # quantidade de ações na carteira

#' #############################################################################
#' Deposita dinheiro inicialmente na conta
acao_investimento$conta_valor_inicial[1] <- 1000

#' #############################################################################
#' Cria os eventos de compra e venda
#' 
acao_investimento$eventos[1] <- acao_investimento$recomendacao_cci[1]
for(i in 2:nrow(acao_investimento))
{
  if (acao_investimento$recomendacao_cci[i] != acao_investimento$recomendacao_cci[i-1])
  {
    acao_investimento$eventos[i] <- acao_investimento$recomendacao_cci[i]
  } else {
    acao_investimento$eventos[i] <- 0
  }
}

#' #############################################################################
#' Define a variavel conta_valor_final
#' 

for(i in 1:nrow(acao_investimento))
{
  if (acao_investimento$eventos[i] == "valorização" & acao_investimento$conta_valor_inicial[i] != 0)
  {
    acao_investimento$acoes_qtd[i] <- acao_investimento$conta_valor_inicial[i] /acao_investimento$valor_medio[i]
    acao_investimento$conta_valor_final[i] <- 0
  } else 
  {
    if (acao_investimento$eventos[i] == "desvalorização")
    {
      if (i == 1) 
      {
        acao_investimento$acoes_qtd[i] <- 0
        acao_investimento$conta_valor_final[i] <- acao_investimento$conta_valor_inicial[i]
        if (i < nrow(acao_investimento)) 
        {
          acao_investimento$conta_valor_inicial[i+1] <- acao_investimento$conta_valor_final[i]
        }
        
      } else 
      { if ( acao_investimento$conta_valor_inicial[i] != 0)
      {
        acao_investimento$conta_valor_final[i] <- acao_investimento$conta_valor_inicial[i]
      } else 
      {
        acao_investimento$acoes_qtd[i] <- 0
        acao_investimento$conta_valor_final[i] <- acao_investimento$valor_medio[i] * acao_investimento$acoes_qtd[i-1]
      }  
        if (i < nrow(acao_investimento)) 
        {
          acao_investimento$conta_valor_inicial[i+1] <- acao_investimento$conta_valor_final[i]
        }
      }  
    } else 
    {
      ifelse(i==1,acao_investimento$acoes_qtd[i],acao_investimento$acoes_qtd[i] <- acao_investimento$acoes_qtd[i-1])
      acao_investimento$conta_valor_final[i] <- acao_investimento$conta_valor_inicial[i]
      if (i < nrow(acao_investimento)) 
      {
        acao_investimento$conta_valor_inicial[i+1] <- acao_investimento$conta_valor_final[i]
      }  
    }
  }
}

if ((acao_investimento$conta_valor_final[nrow(acao_investimento)]) > acao_investimento$conta_valor_inicial[nrow(acao_investimento)])  
{
  valor_final_conta <- acao_investimento$conta_valor_final[nrow(acao_investimento)]+acao_investimento$valor_medio[nrow(acao_investimento)] * acao_investimento$acoes_qtd[nrow(acao_investimento)]  
} else {
  valor_final_conta <- acao_investimento$conta_valor_inicial[nrow(acao_investimento)]+acao_investimento$valor_medio[nrow(acao_investimento)] * acao_investimento$acoes_qtd[nrow(acao_investimento)]    
}

valor_inicial_conta <- acao_investimento$conta_valor_inicial[1]
valor_inicial_acao <- acao_investimento$valor_medio[1]
# valor_final_conta <- acao_investimento$conta_valor_inicial[nrow(acao_investimento)]+acao_investimento$valor_medio[nrow(acao_investimento)] * acao_investimento$acoes_qtd[nrow(acao_investimento)]
valor_final_acao <- acao_investimento$valor_medio[nrow(acao_investimento)]
valor_final_sem_tecnica <- acao_investimento$conta_valor_inicial[1] / acao_investimento$valor_medio[1] * acao_investimento$valor_medio[nrow(acao_investimento)]

cat(
  " ============================================\n",
  "    Resultados Financeiros do Modelo \n",
  "============================================\n",
  " Acão negociada:  ", lista_acoes[acao_selecionada],"\n",
  " Data início:     ", (as.character(acao_investimento$Data[1], format = c("%Y-%m-%d"))),"\n",
  " Data final:      ", (as.character(acao_investimento$Data[nrow(acao_investimento)], format = c("%Y-%m-%d"))),"\n",
  "\n",
  " Valor inicial médio de cada ação: ",format(valor_inicial_acao, digits=3, nsmall = 2),"\n",
  " Valor inicial investido:          ",format(valor_inicial_conta, digits=3, nsmall = 2),"\n",
  "\n",
  " Valor final utilizando o modelo:  ",format(valor_final_conta,digits=3, nsmall = 2),"\n",
  " Valor final sem usar o modelo:    ",format(valor_final_sem_tecnica, digits=3, nsmall = 2),"\n",
  " Valor final médio de cada ação:   ",format(valor_final_acao,digits=3, nsmall = 2),"\n",
  "\n",
  "Beneficio utilizando o modelo:     ",format(((valor_final_conta / valor_final_sem_tecnica)-1)*100, digits=3, nsmall = 1),"% \n"
)


