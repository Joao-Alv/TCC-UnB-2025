library(PortfolioAnalytics)
library(quantmod)
library(ROI.plugin.quadprog)
library(ROI.plugin.deoptim)
library(ggplot2)
library(tidyr)
library(xts)
library(dplyr)

fonte <- 'yahoo'                  ## fonte dos dados: banco de dados do yahoo
inicial <- as.Date('2015-01-01')  ## data inicial
final <- as.Date('2025-06-03')

################################################## pegando os dados #######################################################################################################################################################
getSymbols("PETR4.SA", src=fonte, from=inicial, to = final)

getSymbols("ITUB4.SA", src=fonte, from=inicial, to = final)

getSymbols("VALE3.SA", src=fonte, from=inicial, to = final)

getSymbols("BBDC3.SA", src=fonte, from=inicial, to = final)

getSymbols("ABEV3.SA", src=fonte, from=inicial, to = final)

getSymbols("COGN3.SA", src=fonte, from=inicial, to = final) #kroton

getSymbols("BRFS3.SA", src=fonte, from=inicial, to = final) #empresa de alimento

getSymbols("BOVA11.SA", src=fonte, from=inicial, to = final) 

getSymbols("BRL=X", src=fonte, from=inicial, to = final)  # cambio dolar/real

getSymbols("LFTS11.SA", src=fonte, from=inicial, to = final) #LFTS11 é um ETF, um fundo de indice listado na B3 que acompanha a performance de títulos públicos pós fixados ligados a selic  https://www.investoetf.com/etf/lfts11/

###########################################################################################################################################################################################################################


####################################################### CALC RETORNOS #####################################################################################################################################################

petro <- PETR4.SA$PETR4.SA.Adjusted
petro_retorno <- dailyReturn(petro, type = 'log')

itau <- ITUB4.SA$ITUB4.SA.Adjusted
itau_retorno <- dailyReturn(itau, type = 'log')

vale <- VALE3.SA$VALE3.SA.Adjusted
vale_retorno <- dailyReturn(vale, type = 'log')

bradesco <- BBDC3.SA$BBDC3.SA.Adjusted
bradesco_retorno <- dailyReturn(bradesco, type = 'log')

ambev <- ABEV3.SA$ABEV3.SA.Adjusted
ambev_retorno <- dailyReturn(ambev, type = 'log')

kroton <- COGN3.SA$COGN3.SA.Adjusted
kroton_retorno <- dailyReturn(kroton, type = 'log')

brf <- BRFS3.SA$BRFS3.SA.Adjusted
brf_retorno <- dailyReturn(brf, type = 'log')

bova11 <- BOVA11.SA$BOVA11.SA.Adjusted
bova11_retorno <- dailyReturn(bova11, type = 'log')

cambio <- `BRL=X`$`BRL=X.Adjusted`
cambio_retorno <- dailyReturn(cambio, type = 'log')

#selic <- LFTS11.SA$LFTS11.SA.Adjusted
#selic_retorno <- dailyReturn(selic, type = 'log')

###########################################################################################################################################################################################################################

############################################################ fazendo o portifolio e o loop #######################################################################################################

preço_portifolio <- cbind(petro_retorno, itau_retorno, vale_retorno, bradesco_retorno, ambev_retorno, kroton_retorno, brf_retorno, bova11_retorno, cambio_retorno)

nomes <- c('petro','itau','vale','bradesco','ambev','kroton','brf', 'bova11', 'cambio')

colnames(preço_portifolio) <- nomes

preço_portifolio <- na.omit(preço_portifolio)

portf <- portfolio.spec(colnames(preço_portifolio))

portf <- add.constraint(portf, type = "weight_sum", min_sum=0.99, max_sum=1.01)
portf <- add.constraint(portf, type = 'box', min=0.01, max = .99)
portf <- add.objective(portf, type = "return", name = "mean")
portf <- add.objective(portf, type = 'risk', name = "StdDev")


optPort <- optimize.portfolio(preço_portifolio, portf, optimize_method = 'ROI') # para anualizar o retorno multiplique por 252, para anualizar o risco multiplqiue pela raiz(252)


optLoop <- optimize.portfolio.rebalancing(preço_portifolio, portf, optimize_method = "DEoptim", rebalance_on = "months", training_period=1, rolling_window=1)
#Rebalanceamento Mensal: No último dia útil de cada mês, o portfólio é rebalanceado
#Janela de Treinamento: Usa apenas 1 período (1 mês) de dados para cada otimização
#DEoptim: Algoritmo de otimização evolucionária (útil para problemas não-convexos)
#optLoop <- optimize.portfolio.rebalancing(preço_portifolio, portf, optimize_method = "DEoptim", rebalance_on = "months")

optLoop$opt_rebalancing

optLoop$opt_rebalancing$`2015-01-30`$objective_measures$mean

optLoop$opt_rebalancing$`2015-02-27`$objective_measures$mean

# Extrai todas as datas do rebalanceamento
datas <- names(optLoop$opt_rebalancing)

# Pega o retorno médio para cada data
retornos_medios <- sapply(datas, function(d) optLoop$opt_rebalancing[[d]]$objective_measures$mean)

# Exibe os retornos médios por data
print(retornos_medios)

# criando um df com os retornos para cada mês
df_retorno <- data.frame(Data = datas, Retorno_Medio = retornos_medios)
print(df_retorno)

# gráfico dos retornos

library(ggplot2)

# Criando um dataframe com os retornos médios
df_retorno <- data.frame(Data = as.Date(datas), Retorno_Medio = retornos_medios)

# Plotando com ggplot2
ggplot(df_retorno, aes(x = Data, y = Retorno_Medio)) +
  geom_line(color = "#9bbef3", size = 1) +  # Linha azul
  geom_point(color = "#ed8bdd", size = 2) +  # Pontos vermelhos para destacar os retornos
  labs(title = "Evolução dos Retornos Médios do Portfólio",
       x = "Data",
       y = "Retorno Médio",
       caption = "Fonte: Cálculos do Portfólio") +
  theme_minimal()  # Estilo mais clean

# Calcular retorno acumulado
df_retorno$Retorno_Acumulado <- cumprod(1 + df_retorno$Retorno_Medio) - 1

# Criando o gráfico do retorno acumulado
ggplot(df_retorno, aes(x = Data, y = Retorno_Acumulado)) +
  geom_line(color = "#9bbef3", size = 1.2) +  # Linha azul escura
  labs(title = "Evolução do Retorno Acumulado do Portfólio",
       x = "Data",
       y = "Retorno Acumulado",
       caption = "Fonte: Cálculos do Portfólio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona rótulos do eixo X para melhor visualização


rebal_weights <- extractWeights(optLoop)
rebal_returns <- Return.portfolio(preço_portifolio, weights = rebal_weights)

rets_df <- cbind(rebal_returns)
charts.PerformanceSummary(rets_df, main = 'Performance do Portfólio')

###########################################################################################################################################################################################################################



#teste
ibovespapreço <- getSymbols.yahoo("^BVSP", from = inicial, periodicity = 'daily', auto.assign=FALSE)[,4]
ibovespa <- na.omit(ROC(ibovespapreço))
ibovespa <- as.xts(ibovespa)


# evolução gráfico dos pesos do meu portfolio
# Transforma os pesos em dataframe
weights_df <- do.call(rbind, lapply(optLoop$opt_rebalancing, function(x) {
  data.frame(t(x$weights))
}))

# Gráfico de evolução
weights_df %>% 
  mutate(Date = as.Date(row.names(.))) %>% 
  gather(Ativo, Peso, -Date) %>% 
  ggplot(aes(x = Date, y = Peso, color = Ativo)) +
  geom_line() +
  labs(title = "Evolução dos Pesos do Portfólio")

# comparar a perfomance
# Retornos do portfólio ótimo
port_returns <- Return.portfolio(preço_portifolio, weights = rebal_weights)

# Comparação com IBOV
comparison <- cbind(port_returns, bova11_retorno[index(port_returns)])
charts.PerformanceSummary(comparison)




##### teste ##################################################################################################################################################################
nomes <- c('Petrobras', 'Itau', 'Vale', 'Bradesco', 'Ambev', 'Kroton', 'BRF', 'Bova11', 'Cambio')

acoes <- cbind(petro, itau, vale, bradesco, ambev, kroton, brf, bova11, cambio)

autoplot(acoes, facet = NULL) + xlab("Tempo") + labs(colour = "Ações") + ylab("Preço de Fechamento") + 
  scale_color_manual(labels = nomes, values = c('#35a9d8', '#35d87d','#ab39e1', '#bb289c','#2841bb', '#28bbb4', '#bb8128', '#c6d810', '#d8103a', '#20e701')) + 
  theme_minimal()
###############################################################################################################################################################################



###### DCC GARCH #########################################################################################################################################################################

# Preparação dos Pacotes e Dados
library(rugarch)
library(rmgarch)

# Usando os retornos calculados
retornos <- preço_portifolio  

#Especificação do Modelo Univariado GARCH
# Especificação GARCH para cada série (usando distribuição t-Student)
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"  # Distribuição t-Student para capturar caudas pesadas
)

#aplicando o modelo feito acima nos retornos das ações (exemplo)
ugfit_petro <- ugarchfit(spec = garch_spec, data = retornos$petro) # aplicando nos retornos da petrobras
ugfit_petro

#extraindo as informações (estimativas dos param, seus erros ou os residuos)
names(ugfit_petro@model) #informaçoes
names(ugfit_petro@fit) #informações
ugfit_petro@fit$coef #coeficientes estimados
ug_var <- ugfit_petro@fit$var # variâncias condicionais estimadas
ug_res2 <- (ugfit_petro@fit$residuals)^2 # residuos ao quadrado

##### plotando os residuos e as variâncias condicionais estimadas
plot(ug_res2, type = "l")
lines(ug_var, col = "green")



# Especificação do Modelo DCC-GARCH
# Especificação DCC com dist t multivariada
dcc_spec <- dccspec(
  uspec = multispec(replicate(ncol(retornos), garch_spec)),  # Aplica GARCH a todas séries (replica oque eu fiz acima para todas as series)
  dccOrder = c(1, 1),  # Ordem (1,1) para o modelo DCC
  distribution = "mvt"  # Distribuição t multivariada
)

#dcc_fit@mfit$coef todos os coeficientes dos garch univariado

# proximo passo: Ajuste do Modelo
# Ajustar o modelo DCC-GARCH (pode demorar alguns minutos)
dcc_fit <- dccfit(
  dcc_spec,
  data = retornos,
  fit.control = list(eval.se = TRUE)  # Calcula erros padrão
)

# Ver resultados
print(dcc_fit)

#lopes <- round(dcc_fit@mfit$matcoef, 6) #teste

# Extração e Visualização dos Resultados
# Extrair correlações e cov condicionais
cov1 <- rcov(dcc_fit) #extrai a maitriz de convariâncias
cor1 <- rcor(dcc_fit) #extrai a maitrz de correlação
dim(cov1) #vendo as dimeções temos 9x9x2237 ou seja, uma matriz 9x9 dos ativos para cada um dos 2237 dias

dcc_cor <- rcor(dcc_fit)
#dimnames(dcc_cor) <- list(NULL, colnames(retornos), colnames(retornos))

# Visualizar correlação entre dois ativos (ex: Petrobras e Vale)
cor_petro_vale <- xts(dcc_cor[1, 3, ], index(retornos))
plot(cor_petro_vale, main = "Correlação Dinâmica PETR4-VALE3")


library(psych)

# analise de residuos
residuos <- dcc_fit@mfit$stdresid
colnames(residuos) <- nomes

#Média próxima de zero
#Variância aproximadamente 1
#Distribuição sem padrões evidentes

# Teste para autocorrelação nos resíduos (atraso 10)
Box.test(residuos[,1], lag = 10, type = "Ljung-Box") #Valor-p > 0.05 indica ausência de autocorrelação

# Teste para autocorrelação nos quadrados dos resíduos (atraso 10)
Box.test(residuos[,1]^2, lag = 10, type = "Ljung-Box")

#Visualização Gráfica dos Resíduos
par(mfrow = c(2,1))
acf(residuos[,1], main = "ACF dos Resíduos Padronizados")
acf(residuos[,1]^2, main = "ACF dos Resíduos ao Quadrado")
par(mfrow = c(1,1))
# espero Nenhuma barra significativa na ACF dos resíduos
# e Nenhuma barra significativa na ACF dos quadrado

#QQ plots
qqnorm(residuos[,1], main = "QQ-Plot dos Resíduos")
qqline(residuos[,1])
# Pontos próximos à linha indicam aderência à distribuição especificada
# Caudas pesadas serão visíveis como desvios nas extremidades

#par(mfrow = c(3,3))
#acf(residuos[,1]^2, main = "ACF dos Resíduos Padronizados Petrobras")
#acf(residuos[,2]^2, main = "ACF dos Resíduos Padronizados Itau")
#acf(residuos[,3]^2, main = "ACF dos Resíduos Padronizados Vale")
#acf(residuos[,4]^2, main = "ACF dos Resíduos Padronizados Bradesco")
#acf(residuos[,5]^2, main = "ACF dos Resíduos Padronizados Ambev")
#acf(residuos[,6]^2, main = "ACF dos Resíduos Padronizados Kroton")
#acf(residuos[,7]^2, main = "ACF dos Resíduos Padronizados BRF")
#acf(residuos[,8]^2, main = "ACF dos Resíduos Padronizados Bova11")
#acf(residuos[,9]^2, main = "ACF dos Resíduos Padronizados Cambio")


#par(mfrow = c(3,3))
#qqnorm(residuos[,1], main = "QQ-Plot dos Resíduos Petrobras")
#qqline(residuos[,1])

#qqnorm(residuos[,1], main = "QQ-Plot dos Resíduos Itau")
#qqline(residuos[,1])

#qqnorm(residuos[,1], main = "QQ-Plot dos Resíduos Vale")
#qqline(residuos[,1])

#qqnorm(residuos[,1], main = "QQ-Plot dos Resíduos Bradesco")
#qqline(residuos[,1])

#qqnorm(residuos[,1], main = "QQ-Plot dos Resíduos Ambev")
#qqline(residuos[,1])

#qqnorm(residuos[,1], main = "QQ-Plot dos Resíduos Kroton")
#qqline(residuos[,1])

#qqnorm(residuos[,1], main = "QQ-Plot dos Resíduos BRF")
#qqline(residuos[,1])

#qqnorm(residuos[,1], main = "QQ-Plot dos Resíduos Bova11")
#qqline(residuos[,1])

#qqnorm(residuos[,1], main = "QQ-Plot dos Resíduos Cambio")
#qqline(residuos[,1])

#par(mfrow = c(1,1))

Box.test(residuos[,1]^2, lag = 10, type = "Ljung-Box")
Box.test(residuos[,2]^2, lag = 10, type = "Ljung-Box")
Box.test(residuos[,3]^2, lag = 10, type = "Ljung-Box")
Box.test(residuos[,4]^2, lag = 10, type = "Ljung-Box")
Box.test(residuos[,5]^2, lag = 10, type = "Ljung-Box")
Box.test(residuos[,6]^2, lag = 10, type = "Ljung-Box")
Box.test(residuos[,7]^2, lag = 10, type = "Ljung-Box")
Box.test(residuos[,8]^2, lag = 10, type = "Ljung-Box")
Box.test(residuos[,9]^2, lag = 10, type = "Ljung-Box")


#lopes2 <- describe(residuos)
#write.csv(lopes2, "lopes2.csv")
#############################################################################################################################################################


############################################################################################################################################################
# INTEGRANDO COM A ANALISE DE PORTIFLIO

# Obter a matriz de covariância condicional
dcc_cov <- rcov(dcc_fit)

# Usar a última covariância estimada para otimização
ultima_cov <- dcc_cov[,,dim(dcc_cov)[3]]

# Modificar a especificação de portfólio
portf_dcc <- add.constraint(portf, type = "risk", # add.constraint() modifica a especificação do portfólio type = "risk" indica que estamos adicionando uma restrição de risco
                            risk = "covariance",  # risk = "covariance" especifica que usaremos uma matriz de covariância
                            cov_matrix = ultima_cov) #cov_matrix = ultima_cov fornece a matriz DCC-GARCH

# testando para ver se funcionou

# Peso médio usando covariância histórica
pesos_classico <- extractWeights(optPort)

# Peso médio usando DCC-GARCH
pesos_dcc <- extractWeights(optimize.portfolio(retornos, portf_dcc))

# Comparação entre os pesos
data.frame(Ativo = colnames(retornos),
           Clássico = round(pesos_classico, 4),
           DCC = round(pesos_dcc, 4))


# comparação gráfica dos pesos
# Otimização com DCC
opt_dcc <- optimize.portfolio(retornos, portf_dcc)

# Extrair pesos
pesos_dcc <- extractWeights(opt_dcc)

# Comparar com otimização clássica
pesos_classico <- extractWeights(optPort)

df_pesos <- data.frame(ativo = factor(rep(colnames(retornos),2)), abordagem = rep(c("classica","DCC-GARCH"), each = ncol(retornos)), peso = c(pesos_classico, pesos_dcc))


ggplot(df_pesos, aes(x = ativo, y=peso, fill = abordagem)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Comparação de Alocações: Abordagem Clássica vs DCC-GARCH") +
  theme_minimal()

##############################################################################################################################################################################


############# LOOP FEITO NO MODELO CLASSICO MAS AGORA COM AS MATRIZES DE COVARIÂNCIA DO MODELO DCC ###############################################################################

# Passo 1: Criar Função para Atualizar a Matriz de Covariância
# Função que atualiza a matriz de covariância do portfólio com base no DCC-GARCH
atualizar_cov_dcc <- function(dados, dcc_fit, data_rebalanceamento) {
  # Encontrar o índice correspondente a data de rebalanceamento
  idx <- which(index(dados) == data_rebalanceamento)
  
  # Extrair a matriz de covariância condicional para aquela data específica
  dcc_cov <- rcov(dcc_fit)
  cov_matrix <- dcc_cov[,,idx]
  
  return(cov_matrix)
}


# Passo 2: Modificar a Especificação do Portfólio
# Criar uma cópia do portfólio original
portf_dcc <- portf

# Remover a restrição de risco padrão (será substituída pela do DCC)
portf_dcc$constraints <- portf_dcc$constraints[sapply(portf_dcc$constraints, function(x) x$type != "risk")]


# Passo 3: Implementar o Loop de Rebalanceamento ManuaL
# Definir as datas de rebalanceamento (último dia de cada mês)
datas_rebalanceamento <- endpoints(preço_portifolio, on = "months")[-1]

# Lista para armazenar resultados
resultados_dcc <- list()

# Loop pelas datas de rebalanceamento
for(i in seq_along(datas_rebalanceamento)) {
  data_rebal <- index(preço_portifolio)[datas_rebalanceamento[i]]
  cat("Processando:", as.character(data_rebal), "\n")
  
  # 1. Obter matriz de covariância DCC para esta data
  cov_dcc <- atualizar_cov_dcc(preço_portifolio, dcc_fit, data_rebal)
  
  # 2. Atualizar a restrição de covariância no portfólio
  portf_atual <- add.constraint(portf_dcc, 
                                type = "risk", 
                                risk = "covariance", 
                                cov_matrix = cov_dcc)
  
  # 3. Dados de treinamento (usando janela de 12 meses/mudar para 1)
  dados_treino <- preço_portifolio[paste0(data_rebal - 365, "::", data_rebal)]
  
  # 4. Otimizar o portfólio
  opt <- optimize.portfolio(dados_treino, 
                            portf_atual, 
                            optimize_method = "DEoptim",
                            trace = FALSE)
  
  # 5. Armazenar resultados
  resultados_dcc[[as.character(data_rebal)]] <- list(
    weights = extractWeights(opt),
    objective_measures = opt$objective_measures,
    cov_matrix = cov_dcc
  )
}

resultados_dcc$`2015-01-30`  #um exemplo de um rebalanceamento com pesos dos ativos e a matriz de covariância


# Passo 4: Processar os Resultados
# Extrair pesos para cada rebalanceamento
weights_dcc <- do.call(rbind, lapply(resultados_dcc, function(x) x$weights))

# Converter para xts
weights_dcc_xts <- xts(weights_dcc, as.Date(names(resultados_dcc)))

# Calcular retornos do portfólio
returns_dcc <- Return.portfolio(preço_portifolio, weights = weights_dcc_xts)

# Comparar com abordagem original
comparison <- cbind(returns_dcc, rebal_returns)
colnames(comparison) <- c("DCC-GARCH", "Original")
charts.PerformanceSummary(comparison)


#Passo 5: Análise dos Resultados
# Estatísticas de performance
table.AnnualizedReturns(comparison)

# Turnover comparativo
turnover_dcc <- sum(abs(diff(weights_dcc_xts)))
turnover_original <- sum(abs(diff(rebal_weights)))
cat("Turnover DCC:", turnover_dcc, "\nTurnover Original:", turnover_original)

# Visualizar evolução dos pesos
plot(weights_dcc_xts, main = "Pesos com DCC-GARCH")

############################################################################################################################################################


##################### TUDO O QUE EU FIZ MAS AGORA USANDO O PACOTE 'PARMA' #####################################################################################
library(parma)

# vamos começar com montando o framework basico clássico de markowitz

# Dados de retorno (usando seus dados anteriores)
retornos <- preço_portifolio  # Matriz de retornos logarítmicos

# Especificação do problema
spec <- parmaspec(
  scenario = retornos,
  forecast = colMeans(retornos),  # Retornos esperados
  risk = "EV",                   # Risco de variância (Efficient Variance)
  target = NULL,                 # Sem retorno alvo (minimizar risco)
  riskType = "minrisk",          # Minimizar o risco
  LB = rep(0.01, ncol(retornos)), # Peso mínimo de 1% por ativo
  UB = rep(0.50, ncol(retornos))  # Peso máximo de 50% por ativo
)

# Resolver
sol_mv <- parmasolve(spec)
weights_mv <- weights(sol_mv)  




### agora integrando com o DCC-GARCH

# Extrair a última matriz de covariância do DCC-GARCH
dcc_cov <- rcov(dcc_fit)[,,dim(rcov(dcc_fit))[3]]

# Especificar otimização no parma usando a matriz DCC
spec_dcc <- parmaspec(
  S = dcc_cov,                   # Matriz de covariância DCC
  forecast = colMeans(retornos),
  risk = "EV",
  target = 0.001,                # Retorno alvo diário (0.1%)
  riskType = "optimal",          # Busca trade-off risco-retorno
  LB = rep(0.01, ncol(retornos)),
  UB = rep(0.50, ncol(retornos))
)

sol_dcc <- parmasolve(spec_dcc)
weights_dcc <- weights(sol_dcc)  


#### comparação gráfico dos pesos dos ativos

# 3. Preparar dados para visualização
df_weights <- data.frame(
  Ativo = colnames(retornos),
  Média_Variância = weights_mv,
  DCC_GARCH = weights_dcc
)

# 4. Gráfico de comparação
ggplot(df_weights, aes(x = Ativo)) +
  geom_segment(aes(xend = Ativo, y = Média_Variância, yend = DCC_GARCH),
               color = "gray", size = 1.5, alpha = 0.7) +
  geom_point(aes(y = Média_Variância, color = "Média-Variância"), size = 4) +
  geom_point(aes(y = DCC_GARCH, color = "DCC-GARCH"), size = 4) +
  scale_color_manual(values = c("Média-Variância" = "#F8766D", 
                                "DCC-GARCH" = "#00BFC4")) +
  labs(title = "Comparação dos Pesos dos Ativos",
       subtitle = "Otimização Média-Variância vs DCC-GARCH",
       y = "Pesos", x = "",
       color = "Método") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))




# barras lado a lado
df_long <- pivot_longer(df_weights, cols = -Ativo, names_to = "Modelo", values_to = "Peso")

ggplot(df_long, aes(x = Ativo, y = Peso, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Média_Variância" = "#F8766D", 
                               "DCC_GARCH" = "#00BFC4")) +
  labs(title = "Distribuição de Pesos por Modelo",
       y = "Alocação", x = "",
       fill = "Modelo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#############################################################################################################################################################


###################################################### FAZENDO O LOOP MAS DESSA VEZ UTILOZANDO O PACOTE PARMA ###############################################

## Passo 1: Preparação dos dados (usando os dados anteriores)
retornos <- preço_portifolio  # retornos logarítmicos
datas_rebalanceamento <- endpoints(retornos, on = "months")[-1]  # Mensal

## Passo 2: Configuração do DCC-GARCH (como eu fiz acima)
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(0,0)),
                         distribution.model = "std")

dcc_spec <- dccspec(uspec = multispec(replicate(ncol(retornos), garch_spec)), 
                    dccOrder = c(1,1), 
                    distribution = "mvt")

dcc_fit <- dccfit(dcc_spec, data = retornos)


rebalanceamento_parma_correto <- function(retornos, dcc_fit, datas_rebalanceamento, janela_treino = 12) {
  
  resultados <- list()
  
  for(i in seq_along(datas_rebalanceamento)) {
    data_atual <- index(retornos)[datas_rebalanceamento[i]]
    cat("Processando:", as.character(data_atual), "\n")
    
    # 1. Dados de treino (janela móvel)
    posicao_atual <- which(index(retornos) == data_atual)
    inicio_treino <- index(retornos)[max(1, posicao_atual - janela_treino*21)]
    dados_treino <- window(retornos, start = inicio_treino, end = data_atual)
    
    # 2. Matriz de covariância DCC para a data atual
    cov_dcc <- rcov(dcc_fit)[,,posicao_atual]
    
    # 3. Especificação parma com restrições
    spec <- parmaspec(
      S = cov_dcc,
      forecast = colMeans(dados_treino, na.rm = TRUE),
      risk = "EV",
      target = NULL,
      riskType = "minrisk",
      LB = rep(0.01, ncol(retornos)),
      UB = rep(0.50, ncol(retornos)),
      budget = 1 #soma dos pesos = 1
    )
    
    # 4. Otimização
    sol <- tryCatch(
      parmasolve(spec),
      error = function(e) {
        cat("Erro na data", data_atual, ":", e$message, "\n")
        return(NULL)
      }
    )
    
    if(!is.null(sol)) {
      # atencao: Usar as funções corretas do parma
      resultados[[as.character(data_atual)]] <- list(
        weights = weights(sol),
        risk = parmarisk(sol),  # Função correta para extrair o risco
        return = t(colMeans(dados_treino)) %*% weights(sol)
      )
    }
  }
  
  return(resultados)
}

# Execução
resultados <- rebalanceamento_parma_correto(retornos, dcc_fit, datas_rebalanceamento)

## Passo 4: Executar o rebalanceamento NAO USAR ISSO DAKI RETIREI O OBJETO (fica para teste)
#resultados_parma <- rebalanceamento_parma(retornos, dcc_fit, datas_rebalanceamento)

processar_resultados <- function(resultados) {
  datas <- names(resultados)
  pesos <- do.call(rbind, lapply(resultados, function(x) x$weights))
  riscos <- sapply(resultados, function(x) if(!is.null(x$risk)) x$risk else NA)
  retornos <- sapply(resultados, function(x) if(!is.null(x$return)) x$return else NA)
  
  list(
    weights = xts(pesos, as.Date(datas)),
    risk = xts(riscos, as.Date(datas)),
    return = xts(retornos, as.Date(datas))
  )
}

dados_finais <- processar_resultados(resultados)

# Verifique as primeiras linhas
head(dados_finais$weights)
head((dados_finais$risk))
head(dados_finais$return)



# Evolução dos pesos
matplot(dados_finais$weights, type = "l", lty = 1, main = "Evolução dos Pesos")
legend("topright", legend = colnames(retornos), col = 1:ncol(retornos), lty = 1)


# comparação de desempenho retornos do portifolio vs ibovespa
retornos_portfolio <- Return.portfolio(retornos, weights = dados_finais$weights)
comparacao <- merge(retornos_portfolio, bova11_retorno[index(retornos_portfolio)])
charts.PerformanceSummary(comparacao)


#############################################################################################################################################################


################################## comparação abordagem parma-DCC com markowitz clássico ####################################################################

library(reshape2)
library(ggplot2)
library(PerformanceAnalytics)
library(gridExtra)

## 1. Preparar os dados para comparação
# Resultados do método (parma + DCC)
weights_parma <- dados_finais$weights
returns_parma <- Return.portfolio(retornos, weights = weights_parma)
colnames(returns_parma) <- "DCC-GARCH"

# Resultados da abordagem original (optLoop) feito no inicio
returns_original <- rebal_returns  # já fiz esse objeto
colnames(returns_original) <- "Clássica"

# Combinar os resultados
comparison <- merge(returns_parma, returns_original, bova11_retorno[index(returns_parma)])
colnames(comparison) <- c("DCC-GARCH", "Clássica", "IBOVESPA")

## 2. Gráfico de Desempenho Comparado
charts.PerformanceSummary(comparison, 
                          colorset = c("#1f77b4", "#ff7f0e", "#2ca02c"),
                          main = "Comparação de Desempenho: DCC-GARCH vs Clássica vs IBOVESPA")

## 3. Comparação dos Pesos (Gráfico de Área)
# Preparar dados
weights_comparison <- merge.xts(weights_parma, rebal_weights)
colnames(weights_comparison) <- c(paste0(colnames(retornos), "_PARMA"),
                                  paste0(colnames(retornos), "_Original"))

# Gráfico para PARMA-DCC
p1 <- autoplot(weights_parma, facet = NULL) +
  geom_area(aes(fill = Series), alpha = 0.8) +
  labs(title = "Alocação DCC-GARCH", x = "", y = "Peso") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "bottom")

# Gráfico para abordagem Original
p2 <- autoplot(rebal_weights, facet = NULL) +
  geom_area(aes(fill = Series), alpha = 0.8) +
  labs(title = "Alocação Clássica", x = "", y = "Peso") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "bottom")

# Exibir lado a lado
grid.arrange(p1, p2, ncol = 1)

## 4. Diferença Percentual nos Pesos (Heatmap)
diff_weights <- weights_parma - rebal_weights[index(weights_parma),]

ggplot(melt(fortify(diff_weights), id.vars = "Index"), 
       aes(x = Index, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, limits = c(-0.3, 0.3)) +
  labs(title = "Diferença nos Pesos: DCC-GARCH vs Clássica",
       x = "", y = "", fill = "Diferença") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 5. Métricas Quantitativas Comparadas
metrics <- rbind(
  table.AnnualizedReturns(comparison),
  table.DownsideRisk(comparison),
  maxDrawdown(comparison)
)

metrics

## 6. Turnover Comparativo
calculate_turnover <- function(weights) {
  sum(abs(diff(weights)), na.rm = TRUE) / nrow(weights)
}

turnover <- data.frame(
  Método = c("PARMA-DCC", "Original"),
  Turnover = c(calculate_turnover(weights_parma),
               calculate_turnover(rebal_weights))
)

ggplot(turnover, aes(x = Método, y = Turnover, fill = Método)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparação de Turnover", y = "Turnover Médio Mensal") +
  theme_minimal()




#### gráfico de correlação junto com alocação de pesos 

colnames(weights_parma)


# 1. Extrair correlações DCC (ATENCAO USAR NOMES CORRETOS SE N DA ERRADO)
cor_petro_vale <- xts(rcor(dcc_fit)["petro", "vale", ], index(retornos))

# 2. Preparar dados para ggplot
df_cor <- data.frame(
  Data = index(cor_petro_vale),
  Correlacao = as.numeric(cor_petro_vale)
)

df_weights <- data.frame(
  Data = index(weights_parma),
  Petrobras = weights_parma[,"petro"],  # Usando o nome correto
  Vale = weights_parma[,"vale"]         # Usando o nome correto
)

# 3. Plot integrado
ggplot() +
  geom_line(data = df_cor, 
            aes(x = Data, y = Correlacao, color = "Correlação PETR-VALE") 
  ) +
  geom_line(data = df_weights, 
            aes(x = Data, y = petro, color = "Peso PETR") 
  ) +
  geom_line(data = df_weights, 
            aes(x = Data, y = vale, color = "Peso VALE"), 
  ) +
  scale_y_continuous(
    name = "Pesos",
    sec.axis = sec_axis(~., name = "Correlação")
  ) +
  scale_color_manual(values = c(
    "Correlação PETR-VALE" = "black",
    "Peso PETR" = "blue",
    "Peso VALE" = "red"
  )) +
  labs(title = "Relação entre Correlação e Alocação",
       x = "Data",
       color = "Série") +
  theme_minimal() +
  theme(legend.position = "bottom")

######################################################################################################################################################################



########################################### METODOLOGIA #####################################################################################################


################## gráfico valores de fechamento #######################################################

# Gráfico 1 - Petrobras
p_petro <- ggplot(data = fortify(acoes$PETR4.SA.Adjusted), 
                  aes(x = Index, y = PETR4.SA.Adjusted)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "PETROBRAS", 
       subtitle = "Valores de Fechamento Ajustado",
       y = "Valores de Fechamento",
       x = "Data") +
  theme_bw()



# Gráfico 2 - Vale
p_vale <- ggplot(data = fortify(acoes$VALE3.SA.Adjusted), 
                 aes(x = Index, y = VALE3.SA.Adjusted)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "VALE", 
       subtitle = "Valores de Fechamento Ajustado",
       y = "Valores de Fechamento",
       x = "Data") +
  theme_bw()



# Gráfico 3 - itau
p_itau <- ggplot(data = fortify(acoes$ITUB4.SA.Adjusted), 
                 aes(x = Index, y = ITUB4.SA.Adjusted)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Itau Unibanco", 
       subtitle = "Valores de Fechamento Ajustado",
       y = "Valores de Fechamento",
       x = "Data") +
  theme_bw()


# Gráfico 4 - bradesco
p_bradesco <- ggplot(data = fortify(acoes$BBDC3.SA.Adjusted), 
                     aes(x = Index, y = BBDC3.SA.Adjusted)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Bradesco", 
       subtitle = "Valores de Fechamento Ajustado",
       y = "Valores de Fechamento",
       x = "Data") +
  theme_bw()


# Gráfico 5 - ambev
p_ambev <- ggplot(data = fortify(acoes$ABEV3.SA.Adjusted), 
                  aes(x = Index, y = ABEV3.SA.Adjusted)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "AMBEV S.A", 
       subtitle = "Valores de Fechamento Ajustado",
       y = "Valores de Fechamento",
       x = "Data") +
  theme_bw()


# Gráfico 6 - kroton
p_kroton <- ggplot(data = fortify(acoes$COGN3.SA.Adjusted), 
                   aes(x = Index, y = COGN3.SA.Adjusted)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Kroton Educacional", 
       subtitle = "Valores de Fechamento Ajustado",
       y = "Valores de Fechamento",
       x = "Data") +
  theme_bw()

# Gráfico 7 - BRF
p_brf <- ggplot(data = fortify(acoes$BRFS3.SA.Adjusted), 
                aes(x = Index, y = BRFS3.SA.Adjusted)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "BRF S.A", 
       subtitle = "Valores de Fechamento Ajustado",
       y = "Valores de Fechamento",
       x = "Data") +
  theme_bw()



# Gráfico 8 - bova11
p_bova11 <- ggplot(data = fortify(acoes$BOVA11.SA.Adjusted), 
                   aes(x = Index, y = BOVA11.SA.Adjusted)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "ETF Bova11", 
       subtitle = "Valores de Fechamento Ajustado",
       y = "Valores de Fechamento",
       x = "Data") +
  theme_bw()


# Gráfico 9 - cambio
p_cambio <- ggplot(data = fortify(acoes$BRL.X.Adjusted), 
                   aes(x = Index, y = BRL.X.Adjusted)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Dólar Comercial", 
       subtitle = "Valores de Fechamento Ajustado",
       y = "Valores de Fechamento",
       x = "Data") +
  theme_bw()



# Combinar gráficos selecionados
library(patchwork)
(p_petro + p_vale + p_itau ) / 
  ( p_bradesco + p_kroton + p_brf) + 
  (p_cambio + p_bova11 + p_ambev ) + # LEMBRAR DE NA HORA Q FOR PASSAR PARA O DOC, COLOCAR OUTROS GRAFS
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')


p_petro
p_itau
p_vale
p_bradesco
p_ambev
p_kroton
p_brf
p_bova11
p_cambio


##############################################################################################################


############################### gráfico dos retornos #########################################################

# Gráfico 1 - Petrobras
p_petro <- ggplot(data = fortify(retornos$petro), 
                  aes(x = Index, y = petro)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "PETROBRAS", 
       subtitle = "Retornos Diários",
       y = "Retorno Logarítmico (%)",
       x = "Data") +
  theme_bw()



# Gráfico 2 - Vale
p_vale <- ggplot(data = fortify(retornos$vale), 
                 aes(x = Index, y = vale)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "VALE", 
       subtitle = "Retornos Diários",
       y = "Retorno Logarítmico (%)",
       x = "Data") +
  theme_bw()



# Gráfico 3 - itau
p_itau <- ggplot(data = fortify(retornos$itau), 
                 aes(x = Index, y = itau)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Itau Unibanco", 
       subtitle = "Retornos Diários",
       y = "Retorno Logarítmico (%)",
       x = "Data") +
  theme_bw()


# Gráfico 4 - bradesco
p_bradesco <- ggplot(data = fortify(retornos$bradesco), 
                     aes(x = Index, y = bradesco)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Bradesco", 
       subtitle = "Retornos Diários",
       y = "Retorno Logarítmico (%)",
       x = "Data") +
  theme_bw()


# Gráfico 5 - ambev
p_ambev <- ggplot(data = fortify(retornos$ambev), 
                  aes(x = Index, y = ambev)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "AMBEV S.A", 
       subtitle = "Retornos Diários",
       y = "Retorno Logarítmico (%)",
       x = "Data") +
  theme_bw()


# Gráfico 6 - kroton
p_kroton <- ggplot(data = fortify(retornos$kroton), 
                   aes(x = Index, y = kroton)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Kroton Educacional", 
       subtitle = "Retornos Diários",
       y = "Retorno Logarítmico (%)",
       x = "Data") +
  theme_bw()

# Gráfico 7 - BRF
p_brf <- ggplot(data = fortify(retornos$brf), 
                aes(x = Index, y = brf)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "BRF S.A", 
       subtitle = "Retornos Diários",
       y = "Retorno Logarítmico (%)",
       x = "Data") +
  theme_bw()



# Gráfico 8 - bova11
p_bova11 <- ggplot(data = fortify(retornos$bova11), 
                   aes(x = Index, y = bova11)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "ETF Bova11", 
       subtitle = "Retornos Diários",
       y = "Retorno Logarítmico (%)",
       x = "Data") +
  theme_bw()


# Gráfico 9 - cambio
p_cambio <- ggplot(data = fortify(retornos$cambio), 
                   aes(x = Index, y = cambio)) +
  geom_line(color = "#377eb8", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Dólar Comercial", 
       subtitle = "Retornos Diários",
       y = "Retorno Logarítmico (%)",
       x = "Data") +
  theme_bw()



# Combinar gráficos selecionados
library(patchwork)
(p_petro + p_vale + p_itau ) / 
  ( p_bradesco + p_kroton + p_brf) + 
  (p_cambio + p_bova11 + p_ambev ) + # LEMBRAR DE NA HORA Q FOR PASSAR PARA O DOC, COLOCAR OUTROS GRAFS
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')


p1 <- p_petro + geom_line(color = "#377eb8", linewidth = 0.7) + 
  
  annotate("rect", 
           xmin = as.Date("2020-01-01"), 
           xmax = as.Date("2020-12-12"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.1) +
  
  # Linha vertical e rótulo
  geom_vline(xintercept = as.Date("2020-03-11"), 
             color = "red", linetype = "dashed", linewidth = 0.5) +
  annotate("text", 
           x = as.Date("2020-04-01"), 
           y = max(preço_portifolio$petro) * 0.9,
           label = "Pandemia COVID-19",
           color = "red", angle = 0, size = 4.5) 




p2 <- p_itau + geom_line(color = "#377eb8", linewidth = 0.7) + 
  
  annotate("rect", 
           xmin = as.Date("2020-01-01"), 
           xmax = as.Date("2020-12-12"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.1) +
  
  # Linha vertical e rótulo
  geom_vline(xintercept = as.Date("2020-03-11"), 
             color = "red", linetype = "dashed", linewidth = 0.01) +
  annotate("text", 
           x = as.Date("2020-04-01"), 
           y = max(preço_portifolio$itau) * 0.9,
           label = "Pandemia COVID-19",
           color = "red", angle = 0, size = 4.5) 



p3 <- p_vale + geom_line(color = "#377eb8", linewidth = 0.7) + 
  
  annotate("rect", 
           xmin = as.Date("2020-01-01"), 
           xmax = as.Date("2020-12-12"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.1) +
  
  # Linha vertical e rótulo
  geom_vline(xintercept = as.Date("2020-03-11"), 
             color = "red", linetype = "dashed", linewidth = 0.5) +
  annotate("text", 
           x = as.Date("2020-04-01"), 
           y = max(preço_portifolio$vale) * 0.9,
           label = "Pandemia COVID-19",
           color = "red", angle = 0, size = 4.5) 





p4 <- p_bradesco + geom_line(color = "#377eb8", linewidth = 0.7) + 
  
  annotate("rect", 
           xmin = as.Date("2020-01-01"), 
           xmax = as.Date("2020-12-12"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.1) +
  
  # Linha vertical e rótulo
  geom_vline(xintercept = as.Date("2020-03-11"), 
             color = "red", linetype = "dashed", linewidth = 0.5) +
  annotate("text", 
           x = as.Date("2020-04-01"), 
           y = max(preço_portifolio$bradesco) * 0.9,
           label = "Pandemia COVID-19",
           color = "red", angle = 0, size = 4.5) 






p5 <- p_ambev + geom_line(color = "#377eb8", linewidth = 0.7) + 
  
  annotate("rect", 
           xmin = as.Date("2020-01-01"), 
           xmax = as.Date("2020-12-12"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.1) +
  
  # Linha vertical e rótulo
  geom_vline(xintercept = as.Date("2020-03-11"), 
             color = "red", linetype = "dashed", linewidth = 0.5) +
  annotate("text", 
           x = as.Date("2020-04-01"), 
           y = max(preço_portifolio$ambev) * 0.9,
           label = "Pandemia COVID-19",
           color = "red", angle = 0, size = 4.5) 





p6 <- p_kroton + geom_line(color = "#377eb8", linewidth = 0.7) + 
  
  annotate("rect", 
           xmin = as.Date("2020-01-01"), 
           xmax = as.Date("2020-12-12"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.1) +
  
  # Linha vertical e rótulo
  geom_vline(xintercept = as.Date("2020-03-11"), 
             color = "red", linetype = "dashed", linewidth = 0.5) +
  annotate("text", 
           x = as.Date("2020-04-01"), 
           y = max(preço_portifolio$kroton) * 0.9,
           label = "Pandemia COVID-19",
           color = "red", angle = 0, size = 4.5) 





p7 <- p_brf + geom_line(color = "#377eb8", linewidth = 0.7) + 
  
  annotate("rect", 
           xmin = as.Date("2020-01-01"), 
           xmax = as.Date("2020-12-12"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.1) +
  
  # Linha vertical e rótulo
  geom_vline(xintercept = as.Date("2020-03-11"), 
             color = "red", linetype = "dashed", linewidth = 0.5) +
  annotate("text", 
           x = as.Date("2020-04-01"), 
           y = max(preço_portifolio$brf) * 0.9,
           label = "Pandemia COVID-19",
           color = "red", angle = 0, size = 4.5) 





p8 <- p_bova11 + geom_line(color = "#377eb8", linewidth = 0.7) + 
  
  annotate("rect", 
           xmin = as.Date("2020-01-01"), 
           xmax = as.Date("2020-12-12"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.1) +
  
  # Linha vertical e rótulo
  geom_vline(xintercept = as.Date("2020-03-11"), 
             color = "red", linetype = "dashed", linewidth = 0.5) +
  annotate("text", 
           x = as.Date("2020-04-01"), 
           y = max(preço_portifolio$bova11) * 0.9,
           label = "Pandemia COVID-19",
           color = "red", angle = 0, size = 4.5) 






p9 <- p_cambio + geom_line(color = "#377eb8", linewidth = 0.7) + 
  
  annotate("rect", 
           xmin = as.Date("2020-01-01"), 
           xmax = as.Date("2020-12-12"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.1) +
  
  # Linha vertical e rótulo
  geom_vline(xintercept = as.Date("2020-03-11"), 
             color = "red", linetype = "dashed", linewidth = 0.5) +
  annotate("text", 
           x = as.Date("2020-04-01"), 
           y = max(preço_portifolio$cambio) * 0.9,
           label = "Pandemia COVID-19",
           color = "red", angle = 0, size = 4.5) 

(p1 + p2 + p3 ) / 
  ( p4 + p5 + p6) + 
  (p7 + p8 + p9) + # LEMBRAR DE NA HORA Q FOR PASSAR PARA O DOC, COLOCAR OUTROS GRAFS
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')


################################################################################################################


############################################# tabela descritiva ################################################


library(PerformanceAnalytics)
library(moments)
library(dplyr)

# 1. Converter retornos para porcentagem
retornos_pct <- preço_portifolio * 100  # Multiplica por 100 para ficar em %

# 2. Calcular estatísticas para cada ativo
estatisticas <- data.frame(
  Ativo = colnames(retornos_pct),
  Média = apply(retornos_pct, 2, mean, na.rm = TRUE),
  Mediana = apply(retornos_pct, 2, median, na.rm = TRUE),
  Mínimo = apply(retornos_pct, 2, min, na.rm = TRUE),
  Máximo = apply(retornos_pct, 2, max, na.rm = TRUE),
  Desvio_Padrão = apply(retornos_pct, 2, sd, na.rm = TRUE),
  Curtose = apply(retornos_pct, 2, kurtosis, na.rm = TRUE),
  Assimetria = apply(retornos_pct, 2, skewness, na.rm = TRUE),
  row.names = NULL
)

# 3. Arredondar valores
estatisticas[, -1] <- round(estatisticas[, -1], 2)

# 4. Adicionar nomes completos (IMPORTANTE)
nomes_completos <- c(
  "petro" = "Petrobras",
  "itau" = "Itaú Unibanco",
  "vale" = "Vale",
  "bradesco" = "Bradesco",
  "ambev" = "Ambev",
  "kroton" = "Kroton",
  "brf" = "BRF",
  "bova11" = "Ibovespa ETF",
  "cambio" = "Dólar"
)

estatisticas$Ativo <- nomes_completos[estatisticas$Ativo]

# 5. Resultado final (Tabela formatada)
print(estatisticas)

# 6. Exportar para CSV (opcional) facilita para fazer as tabs
#write.csv(estatisticas, "estatisticas_retornos.csv", row.names = FALSE)

#####################################################################################################################################


##################################### correlaçao incondicional ##################################################################


library(corrplot)
cor_matrix <- cor(preço_portifolio, use = "complete.obs")

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(100))


####################################################################################################################################

######################################## grafico das volatilidaes condicionais #######################

# Extrair as volatilidades condicionais (desvios padrão)
volatilidades_condicionais <- sqrt(t(apply(cov1, 3, diag))) # cov1 é um array 3D

# Converter para objeto xts para fácil plotagem com datas
volatilidades_xts <- xts(volatilidades_condicionais, order.by = index(retornos))
colnames(volatilidades_xts) <- nomes # colocar os nomes dos ativos

# Plotar todas as volatilidades em um único gráfico facetado
# Usaremos ggplot2 para maior flexibilidade e estética

library(ggplot2)
library(tidyr) # Para pivot_longer
library(xts) # Para as.data.frame.xts

# Converter o xts para data.frame para ggplot
df_volatilidades <- fortify.zoo(volatilidades_xts, melt = TRUE)
colnames(df_volatilidades) <- c("Data", "Ativo", "Volatilidade")

p_vol_individual <- ggplot(df_volatilidades, aes(x = Data, y = Volatilidade, color = Ativo)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ Ativo, scales = "free_y", ncol = 3) + # scales = "free_y" para cada ativo ter sua própria escala
  labs(title = "Volatilidade Condicional Diária por Ativo",
       x = "Data",
       y = "Volatilidade",
       color = "Ativo") +
  theme_minimal() +
  theme(legend.position = "none", # A legenda por cor se torna redundante com facet_wrap
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_vol_individual)

# Ou gráficos separados (para controle individual) VAI TER MTO GRAFICO TALVEZ N USAR TODOS
for (ativo_nome in nomes) {
  p <- ggplot(df_volatilidades %>% filter(Ativo == ativo_nome),
              aes(x = Data, y = Volatilidade)) +
    geom_line(color = "blue", linewidth = 0.8) +
    labs(title = paste("Volatilidade Condicional", ativo_nome),
         x = "Data",
         y = "Volatilidade") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}


####################################################################################################################################################


################################ correlações condicionais#######################################################################################
# vou fazer combinações 2a2 se n tem muito grafico
                     
cor_vale_brf <- xts(cor1["vale", "brf", ], order.by = index(retornos))
colnames(cor_vale_brf) <- "Vale-BRF"

# Preparar dados para ggplot
df_cor_vale_brf <- fortify.zoo(cor_vale_brf, melt = TRUE)
colnames(df_cor_vale_brf) <- c("Data", "Par_Ativos", "Correlacao")

# Plotar
p_vale_brf <- ggplot(df_cor_vale_brf, aes(x = Data, y = Correlacao)) +
  geom_line(color = "black", linewidth = 0.5) +
  labs(title = "Corr. Cond.: VALE e BRF",
       x = "Data",
       y = "Correlação") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_vale_brf)


# --- Gráfico 2: Correlação Condicional ITUB4.SA e BBDC3.SA ---
# Extrair a série de correlação entre Itaú e Bradesco
cor_itau_bradesco <- xts(cor1["itau", "bradesco", ], order.by = index(retornos))
colnames(cor_itau_bradesco) <- "Itaú-Bradesco"

# Preparar dados para ggplot
df_cor_itau_bradesco <- fortify.zoo(cor_itau_bradesco, melt = TRUE)
colnames(df_cor_itau_bradesco) <- c("Data", "Par_Ativos", "Correlacao")

# Plotar
p_itau_bradesco <- ggplot(df_cor_itau_bradesco, aes(x = Data, y = Correlacao)) +
  geom_line(color = "black", linewidth = 0.5) +
  labs(title = "Corr. Cond.: ITAU e BRADESCO",
       x = "Data",
       y = "Correlação") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_itau_bradesco)


# --- Gráfico 3: Correlação Condicional BOVA11.SA e BRL=X (Câmbio) ---
# Extrair a série de correlação entre BOVA11 e Câmbio
cor_bova_cambio <- xts(cor1["bova11", "cambio", ], order.by = index(retornos))
colnames(cor_bova_cambio) <- "BOVA11-Câmbio"

# Preparar dados para ggplot
df_cor_bova_cambio <- fortify.zoo(cor_bova_cambio, melt = TRUE)
colnames(df_cor_bova_cambio) <- c("Data", "Par_Ativos", "Correlacao")

# Plotar
p_bova_cambio <- ggplot(df_cor_bova_cambio, aes(x = Data, y = Correlacao)) +
  geom_line(color = "black", linewidth = 0.5) +
  labs(title = "Corr. Cond.: BOVA11 e CAMBIO",
       x = "Data",
       y = "Correlação") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_bova_cambio)


############################################################################################################################################################


############################################################ volta cond 2 a 2 ##########################################################################################
# outra vez combinacao 2a2
# Gráfico 1: Volatilidade Condicional de VALE3.SA e BRFS3.SA ---
# Selecionar as séries de volatilidade para Vale e BRF
vol_vale_brf <- volatilidades_xts[, c("Vale", "BRF")]

# Preparar dados para ggplot
# fortify.zoo com melt = TRUE é perfeito para isso
df_vol_vale_brf <- fortify.zoo(vol_vale_brf, melt = TRUE)
colnames(df_vol_vale_brf) <- c("Data", "Ativo", "Volatilidade")

# Plotar
p_vol_vale_brf <- ggplot(df_vol_vale_brf, aes(x = Data, y = Volatilidade, color = Ativo)) +
  geom_line(linewidth = 0.5) +
  labs(title = "Volat. Cond.: VALE e BRF",
       x = "Data",
       y = "Volatilidade",
       color = "Ativo") + # MANTER a legenda para diferenciar as linhas
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_vol_vale_brf)


# --- Gráfico 2: Volatilidade Condicional de ITUB4.SA e BBDC3.SA ---
# Selecionar as séries de volatilidade para Itaú e Bradesco
vol_itau_bradesco <- volatilidades_xts[, c("Itau", "Bradesco")]

# Preparar dados para ggplot
df_vol_itau_bradesco <- fortify.zoo(vol_itau_bradesco, melt = TRUE)
colnames(df_vol_itau_bradesco) <- c("Data", "Ativo", "Volatilidade")

# Plotar
p_vol_itau_bradesco <- ggplot(df_vol_itau_bradesco, aes(x = Data, y = Volatilidade, color = Ativo)) +
  geom_line(linewidth = 0.5) +
  labs(title = "Volat. Cond.: ITAU e BRADESCO",
       x = "Data",
       y = "Volatilidade",
       color = "Ativo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_vol_itau_bradesco)


# --- Gráfico 3: Volatilidade Condicional de BOVA11.SA e BRL=X (Câmbio) ---
# Selecionar as séries de volatilidade para BOVA11 e Câmbio
vol_bova_cambio <- volatilidades_xts[, c("Bova11", "Cambio")]

# Preparar dados para ggplot
df_vol_bova_cambio <- fortify.zoo(vol_bova_cambio, melt = TRUE)
colnames(df_vol_bova_cambio) <- c("Data", "Ativo", "Volatilidade")

# Plotar
p_vol_bova_cambio <- ggplot(df_vol_bova_cambio, aes(x = Data, y = Volatilidade, color = Ativo)) +
  geom_line(linewidth = 0.5) +
  labs(title = "Volat. Cond.: BOVA11 e CAMBIO",
       x = "Data",
       y = "Volatilidade",
       color = "Ativo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_vol_bova_cambio)


(p_vale_brf + p_itau_bradesco + p_bova_cambio ) / 
  ( p_vol_vale_brf + p_vol_itau_bradesco + p_vol_bova_cambio) +  # lembrar de colocar outros graf na hora de passar pro doc final
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')


#########################################################################################################################################################################


###################################################### diagnostico dos residuos ####################################################################################################


library(rmgarch)
library(xts)
library(ggplot2)
library(forecast) # Para a função Acf e Pacf 
library(tidyr) # Para pivot_longer
library(gridExtra) # Para organizar múltiplos plots em uma única figura
library(dplyr) # Para manipulação de dados

# PASSO 1: Extrair Resíduos Padronizados do Modelo DCC-GARCH

# Os resíduos padronizados são geralmente os resíduos divididos pelo desvio padrão condicional.
# Em rmgarch, a função residuals() com standardize=TRUE já faz isso.
residuos_padronizados_multi <- residuals(dcc_fit)

# converter para xts e adicionar os nomes das colunas
residuos_xts <- xts(residuos_padronizados_multi, order.by = index(retornos))
colnames(residuos_xts) <- nomes # LEMBRAR DE USAR NOMES CERTOS SE NAO DA ERRADO

# Criar os resíduos padronizados ao quadrado
residuos_quadrados_xts <- residuos_xts^2

# PASSO 2: Estatísticas Descritivas dos Resíduos Padronizados

# Calcular média e desvio padrão para cada série de resíduos padronizados
stats_residuos <- data.frame(
  Ativo = colnames(residuos_xts),
  Media = apply(residuos_xts, 2, mean),
  Desvio_Padrao = apply(residuos_xts, 2, sd),
  Assimetria = apply(residuos_xts, 2, PerformanceAnalytics::skewness),
  Curtose = apply(residuos_xts, 2, PerformanceAnalytics::kurtosis)
)

# Arredondar para melhor visualização
stats_residuos_formatado <- stats_residuos %>%
  mutate(across(where(is.numeric), ~round(., 4)))

print("Tabela A.1: Estatísticas Descritivas dos Resíduos Padronizados")
print(stats_residuos_formatado)

# PASSO 3: Análise Gráfica dos Resíduos (Séries Temporais e ACF/PACF)

# Selecione alguns ativos apenas ou todos n sei ainda vai ficar muita coisa
ativos_para_plot <- c("Petrobras", "Itau", "Vale", "Bradesco","Ambev","Kroton","BRF","Bova11","Cambio") # Ajuste conforme seu interesse

# Lista para armazenar os plots
plots_residuos <- list()
plots_residuos_quadrados <- list()
plots_acf_residuos <- list()
plots_pacf_residuos <- list()
plots_acf_residuos_quadrados <- list()
plots_pacf_residuos_quadrados <- list()


for (ativo in ativos_para_plot) {
  #  Resíduos Padronizados 
  df_res <- fortify.zoo(residuos_xts[, ativo], melt = TRUE)
  colnames(df_res) <- c("Data", "Ativo", "Residuo")
  
  p_res_ts <- ggplot(df_res, aes(x = Data, y = Residuo)) +
    geom_line(linewidth = 0.5, color = "darkblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Resíduos Padronizados de", ativo), x = "Data", y = "Resíduo") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  plots_residuos[[ativo]] <- p_res_ts
  
  # ACF dos resíduos
  
  acf_res <- stats::acf(residuos_xts[, ativo], plot = FALSE, lag.max = 20)
  df_acf_res <- data.frame(Lag = acf_res$lag, ACF = acf_res$acf)
  p_acf_res <- ggplot(df_acf_res, aes(x = Lag, y = ACF)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    geom_hline(yintercept = c(0.0, 1.96/sqrt(length(residuos_xts[, ativo]))), linetype = "dashed", color = "blue") +
    geom_hline(yintercept = c(0.0, -1.96/sqrt(length(residuos_xts[, ativo]))), linetype = "dashed", color = "blue") +
    labs(title = paste("ACF dos Resíduos Padronizados de", ativo), x = "Lag", y = "ACF") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  plots_acf_residuos[[ativo]] <- p_acf_res
  
  # PACF dos resíduos
  pacf_res <- stats::pacf(residuos_xts[, ativo], plot = FALSE, lag.max = 20)
  df_pacf_res <- data.frame(Lag = pacf_res$lag, PACF = pacf_res$acf)
  p_pacf_res <- ggplot(df_pacf_res, aes(x = Lag, y = PACF)) +
    geom_bar(stat = "identity", fill = "darkorange") +
    geom_hline(yintercept = c(0.0, 1.96/sqrt(length(residuos_xts[, ativo]))), linetype = "dashed", color = "blue") +
    geom_hline(yintercept = c(0.0, -1.96/sqrt(length(residuos_xts[, ativo]))), linetype = "dashed", color = "blue") +
    labs(title = paste("PACF dos Resíduos Padronizados de", ativo), x = "Lag", y = "PACF") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  plots_pacf_residuos[[ativo]] <- p_pacf_res
  
  
  #  Resíduos Padronizados ao Quadrad
  df_res_sq <- fortify.zoo(residuos_quadrados_xts[, ativo], melt = TRUE)
  colnames(df_res_sq) <- c("Data", "Ativo", "Residuo_Quadrado")
  
  p_res_sq_ts <- ggplot(df_res_sq, aes(x = Data, y = Residuo_Quadrado)) +
    geom_line(linewidth = 0.5, color = "purple") +
    geom_hline(yintercept = mean(residuos_quadrados_xts[, ativo]), linetype = "dashed", color = "red") +
    labs(title = paste("Resíduos Padronizados ao Quadrado de", ativo), x = "Data", y = "Resíduo²") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  plots_residuos_quadrados[[ativo]] <- p_res_sq_ts
  
  # ACF dos resíduos ao quadrado
  acf_res_sq <- stats::acf(residuos_quadrados_xts[, ativo], plot = FALSE, lag.max = 20)
  df_acf_res_sq <- data.frame(Lag = acf_res_sq$lag, ACF = acf_res_sq$acf)
  p_acf_res_sq <- ggplot(df_acf_res_sq, aes(x = Lag, y = ACF)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    geom_hline(yintercept = c(0.0, 1.96/sqrt(length(residuos_quadrados_xts[, ativo]))), linetype = "dashed", color = "blue") +
    geom_hline(yintercept = c(0.0, -1.96/sqrt(length(residuos_quadrados_xts[, ativo]))), linetype = "dashed", color = "blue") +
    labs(title = paste("ACF dos Resíduos Padronizados ao Quadrado de", ativo), x = "Lag", y = "ACF") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  plots_acf_residuos_quadrados[[ativo]] <- p_acf_res_sq
  
  # PACF dos resíduos ao quadrado
  pacf_res_sq <- stats::pacf(residuos_quadrados_xts[, ativo], plot = FALSE, lag.max = 20)
  df_pacf_res_sq <- data.frame(Lag = pacf_res_sq$lag, PACF = pacf_res_sq$acf)
  p_pacf_res_sq <- ggplot(df_pacf_res_sq, aes(x = Lag, y = PACF)) +
    geom_bar(stat = "identity", fill = "darkorange") +
    geom_hline(yintercept = c(0.0, 1.96/sqrt(length(residuos_quadrados_xts[, ativo]))), linetype = "dashed", color = "blue") +
    geom_hline(yintercept = c(0.0, -1.96/sqrt(length(residuos_quadrados_xts[, ativo]))), linetype = "dashed", color = "blue") +
    labs(title = paste("PACF dos Resíduos Padronizados ao Quadrado de", ativo), x = "Lag", y = "PACF") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  plots_pacf_residuos_quadrados[[ativo]] <- p_pacf_res_sq
}

# Organizar e imprimir alguns plots (talvez todos fica muito)
# Agrupar 2x2 para cada tipo de gráfico para os ativos selecionados
# Gráficos de série temporal dos resíduos padronizados
print(grid.arrange(plots_residuos[[1]], plots_residuos[[2]], plots_residuos[[3]], plots_residuos[[4]], ncol = 2,
                   top = "Figura A.1: Séries Temporais dos Resíduos Padronizados de Ativos Selecionados"))

# Gráficos de ACF dos resíduos padronizados
print(grid.arrange(plots_acf_residuos[[1]], plots_acf_residuos[[2]], plots_acf_residuos[[3]], plots_acf_residuos[[4]], ncol = 2,
                   top = "Figura A.2: Funções de Autocorrelação (ACF) dos Resíduos Padronizados de Ativos Selecionados"))

# Gráficos de ACF dos resíduos padronizados ao quadrado
print(grid.arrange(plots_acf_residuos_quadrados[[1]], plots_acf_residuos_quadrados[[2]], plots_acf_residuos_quadrados[[3]], plots_acf_residuos_quadrados[[4]], ncol = 2,
                   top = "Figura A.3: Funções de Autocorrelação (ACF) dos Resíduos Padronizados ao Quadrado de Ativos Selecionados"))


# PASSO 4: Testes Formais de Autocorrelação (Teste de Ljung-Box)


# Definir os lags para o teste de Ljung-Box (nesse caso 10 15 20)
lags_test <- c(10, 15, 20)

# dataframes para armazenar os resultados dos testes
ljung_box_residuos <- data.frame(Ativo = character(), Lag = numeric(), Chi_Sq = numeric(), P_Valor = numeric(), stringsAsFactors = FALSE)
ljung_box_residuos_quadrados <- data.frame(Ativo = character(), Lag = numeric(), Chi_Sq = numeric(), P_Valor = numeric(), stringsAsFactors = FALSE)

for (ativo in colnames(residuos_xts)) {
  for (lag in lags_test) {
    # Teste para resíduos padronizados
    test_res <- Box.test(residuos_xts[, ativo], lag = lag, type = "Ljung-Box")
    ljung_box_residuos <- rbind(ljung_box_residuos, data.frame(
      Ativo = ativo, Lag = lag, Chi_Sq = round(test_res$statistic, 2), P_Valor = round(test_res$p.value, 4)
    ))
    
    # Teste para resíduos padronizados ao quadrado
    test_res_sq <- Box.test(residuos_quadrados_xts[, ativo], lag = lag, type = "Ljung-Box")
    ljung_box_residuos_quadrados <- rbind(ljung_box_residuos_quadrados, data.frame(
      Ativo = ativo, Lag = lag, Chi_Sq = round(test_res_sq$statistic, 2), P_Valor = round(test_res_sq$p.value, 4)
    ))
  }
}

print("Tabela A.2: Resultados do Teste de Ljung-Box para Resíduos Padronizados")
print(ljung_box_residuos)

print("Tabela A.3: Resultados do Teste de Ljung-Box para Resíduos Padronizados ao Quadrado")
print(ljung_box_residuos_quadrados)


# fazer uma explicacao do codigo usado dps se precisar sei la tem muita coisa ja ou pedir pro chatgpt explicar com ptbr bom (LEMBRAR!!)
                     
getSymbols("BBAS3.SA", src=fonte, from=inicial, to=final)
xBB <- BBAS3.SA$BBAS3.SA.Adjusted

bb_retorno <- dailyReturn(xBB, type = 'log')
bb_retorno <- na.omit(bb_retorno)


par(mfrow = c(3, 1))
plot(bb_retorno, main = "Retorno Banco do brasil")
plot(retornos$vale, main = 'Retorno Vale')
plot(retornos$petro, main = 'Retorno Petrobras')
par(mfrow = c(1, 1))


par(mfrow = c(3,3))

# Gerar quantis teóricos da distribuição t-Student
# O número de pontos deve ser igual ao número de resíduos
n_residuos <- length(residuos[,1])
probabilidades <- (1:n_residuos - 0.5) / n_residuos # Pontos de probabilidade para os quantis
quantis_teoricos_t <- qt(probabilidades, df = dcc_fit@mfit$coef[5]) # Usa o df estimado para PETR4
# Ordenar os resíduos para plotar
residuos_ordenados <- sort(residuos[,1])
# Plotar o QQ-Plot
plot(quantis_teoricos_t, residuos_ordenados,
     main = paste0("QQ-Plot: Resíduos Petrobras vs. t-Student (df=", round(dcc_fit@mfit$coef[5], 2), ")"),
     xlab = paste0("Quantis Teóricos (t-Student, df=", round(dcc_fit@mfit$coef[5], 2), ")"),
     ylab = "Quantis Amostrais",
     col = "blue", pch = 16)

# Adicionar a linha de referência (y = x)
abline(0, 1, col = "red", lwd = 2)


# Gerar quantis teóricos da distribuição t-Student
# O número de pontos deve ser igual ao número de resíduos
n_residuos <- length(residuos[,2])
probabilidades <- (1:n_residuos - 0.5) / n_residuos # Pontos de probabilidade para os quantis
quantis_teoricos_t <- qt(probabilidades, df = dcc_fit@mfit$coef[10]) 
# Ordenar os resíduos para plotar
residuos_ordenados <- sort(residuos[,2])
# Plotar o QQ-Plot
plot(quantis_teoricos_t, residuos_ordenados,
     main = paste0("QQ-Plot: Resíduos Itau vs. t-Student (df=", round(dcc_fit@mfit$coef[10], 2), ")"),
     xlab = paste0("Quantis Teóricos (t-Student, df=", round(dcc_fit@mfit$coef[10], 2), ")"),
     ylab = "Quantis Amostrais",
     col = "blue", pch = 16)

# Adicionar a linha de referência (y = x)
abline(0, 1, col = "red", lwd = 2)



# Gerar quantis teóricos da distribuição t-Student
# O número de pontos deve ser igual ao número de resíduos
n_residuos <- length(residuos[,3])
probabilidades <- (1:n_residuos - 0.5) / n_residuos # Pontos de probabilidade para os quantis
quantis_teoricos_t <- qt(probabilidades, df = dcc_fit@mfit$coef[15]) 
# Ordenar os resíduos para plotar
residuos_ordenados <- sort(residuos[,3])
# Plotar o QQ-Plot
plot(quantis_teoricos_t, residuos_ordenados,
     main = paste0("QQ-Plot: Resíduos Vale vs. t-Student (df=", round(dcc_fit@mfit$coef[15], 2), ")"),
     xlab = paste0("Quantis Teóricos (t-Student, df=", round(dcc_fit@mfit$coef[15], 2), ")"),
     ylab = "Quantis Amostrais",
     col = "blue", pch = 16)

# Adicionar a linha de referência (y = x)
abline(0, 1, col = "red", lwd = 2)



# Gerar quantis teóricos da distribuição t-Student
# O número de pontos deve ser igual ao número de resíduos
n_residuos <- length(residuos[,4])
probabilidades <- (1:n_residuos - 0.5) / n_residuos # Pontos de probabilidade para os quantis
quantis_teoricos_t <- qt(probabilidades, df = dcc_fit@mfit$coef[20]) 
# Ordenar os resíduos para plotar
residuos_ordenados <- sort(residuos[,4])
# Plotar o QQ-Plot
plot(quantis_teoricos_t, residuos_ordenados,
     main = paste0("QQ-Plot: Resíduos Bradesco vs. t-Student (df=", round(dcc_fit@mfit$coef[20], 2), ")"),
     xlab = paste0("Quantis Teóricos (t-Student, df=", round(dcc_fit@mfit$coef[20], 2), ")"),
     ylab = "Quantis Amostrais",
     col = "blue", pch = 16)

# Adicionar a linha de referência (y = x)
abline(0, 1, col = "red", lwd = 2)



# Gerar quantis teóricos da distribuição t-Student
# O número de pontos deve ser igual ao número de resíduos
n_residuos <- length(residuos[,5])
probabilidades <- (1:n_residuos - 0.5) / n_residuos # Pontos de probabilidade para os quantis
quantis_teoricos_t <- qt(probabilidades, df = dcc_fit@mfit$coef[25]) 
# Ordenar os resíduos para plotar
residuos_ordenados <- sort(residuos[,5])
# Plotar o QQ-Plot
plot(quantis_teoricos_t, residuos_ordenados,
     main = paste0("QQ-Plot: Resíduos Ambev vs. t-Student (df=", round(dcc_fit@mfit$coef[25], 2), ")"),
     xlab = paste0("Quantis Teóricos (t-Student, df=", round(dcc_fit@mfit$coef[25], 2), ")"),
     ylab = "Quantis Amostrais",
     col = "blue", pch = 16)

# Adicionar a linha de referência (y = x)
abline(0, 1, col = "red", lwd = 2)



# Gerar quantis teóricos da distribuição t-Student
# O número de pontos deve ser igual ao número de resíduos
n_residuos <- length(residuos[,6])
probabilidades <- (1:n_residuos - 0.5) / n_residuos # Pontos de probabilidade para os quantis
quantis_teoricos_t <- qt(probabilidades, df = dcc_fit@mfit$coef[30]) 
# Ordenar os resíduos para plotar
residuos_ordenados <- sort(residuos[,6])
# Plotar o QQ-Plot
plot(quantis_teoricos_t, residuos_ordenados,
     main = paste0("QQ-Plot: Resíduos Kroton vs. t-Student (df=", round(dcc_fit@mfit$coef[30], 2), ")"),
     xlab = paste0("Quantis Teóricos (t-Student, df=", round(dcc_fit@mfit$coef[30], 2), ")"),
     ylab = "Quantis Amostrais",
     col = "blue", pch = 16)

# Adicionar a linha de referência (y = x)
abline(0, 1, col = "red", lwd = 2)



# Gerar quantis teóricos da distribuição t-Student
# O número de pontos deve ser igual ao número de resíduos
n_residuos <- length(residuos[,7])
probabilidades <- (1:n_residuos - 0.5) / n_residuos # Pontos de probabilidade para os quantis
quantis_teoricos_t <- qt(probabilidades, df = dcc_fit@mfit$coef[35]) 
# Ordenar os resíduos para plotar
residuos_ordenados <- sort(residuos[,7])
# Plotar o QQ-Plot
plot(quantis_teoricos_t, residuos_ordenados,
     main = paste0("QQ-Plot: Resíduos BRF vs. t-Student (df=", round(dcc_fit@mfit$coef[35], 2), ")"),
     xlab = paste0("Quantis Teóricos (t-Student, df=", round(dcc_fit@mfit$coef[35], 2), ")"),
     ylab = "Quantis Amostrais",
     col = "blue", pch = 16)

# Adicionar a linha de referência (y = x)
abline(0, 1, col = "red", lwd = 2)



# Gerar quantis teóricos da distribuição t-Student
# O número de pontos deve ser igual ao número de resíduos
n_residuos <- length(residuos[,8])
probabilidades <- (1:n_residuos - 0.5) / n_residuos # Pontos de probabilidade para os quantis
quantis_teoricos_t <- qt(probabilidades, df = dcc_fit@mfit$coef[40]) 
# Ordenar os resíduos para plotar
residuos_ordenados <- sort(residuos[,8])
# Plotar o QQ-Plot
plot(quantis_teoricos_t, residuos_ordenados,
     main = paste0("QQ-Plot: Resíduos BOVA11 vs. t-Student (df=", round(dcc_fit@mfit$coef[40], 2), ")"),
     xlab = paste0("Quantis Teóricos (t-Student, df=", round(dcc_fit@mfit$coef[40], 2), ")"),
     ylab = "Quantis Amostrais",
     col = "blue", pch = 16)

# Adicionar a linha de referência (y = x)
abline(0, 1, col = "red", lwd = 2)



# Gerar quantis teóricos da distribuição t-Student
# O número de pontos deve ser igual ao número de resíduos
n_residuos <- length(residuos[,9])
probabilidades <- (1:n_residuos - 0.5) / n_residuos # Pontos de probabilidade para os quantis
quantis_teoricos_t <- qt(probabilidades, df = dcc_fit@mfit$coef[45]) 
# Ordenar os resíduos para plotar
residuos_ordenados <- sort(residuos[,9])
# Plotar o QQ-Plot
plot(quantis_teoricos_t, residuos_ordenados,
     main = paste0("QQ-Plot: Resíduos BOVA11 vs. t-Student (df=", round(dcc_fit@mfit$coef[45], 2), ")"),
     xlab = paste0("Quantis Teóricos (t-Student, df=", round(dcc_fit@mfit$coef[45], 2), ")"),
     ylab = "Quantis Amostrais",
     col = "blue", pch = 16)

# Adicionar a linha de referência (y = x)
abline(0, 1, col = "red", lwd = 2)

par(mfrow = c(1,1))
