# IC-Economia
Código IC Economia
rm(list = ls())

#Instalando os pacotes necessários

install.packages("AER") #pacote para testes do modelo de regressao
install.packages("psych") #pacote para estatisticas descritivas
install.packages("dplyr") #pacote para organizacao e tratamento dos dados
install.packages("tidyr") #pacote para organizacao e tratamento dos dados
install.packages("openxlsx") #leitura dos dados em excel
install.packages("readxl") #leitura dos dados em Excel
install.packages("stargazer") #tabelas das regressoes
install.packages("collapse") #necessario para plm
install.packages("plm") #regressao de dados em painel
install.packages("xtable") #criar tabelas
install.packages("gplots")    # Various programing tools for plotting data
install.packages("tseries")   # For timeseries analysis
install.packages("lmtest")    # For hetoroskedasticity analysis
install.packages("cowplot") # pacote juntar graficos

library(AER)
library(psych) 
library(dplyr)
library(tidyr) 
library(openxlsx) 
library(readxl) 
library(stargazer) 
library(collapse)
library(xtable)
library(plm) 
library(gplots)    
library(tseries)   
library(lmtest) 
library(cowplot)

# Analise Econometrica

#1. Atualizando o diretório

setwd("~/Desktop/IC_Economia")

#2. baixando a base de dados

base <- read_excel("base_de_dados.xlsx", 
                   sheet = "dados_organizados")

View(base)

#3. Declarando o painel:

base <- pdata.frame(base, index = c("country", "year"))

# 4. Observando os tipos de variaveis:
str(base)

# 5. Estatisticas descritivas das variaveis quantitativas nao binarias e políticas:

descritiva_total <- psych::describe(base[, c(3, 17:20)], 
                                    quant = c(.25, .75))

descritivas_aut <- psych::describe(base[base$democr == 0, c(3, 17:20)], 
                               quant = c(.25, .75))
descritivas_aut <- round(descritivas_aut, 3)
descritivas_aut

descritivas_dem <- psych::describe(base[base$democr == 1, c(3, 10, 17:20)], 
                               quant = c(.25, .75))
descritivas_dem <- round(descritivas_dem, 3)
descritivas_dem


descritivas <- list("Descritivas Democracias" = descritivas_dem, "Descritivas Ditaduras" = descritivas_aut)
write.xlsx(descritivas, "descritivas.xlsx", rowNames = T)


# 6.Análise de correlação (quais variáveis políticas que estão mais relacionados com as variáveis dependentes)
base_corr <- base[, c(3, 7, 8 ,9 , 10, 17, 18, 19, 20)]

base_corr <- na.omit(base_corr)

matriz_corr <- cor(base_corr, method = "pearson") #destaque para perc_party, marg_party, hef_index

# 7 gráfico País e Tempo

coplot(spend ~ year|country, type="b", data=base) 

plotmeans(spend ~ country, data = base, ylab = "Média dos Gastos do Governo em % PIB", xlab = "Países")


plotmeans(spend ~ year, ylab = "Média dos Gastos do Governo em % PIB", xlab = "Anos", data = base)

hist(base$spend)

s1 <- ggplot(base, aes(x = hef_index, y = spend, color = country, group=country)) +
  geom_point()+
  labs(x = "Herfindahl Index", y = "Gastos do Governo em % PIB")

s2 <- ggplot(base, aes(x = perc_gov, y = spend, color = country, group=country)) +
  geom_point()+
  labs(x = "% Cadeiras do  governo", y = "Gastos do Governo em % PIB")

s3 <- ggplot(base, aes(x = perc_op, y = spend, color = country, group=country)) +
  geom_point()+
  labs(x = "% Cadeiras da oposição", y = "Gastos do Governo em % PIB")

plot_grid(s1, s2, s3,
          ncol = 3,
          labels = c("Gráfico 1", "Gráfico 2", "Gráfico 3"),
          rel_widths = c(5, 5, 5))

#8 Modelo Pooled - perc_gov

names(base)
pooled <- plm(spend ~ ideol + cycle + polariz + democr + openess +
                pop15 + pop65 + rural_pop + grow + lag_spend + perc_gov, 
              data = base, model = "pooling")

summary(pooled)


# 9 Modelo de efeitos fixos - perc_gov

fixos <-  plm(spend ~ ideol + cycle + polariz + democr + openess +
                rural_pop + pop65 + lag_spend + perc_gov , model = "within", effect = "twoways",
              data = base)

fixef(fixos)

summary(fixos)


# 10 - Modelo de efeitos aleatorios - perc_gov

aleatorios <-  plm(spend ~ ideol + cycle + polariz + democr + openess +
                     pop15 + pop65 + rural_pop + grow + lag_spend + perc_gov, 
                   data = base, model = "random", random.method = "walhus")
summary(aleatorios)


# 11 - Teste f - Modelo Pooled x Modelo de Efeitos fixos 

pFtest (fixos, pooled)

#como o valor de p-valor é menor que 0,05, o Modelo de Efeitos Fixos é melhor do que o modelo de Pooled

# 12 - Teste BP - Modelo Pooled x Modelo de Efeitos Aleatorios

plmtest(pooled, type = "bp")

#Como o p valor foi inferior a 0,05 o modelo de Efeitos Aleatórios é superior ao modelo Pooled.


#13 - Teste de Breusch-Pagan  - Modelo de Efeitos Aleatorios x Efeito Fixos 

phtest(aleatorios, fixos)

#Como o valor p foi menor a 0,05 o modelo de Efeitos Fixos foi considerado superior ao modelo de Efeitos Aleatorios


# 14 - Modelo Escolhido : Efeitos Aleatorios - Teste de heterocedasticidade.

bptest (fixos)

#Como a hipótese nula é a de que não há homocedasticidade nos resíduos e o p-value foi superior a 0,05, nao há problemas nos resíduos da regressão

# 15 - Teste de Breusch-Godfrey - Autocorrelação Serial 

pbgtest(fixos) 

#não há problemas de correlação serial nos dados, pois o p-value é superior a 0,05.

# 16 - Teste de normalidade de Jarque-Bera
boxplot(fixos$residuals)
hist(fixos$residuals)

jb.norm.test(fixos$residuals)

#distribuição dos erros não é normal, pois p-valor < 0,05


#17 - Organizar em tabela

descritiva_ditaduras <- stargazer(subset(base[c(3, 10, 17:20)], base$democr== 0),
title="Estatisticas Descritivas Ditaduras", type = "html", digits=2)

write(descritiva_ditaduras, "ditadura.html")


descritiva_democracias <- stargazer(subset(base[c(3, 10, 17:20)], base$democr== 1),
                                  title="Estatisticas Descritivas Democracias", type = "html", digits=2)

write(descritiva_democracias, "democracia.html")

matriz_correlação <- stargazer(matriz_corr, title = "Matriz de Correlacao", single.row = TRUE,
                               type = "html")

write(matriz_correlação, "correlacao.html")

resultado <- stargazer(pooled, fixos, aleatorios, type = "html", align=TRUE, ci = FALSE, 
                       column.labels = c("OLS", "Efeitos Fixos", "Efeitos Aleatorios"), 
                       title = "Resultados dos Modelos",
          dep.var.labels = "Gastos do Governo % PIB", decimal.mark = ".", 
          no.space = TRUE,
          omit.stat = "ser")
write(resultado, "res.html")


#FIM
