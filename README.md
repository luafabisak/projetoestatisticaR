# projetoestatisticaR
Realizado no MBA Ciência de dados &amp; IA - SENAC-PE

# Carregar dados
dados <- read.csv("C://Users//Marialua//Downloads//Grupo 7 - Consumo - Cerveja.csv", sep=";", header = TRUE) 
summary(dados)

is.data.frame(dados)
names(dados)
dim(dados)
str(dados)

# Identifica as linhas que apresenta valores faltantes
complete.cases(dados)

# Variável Temperatura média 
dados_numericos_temperaturamedia <- as.numeric(dados$TemperaturaMedia)
 mean(dados_numericos_temperaturamedia) #21.22º
median(dados_numericos_temperaturamedia) #21.38º
sd(dados_numericos_temperaturamedia) #3.18
 
# Variável temperatura mínima 
dados_numericos_temperaturaminima <- as.numeric(dados$TemperaturaMin)
mean(dados_numericos_temperaturaminima) #16.46º
median(dados_numericos_temperaturaminima) #17.9º
sd(dados_numericos_temperaturaminima) #2.87

# Variável temperatura máxima 
dados_numerico_temperaturamaxima <- as.numeric(dados$TemperaturaMax)
mean(dados_numerico_temperaturamaxima) #26.61º
median(dados_numerico_temperaturamaxima) #26.9º
sd(dados_numerico_temperaturamaxima) #4.31

# Variável preciptação
dados_numericos_preciptação <- as.numeric(dados$Precipitacao)
mean(dados_numericos_preciptação) #5.19
median(dados_numericos_preciptação) #0
sd(dados_numericos_preciptação) #12.41

# Variável Final.de.semana
# Transformar os levels do fator "Final.de.Semana" em "Sim" e "Não"
dados$Final.de.Semana <- factor(dados$Final.de.Semana)
print(levels(dados$Final.de.Semana))
# Substituir os valores nos níveis
levels(dados$Final.de.Semana) <- c("Sim", "Não")

# Calcular a tabela de frequência
tabela_frequencia <- table(dados$Final.de.Semana)

# Calcular a porcentagem de ocorrência de "Sim" e "Não"
porcentagem_sim <- prop.table(tabela_frequencia)["Sim"] * 100
porcentagem_nao <- prop.table(tabela_frequencia)["Não"] * 100
print(porcentagem_sim)
print(porcentagem_nao)

# Variável consumo Cerveja
mean(dados$ConsumoCerveja)
median(dados$ConsumoCerveja)
sd(dados$ConsumoCerveja)

# Variável data:
dados$Data <- as.Date(dados$Data, format = "%d/%m/%Y")

dados$Data <- format(dados$Data, "%d-%m-%Y")  
print(dados$Data)

# Correlações
install.packages('dplyr')
library(dplyr)  #para realizar manipulações de dados

# temperatura média com temperatura minima
cor_tempmedia_tempminima <- cor(dados$TemperaturaMedia, dados$TemperaturaMin) #0.8
cor_tempmaxima_tempminima <- cor(dados$TemperaturaMax, dados$TemperaturaMin) #0,6
cor_tempmaxima_precip <- cor(dados$TemperaturaMax, dados$Precipitacao) #-0.04
cor_temprecip_tempminima <- cor(dados_numericos_preciptação, dados_numericos_temperaturaminima)

consumo_fds <- dados$ConsumoCerveja[dados$Final.de.Semana == "Sim"]
consumo_fds_neg <- dados$ConsumoCerveja[dados$Final.de.Semana == "Não"]
teste_t <- t.test(consumo_fds, consumo_fds_neg) #p-value < 2.2e-16

# matriz de correlação entre temperaturas
install.packages("corrplot")
library(corrplot)
matriz_correlacao <- cor(temperaturas)
corrplot(matriz_correlacao, method = "circle")

# matriz_cor_prec <- cor(dados_numericos_preciptação, temperaturas)
corrplot(matriz_cor_prec, method = "circle")

install.packages("GGally")
library(GGally)
ggpairs(temperaturas)

# Análise curtose
install.packages("e1071")
library(e1071)
temperatura_minima <- kurtosis(dados_numericos_temperaturaminima) #-0.5
curtose_temperatura_maxima <- kurtosis(dados_numerico_temperaturamaxima) #-0.3
curtose_preciptação <- kurtosis(dados_numericos_preciptação) #17.4
curtose_consumo <- kurtosis(dados$ConsumoCerveja) #-0.4
curtose_temperatura_media <- kurtosis(dados_numericos_temperaturamedia)

# Análise assimetria
assimetria_temperaturaminima <- skewness(dados_numericos_temperaturaminima) #-0.2
assimetria_temperaturamaxima <- skewness(dados_numerico_temperaturamaxima) #-0.15
assimetria_preciptação <- skewness(dados_numericos_preciptação) #3.74
assimetria_consumo <- skewness(dados$ConsumoCerveja) #0.26
assimetria_temperaturamedia <- skewness(dados_numericos_temperaturamedia)

# Gráficos

install.packages("ggplot2")
library(ggplot2)


temperaturas <- data.frame(
  TemperaturaMedia = c(dados_numericos_temperaturamedia),
  TemperaturaMinima = c(dados_numericos_temperaturaminima),
  TemperaturaMaxima = c(dados_numerico_temperaturamaxima)
)
ggplot(mapping = aes(x = TemperaturaMedia)) + 
  geom_histogram(data = temperaturas, binwidth = 1, fill = "blue", alpha = 0.5) +
  labs(title = "Histograma de Temperatura", x = "Temperatura", y = "Frequência")

hist(temperaturas$TemperaturaMinima, col = "green", main = "Temperatura Mínima", xlab = "Temperatura")
hist(temperaturas$TemperaturaMedia, col = "blue", main = "Temperatura Média", xlab = "Temperatura")
hist(temperaturas$TemperaturaMaxima, col = "red", main = "Temperatura Máxima", xlab = "Temperatura")

#Gráfico de dispersão
ggplot(temperaturas, aes(dados_numericos_temperaturaminima, dados_numericos_temperaturamedia, dados_numerico_temperaturamaxima)) +
  geom_point() +
  geom_smooth(method = "lm")

# Gráfico de barras
categoria <- c("Temperatura mínima", "Temperatura média", "Temperatura máxima")
contagens <- c(dados_numericos_temperaturaminima, dados_numericos_temperaturamedia, dados_numerico_temperaturamaxima)
dados_barras <- data.frame(categoria, contagens)
barplot(dados_barras$contagens,
        names.arg = dados_barras$categoria,
        xlab = "Categoria",
        ylab = "Contagens",
        main = "Gráfico de Barras")

# Calcular as frequências absolutas para a variável Final.de.Semana
tabela_frequencia <- table(dados$Final.de.Semana)

# Calcular as porcentagens
porcentagens <- prop.table(tabela_frequencia) * 100

# Criar o gráfico de barras
barplot(porcentagens,
        xlab = "Final de Semana",
        ylab = "Porcentagem",
        main = "Gráfico de Barras - Final de Semana")



# Aplicando inferências

# Testes de hipóteses#
#se a temperatura média, mínima ou máxima tem um efeito significativo no consumo de cerveja.

shapiro.test(dados$ConsumoCerveja) # p-value = 0.005779 H1. Ou seja, há evidências de que os dados de consumo de cerveja não seguem uma distribuição normal.
shapiro.test(dados_numerico_temperaturamaxima) #p-value = 0.08346 H1
shapiro.test(dados_numericos_temperaturaminima) # p-value = 0.0005333 H1
shapiro.test(dados_numericos_temperaturamedia) # p-value = 0.0005333H1

wilcox.test(dados$ConsumoCerveja, dados$TemperaturaMin) #p-value < 2.2e-16
#concluímos que há uma diferença significativa entre o consumo de cerveja e a temperatura mínima.



#se há uma diferença significativa no consumo de cerveja entre dias de semana e finais de semana.
amostra_semana <- dados$ConsumoCerveja[dados$Final.de.Semana == "Não"]
amostra_fds <- dados$ConsumoCerveja[dados$Final.de.Semana == "Sim"]

shapiro.test(amostra_semana) #p-value = 0.06494
shapiro.test(amostra_fds) #p-value = 0.3039

wilcox.test(amostra_semana, amostra_fds) #p-value < 2.2e-16
#há uma diferença significativa no consumo de cerveja entre os dias de semana e os finais de semana.

