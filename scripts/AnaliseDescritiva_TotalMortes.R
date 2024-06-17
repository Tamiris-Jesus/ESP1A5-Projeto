
install.packages("readr")
library(readr)
install.packages("dplyr") # mudancas no data frame
library(dplyr)
install.packages("ggplot2") # biblioteca gráfica
library(ggplot2)
install.packages("lattice")
library(lattice)
install.packages("tidyr")
library(tidyr)


total_mortes <- read.csv(file.choose())
vacinacao <- read.csv(file.choose())
total_mortes$date <- as.Date(total_mortes$date)## garantir que data esta formatada corretamente

#total_mortes$World

#Medidas de Centralidade:
media_TotalMortes <- mean(total_mortes$World, na.rm = TRUE)
media_aparada_TotalMortes <- mean(total_mortes$World, trim = 0.1, na.rm = TRUE)
mediana_TotalMortes <- median(total_mortes$World, na.rm = TRUE)

cat("Média:", media_TotalMortes, "\n")
cat("Média Aparada:", media_aparada_TotalMortes, "\n")
cat("Mediana:", mediana_TotalMortes, "\n")

#Medidas de Variação
variancia_TotalMortes <- var(total_mortes$World, na.rm = TRUE)
desvio_padrao_TotalMortes <- sd(total_mortes$World, na.rm = TRUE)
coef_variacao_TotalMortes <- (desvio_padrao_TotalMortes / media_TotalMortes) * 100

cat("Variância:", variancia_TotalMortes, "\n")
cat("Desvio Padrão:", desvio_padrao_TotalMortes, "\n")
cat("Coeficiente de Variação (%):", coef_variacao_TotalMortes, "\n")

#Boxplot
summary_TotalMortes <- summary(total_mortes$World)
boxplot(total_mortes$World, horizontal = TRUE, main = "Mortes por Covid-19 no Mundo", xlab = "Número de Mortes", col = "pink")
abline(v = media_TotalMortes, col = "red", lwd = 2, lty = 2)
text(x = summary_TotalMortes["Min."], y = 1.2, labels = paste("Min:", summary_TotalMortes["Min."]), col = "blue", cex = 0.8)
text(x = summary_TotalMortes["1st Qu."], y = 1.2, labels = paste("1st Qu.:", summary_TotalMortes["1st Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortes["Median"], y = 1.2, labels = paste("Median:", summary_TotalMortes["Median"]), col = "blue", cex = 0.8)
text(x = summary_TotalMortes["Mean"], y = 1.2, labels = paste("Mean:", round(summary_TotalMortes["Mean"], 2)), col = "red", cex = 0.8)
text(x = summary_TotalMortes["3rd Qu."], y = 1.2, labels = paste("3rd Qu.:", summary_TotalMortes["3rd Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortes["Max."], y = 1.2, labels = paste("Max:", summary_TotalMortes["Max."]), col = "blue", cex = 0.8)


#total_mortes$date

total_mortes_long <- pivot_longer(total_mortes, cols = c(Asia, South.America, North.America, Africa, Oceania, Europe), 
                                  names_to = "continent", values_to = "deaths")
str(total_mortes_long)
total_mortes_long <- total_mortes_long %>%
  mutate(deaths = ifelse(is.na(deaths), 0, deaths))

total_mortes_long <- drop_na(total_mortes_long)

ggplot(total_mortes_long, aes(x = date, y = deaths, color = continent)) +
  geom_line(size = 1) +
  labs(title = "Mortes por COVID-19 ao Longo do Tempo por Continente", x = "Data", y = "Número de Mortes") +
  theme_minimal() +
  theme(legend.title = element_blank())

#Continente

colnames(total_mortes)



#Medidas de Centralidade:
media_America_Sul <- mean(total_mortes$South.America, na.rm = TRUE)
media_aparada_America_Sul <- mean(total_mortes$South.America, trim = 0.1,  na.rm = TRUE)
mediana_America_Sul <- median(total_mortes$South.America,  na.rm = TRUE)

cat("Média:", media_America_Sul, "\n")
cat("Média Aparada:", media_aparada_America_Sul, "\n")
cat("Mediana:", mediana_America_Sul, "\n")

#Medidas de Variação
variancia_America_Sul <- var(total_mortes$South.America, na.rm = TRUE)
desvio_padrao_America_Sul <- sd(total_mortes$South.America, na.rm = TRUE)
coef_variacao_America_Sul <- (desvio_padrao_America_Sul / media_America_Sul) * 100

cat("Variância:", variancia_America_Sul, "\n")
cat("Desvio Padrão:", desvio_padrao_America_Sul, "\n")
cat("Coeficiente de Variação (%):", coef_variacao_America_Sul, "\n")

#Boxplot

summary_TotalMortesSouthAmerica <- summary(total_mortes$South.America, na.rm = TRUE)
summary(total_mortes$South.America, na.rm = TRUE)
boxplot(total_mortes$South.America, horizontal = TRUE, main = "Mortes por Covid-19 na America do Sul", xlab = "Número de Mortes", col = "orange")
abline(v = media_America_Sul, col = "red", lwd = 2, lty = 2)

text(x = summary_TotalMortesSouthAmerica["Min."], y = 1.2, labels = paste("Min:", summary_TotalMortesSouthAmerica["Min."]), col = "blue", cex = 0.8)
text(x = summary_TotalMortesSouthAmerica["1st Qu."], y = 1.2, labels = paste("1st Qu.:", summary_TotalMortesSouthAmerica["1st Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortesSouthAmerica["Median"], y = 1.2, labels = paste("Median:", summary_TotalMortesSouthAmerica["Median"]), col = "blue", cex = 0.8)
text(x = summary_TotalMortesSouthAmerica["Mean"], y = 1.2, labels = paste("Mean:", round(summary_TotalMortesSouthAmerica["Mean"], 2)), col = "red", cex = 0.8)
text(x = summary_TotalMortesSouthAmerica["3rd Qu."], y = 1.2, labels = paste("3rd Qu.:", summary_TotalMortesSouthAmerica["3rd Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortesSouthAmerica["Max."], y = 1.2, labels = paste("Max:", summary_TotalMortesSouthAmerica["Max."]), col = "blue", cex = 0.8)



hist(total_mortes$North.America,  main = "Histograma de Mortes por COVID-19 na América do Norte ", xlab = "Número de Mortes", ylab = "Frequência")
plot(total_mortes$date, total_mortes$North.America, type = "b", main = "Mortes por COVID-19 ao Longo do Tempo na América do Norte", 
     xlab = "Data", ylab = "Mortes Acumuladas", col = "blue", pch = 19)
#Medidas de Centralidade:
media_America_Norte <- mean(total_mortes$North.America, na.rm = TRUE)
media_aparada_America_Norte <- mean(total_mortes$North.America, trim = 0.1, na.rm = TRUE)
mediana_America_Norte <- median(total_mortes$North.America,  na.rm = TRUE)

cat("Média:", media_America_Norte, "\n")
cat("Média Aparada:", media_aparada_America_Norte, "\n")
cat("Mediana:", mediana_America_Norte, "\n")

#Medidas de Variação
variancia_America_Norte <- var(total_mortes$North.America, na.rm = TRUE)
desvio_padrao_America_Norte <- sd(total_mortes$North.America, na.rm = TRUE)
coef_variacao_America_Norte <- (desvio_padrao_America_Norte / media_America_Norte) * 100

cat("Variância:", variancia_America_Norte, "\n")
cat("Desvio Padrão:", desvio_padrao_America_Norte, "\n")
cat("Coeficiente de Variação (%):", coef_variacao_America_Norte, "\n")

#Boxplot
boxplot(total_mortes$North.America, main = "Mortes Totais América do Norte", ylab = "Número de Mortes")

summary_TotalMortesNorthAmerica <- summary(total_mortes$North.America, na.rm = TRUE)
summary(total_mortes$North.America, na.rm = TRUE)
boxplot(total_mortes$North.America, horizontal = TRUE, main = "Mortes por Covid-19 na America do Norte", xlab = "Número de Mortes", col = "lightblue")
abline(v = media_America_Norte, col = "red", lwd = 2, lty = 2)

text(x = summary_TotalMortesNorthAmerica["Min."], y = 1.2, labels = paste("Min:", summary_TotalMortesNorthAmerica["Min."]), col = "blue", cex = 0.8)
text(x = summary_TotalMortesNorthAmerica["1st Qu."], y = 1.2, labels = paste("1st Qu.:", summary_TotalMortesNorthAmerica["1st Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortesNorthAmerica["Median"], y = 1.2, labels = paste("Median:", summary_TotalMortesNorthAmerica["Median"]), col = "blue", cex = 0.8)
text(x = summary_TotalMortesNorthAmerica["Mean"], y = 1.2, labels = paste("Mean:", round(summary_TotalMortesNorthAmerica["Mean"], 2)), col = "red", cex = 0.8)
text(x = summary_TotalMortesNorthAmerica["3rd Qu."], y = 1.2, labels = paste("3rd Qu.:", summary_TotalMortesNorthAmerica["3rd Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortesNorthAmerica["Max."], y = 1.2, labels = paste("Max:", summary_TotalMortesNorthAmerica["Max."]), col = "blue", cex = 0.8)


#Medidas de Centralidade:
media_Africa <- mean(total_mortes$Africa, na.rm = TRUE)
media_aparada_Africa <- mean(total_mortes$Africa, trim = 0.1, na.rm = TRUE)
mediana_Africa <- median(total_mortes$Africa,  na.rm = TRUE)

cat("Média:", media_Africa, "\n")
cat("Média Aparada:", media_aparada_Africa, "\n")
cat("Mediana:", mediana_Africa, "\n")

#Medidas de Variação
variancia_Africa <- var(total_mortes$Africa, na.rm = TRUE)
desvio_padrao_Africa <- sd(total_mortes$Africa, na.rm = TRUE)
coef_variacao_Africa <- (desvio_padrao_Africa / media_Africa) * 100

cat("Variância:", variancia_Africa, "\n")
cat("Desvio Padrão:", desvio_padrao_Africa, "\n")
cat("Coeficiente de Variação (%):", coef_variacao_Africa, "\n")

#Boxplot
summary_TotalMortesAfrica <- summary(total_mortes$Africa, na.rm = TRUE)
summary(total_mortes$Africa)
boxplot(total_mortes$Africa, horizontal = TRUE, main = "Mortes por Covid-19 na Africa", xlab = "Número de Mortes", col = "yellow")
abline(v = media_Africa, col = "red", lwd = 2, lty = 2)

text(x = summary_TotalMortesAfrica["Min."], y = 1.2, labels = paste("Min:", summary_TotalMortesAfrica["Min."]), col = "blue", cex = 0.8)
text(x = summary_TotalMortesAfrica["1st Qu."], y = 1.2, labels = paste("1st Qu.:", summary_TotalMortesAfrica["1st Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortesAfrica["Median"], y = 1.2, labels = paste("Median:", summary_TotalMortesAfrica["Median"]), col = "blue", cex = 0.8)
text(x = summary_TotalMortesAfrica["Mean"], y = 1.2, labels = paste("Mean:", round(summary_TotalMortesAfrica["Mean"], 2)), col = "red", cex = 0.8)
text(x = summary_TotalMortesAfrica["3rd Qu."], y = 1.2, labels = paste("3rd Qu.:", summary_TotalMortesAfrica["3rd Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortesAfrica["Max."], y = 1.2, labels = paste("Max:", summary_TotalMortesAfrica["Max."]), col = "blue", cex = 0.8)



#Medidas de Centralidade:
media_Asia <- mean(total_mortes$Asia, na.rm = TRUE)
media_aparada_Asia <- mean(total_mortes$Asia, trim = 0.1,  na.rm = TRUE)
mediana_Asia <- median(total_mortes$Asia,  na.rm = TRUE)

cat("Média:", media_Asia, "\n")
cat("Média Aparada:", media_aparada_Asia, "\n")
cat("Mediana:", mediana_Asia, "\n")

#Medidas de Variação
variancia_Asia <- var(total_mortes$Asia, na.rm = TRUE)
desvio_padrao_Asia <- sd(total_mortes$Asia, na.rm = TRUE)
coef_variacao_Asia <- (desvio_padrao_Asia / media_Asia) * 100

cat("Variância:", variancia_Asia, "\n")
cat("Desvio Padrão:", desvio_padrao_Asia, "\n")
cat("Coeficiente de Variação (%):", coef_variacao_Asia, "\n")

#Boxplot

summary_TotalMortesAsia <- summary(total_mortes$Asia, na.rm = TRUE)
summary(total_mortes$Asia)
boxplot(total_mortes$Asia, horizontal = TRUE, main = "Mortes por Covid-19 na Asia", xlab = "Número de Mortes", col = "green")
abline(v = media_Asia, col = "red", lwd = 2, lty = 2)

text(x = summary_TotalMortesAsia["Min."], y = 1.2, labels = paste("Min:", summary_TotalMortesAsia["Min."]), col = "blue", cex = 0.8)
text(x = summary_TotalMortesAsia["1st Qu."], y = 1.2, labels = paste("1st Qu.:", summary_TotalMortesAsia["1st Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortesAsia["Median"], y = 1.2, labels = paste("Median:", summary_TotalMortesAsia["Median"]), col = "blue", cex = 0.8)
text(x = summary_TotalMortesAsia["Mean"], y = 1.2, labels = paste("Mean:", round(summary_TotalMortesAsia["Mean"], 2)), col = "red", cex = 0.8)
text(x = summary_TotalMortesAsia["3rd Qu."], y = 1.2, labels = paste("3rd Qu.:", summary_TotalMortesAsia["3rd Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortesAsia["Max."], y = 1.2, labels = paste("Max:", summary_TotalMortesAsia["Max."]), col = "blue", cex = 0.8)

#Medidas de Centralidade:
media_Europa <- mean(total_mortes$Europe, na.rm = TRUE)
media_aparada_Europa <- mean(total_mortes$Europe, trim = 0.1, na.rm = TRUE)
mediana_Europa <- median(total_mortes$Europe,  na.rm = TRUE)

cat("Média:", media_Europa, "\n")
cat("Média Aparada:", media_aparada_Europa, "\n")
cat("Mediana:", mediana_Europa, "\n")

#Medidas de Variação
variancia_Europa <- var(total_mortes$Europe, na.rm = TRUE)
desvio_padrao_Europa <- sd(total_mortes$Europe, na.rm = TRUE)
coef_variacao_Europa <- (desvio_padrao_Europa / media_Europa) * 100

cat("Variância:", variancia_Europa, "\n")
cat("Desvio Padrão:", desvio_padrao_Europa, "\n")
cat("Coeficiente de Variação (%):", coef_variacao_Europa, "\n")

#Boxplot
boxplot(total_mortes$Europe, main = "Mortes Totais na Europa", ylab = "Número de Mortes")

summary_TotalMortesEuropa <- summary(total_mortes$Europe, na.rm = TRUE)
summary(total_mortes$Europe, na.rm = TRUE)
boxplot(total_mortes$Europe, horizontal = TRUE, main = "Mortes por Covid-19 na Europa", xlab = "Número de Mortes", col = "grey")
abline(v = media_Europa, col = "red", lwd = 2, lty = 2)

text(x = summary_TotalMortesEuropa["Min."], y = 1.2, labels = paste("Min:", summary_TotalMortesEuropa["Min."]), col = "blue", cex = 0.8)
text(x = summary_TotalMortesEuropa["1st Qu."], y = 1.2, labels = paste("1st Qu.:", summary_TotalMortesEuropa["1st Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortesEuropa["Median"], y = 1.2, labels = paste("Median:", summary_TotalMortesEuropa["Median"]), col = "blue", cex = 0.8)
text(x = summary_TotalMortesEuropa["Mean"], y = 1.2, labels = paste("Mean:", round(summary_TotalMortesEuropa["Mean"], 2)), col = "red", cex = 0.8)
text(x = summary_TotalMortesEuropa["3rd Qu."], y = 1.2, labels = paste("3rd Qu.:", summary_TotalMortesEuropa["3rd Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortesEuropa["Max."], y = 1.2, labels = paste("Max:", summary_TotalMortesEuropa["Max."]), col = "blue", cex = 0.8)


#Medidas de Centralidade:
media_Oceania <- mean(total_mortes$Oceania, na.rm = TRUE)
media_aparada_Oceania <- mean(total_mortes$Oceania, trim = 0.1, na.rm = TRUE)
mediana_Oceania <- median(total_mortes$Oceania,  na.rm = TRUE)

cat("Média:", media_Oceania, "\n")
cat("Média Aparada:", media_aparada_Oceania, "\n")
cat("Mediana:", mediana_Oceania, "\n")

#Medidas de Variação
variancia_Oceania <- var(total_mortes$Oceania, na.rm = TRUE)
desvio_padrao_Oceania <- sd(total_mortes$Oceania, na.rm = TRUE)
coef_variacao_Oceania <- (desvio_padrao_Oceania / media_Oceania) * 100

cat("Variância:", variancia_Oceania, "\n")
cat("Desvio Padrão:", desvio_padrao_Oceania, "\n")
cat("Coeficiente de Variação (%):", coef_variacao_Oceania, "\n")

#Boxplot
boxplot(total_mortes$Oceania, main = "Mortes Totais na Oceania", ylab = "Número de Mortes")

summary_TotalMortesOceania <- summary(total_mortes$Oceania, na.rm = TRUE)
summary(total_mortes$Oceania, na.rm = TRUE)
boxplot(total_mortes$Oceania, horizontal = TRUE, main = "Mortes por Covid-19 na Oceania", xlab = "Número de Mortes", col = "lightgreen")
abline(v = media_Oceania, col = "red", lwd = 2, lty = 2)

text(x = summary_TotalMortesOceania["Min."], y = 1.2, labels = paste("Min:", summary_TotalMortesOceania["Min."]), col = "blue", cex = 0.8)
text(x = summary_TotalMortesOceania["1st Qu."], y = 1.2, labels = paste("1st Qu.:", summary_TotalMortesOceania["1st Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortesOceania["Median"], y = 1.2, labels = paste("Median:", summary_TotalMortesOceania["Median"]), col = "blue", cex = 0.8)
text(x = summary_TotalMortesOceania["Mean"], y = 1.2, labels = paste("Mean:", round(summary_TotalMortesOceania["Mean"], 2)), col = "red", cex = 0.8)
text(x = summary_TotalMortesOceania["3rd Qu."], y = 1.2, labels = paste("3rd Qu.:", summary_TotalMortesOceania["3rd Qu."]), col = "black", cex = 0.8)
text(x = summary_TotalMortesOceania["Max."], y = 1.2, labels = paste("Max:", summary_TotalMortesOceania["Max."]), col = "blue", cex = 0.8)



