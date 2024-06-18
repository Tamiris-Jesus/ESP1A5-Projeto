install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("GGally")
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)

# Base de dados da vacinação
vacinacao <- read.csv(file.choose())
head(vacinacao)
str(vacinacao)
vacinacao_sem_na <- vacinacao %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

#Base de dados do total de mortes
total_mortes <- read.csv(file.choose())
head(total_mortes)
str(total_mortes)
total_mortes_sem_na <- total_mortes %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

# Tirar uma amostra aleatória de 5000 elementos
set.seed(123)
sample_vaccinations <- sample(vacinacao_sem_na$total_vaccinations_per_hundred, size = min(5000, nrow(vacinacao_sem_na)))
sample_people_vaccinated <- sample(vacinacao_sem_na$people_fully_vaccinated_per_hundred, size = min(5000, nrow(vacinacao_sem_na)))

# Teste de Shapiro-Wilk na amostra de vacinação
shapiro.test(sample_vaccinations)
shapiro.test(sample_people_vaccinated)

# Teste de Shapiro-Wilk na amostra de total de mortes
shapiro.test(total_mortes_sem_na$World)
shapiro.test(total_mortes$Africa)
shapiro.test(total_mortes$North.America)
shapiro.test(total_mortes$South.America)
shapiro.test(total_mortes$Asia)
shapiro.test(total_mortes$Europe)
shapiro.test(total_mortes$Oceania)

# HISTOGRAMA
# Calcular a média e o desvio padrão
media <- mean(vacinacao_sem_na$total_vaccinations_per_hundred)
sd <- sd(vacinacao_sem_na$total_vaccinations_per_hundred)

# Criar o gráfico
ggplot(vacinacao_sem_na, aes(x = total_vaccinations_per_hundred)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = media, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media - sd, color = "grey", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media + sd, color = "grey", linetype = "dashed", size = 1) +
  labs(title = "Gráfico Total de Vacinações",
       x = "Total de Vacinações por Cem Habitantes",
       y = "Densidade") +
  theme_minimal()

# HISTOGRAMA
# Calcular a média e o desvio padrão
media <- mean(vacinacao_sem_na$people_fully_vaccinated_per_hundred)
sd <- sd(vacinacao_sem_na$people_fully_vaccinated_per_hundred)

# Criar o gráfico
ggplot(vacinacao_sem_na, aes(x = people_fully_vaccinated_per_hundred)) +
  geom_density(fill = "green", alpha = 0.5) +
  geom_vline(xintercept = media, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media - sd, color = "grey", linetype = "dashed", size = 1) +
  geom_vline(xintercept = media + sd, color = "grey", linetype = "dashed", size = 1) +
  labs(title = "Gráfico Pessoas Totalmente Vacinadas",
       x = "Total de Vacinações por Cem Habitantes",
       y = "Densidade") +
  theme_minimal()

# QQPlot
qqnorm(vacinacao_sem_na$total_vaccinations_per_hundred)
qqline(vacinacao_sem_na$total_vaccinations_per_hundred, col = "red")

qqnorm(vacinacao_sem_na$people_fully_vaccinated_per_hundred)
qqline(vacinacao_sem_na$people_fully_vaccinated_per_hundred, col = "red")
