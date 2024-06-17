#Importação de bibliotecas
install.packages(c("ggplot2", "dplyr", "psych"))

# Carregar pacotes
library(ggplot2)
library(dplyr)
library(psych)

#Importando base de dados 
vacinacao <- read.csv(file.choose())
#Variveis utilizadas
# date
# people_vaccinated
# daily_people_vaccinated
# location

#Preparação dos dados

# Criando Vetor com os nomes dos continentes para filtragem na base
continentes <- c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")


# Filtrar linhas que contêm nomes de continentes
vacinacao_continentes <- vacinacao %>% 
  filter(location %in% continentes)

# Remover linhas que possuem NA em qualquer uma das variáveis de interesse
vacinacao_continentes <- na.omit(vacinacao_continentes[c("date", "people_vaccinated", "daily_people_vaccinated", "location")])

# Verificar se ainda existem NA após a limpeza
sum(is.na(vacinacao_continentes))

# Converter a coluna 'date' para o formato Date
vacinacao_continentes$date <- as.Date(vacinacao_continentes$date)

#--------  ANÁLISE UNIVARIADA DOS DADOS 
# Visualizar as primeiras linhas
head(vacinacao_continentes)

# Resumo estatístico
summary(vacinacao_continentes[, c("people_vaccinated", "daily_people_vaccinated")])

#Calculando média aparada 
mean_people_vaccinated <- mean(vacinacao_continentes$people_vaccinated, trim = 0.1, na.rm = TRUE)
mean_daily_people_vaccinated <- mean(vacinacao_continentes$daily_people_vaccinated, trim = 0.1, na.rm = TRUE)
# Mostrar os resultados
cat("Média aparada (10%) para people_vaccinated: ", mean_people_vaccinated, "\n")
cat("Média aparada (10%) para daily_people_vaccinated: ", mean_daily_people_vaccinated, "\n")

# Calcular a variância e o desvio padrão
variancia_people_vaccinated <- var(vacinacao_continentes$people_vaccinated, na.rm = TRUE)
desvio_padrao_people_vaccinated <- sd(vacinacao_continentes$people_vaccinated, na.rm = TRUE)

variancia_daily_people_vaccinated <- var(vacinacao_continentes$daily_people_vaccinated, na.rm = TRUE)
desvio_padrao_daily_people_vaccinated <- sd(vacinacao_continentes$daily_people_vaccinated, na.rm = TRUE)

# Imprimir os resultados
variancia_people_vaccinated
desvio_padrao_people_vaccinated

variancia_daily_people_vaccinated
desvio_padrao_daily_people_vaccinated

# ---------------  ANALISANDO OS DADOS POR CONTINENTE 
# ---------- RESUMO ESTATÍSTICO DOS DADOS 

# Análise agrupada por continente
analise_por_continente <- vacinacao_continentes %>%
  group_by(location) %>%
  summarise(
    people_vaccinated = max(people_vaccinated, na.rm = TRUE),
    daily_people_vaccinated = mean(daily_people_vaccinated, trim = 0.1, na.rm = TRUE)
  )

print(analise_por_continente)

# -------------- Análises Bi Variadas --------
# -------------- GRÁFICOS 
grafico_pandemia <- ggplot(vacinacao_continentes, aes(x = date, y = people_vaccinated, color = location)) +
  geom_line() +
  labs(title = "Pessoas Vacinadas por Continente Durante a Pandemia",
       x = "Data",
       y = "Pessoas Vacinadas",
       color = "Continente") +
  theme_minimal()

print(grafico_pandemia)

# Gráfico de people_vaccinated por location (scatter plot)
ggplot(vacinacao_continentes, aes(x = location, y = people_vaccinated, color = location)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(x = "Localização", y = "Pessoas Vacinadas", title = "Pessoas Vacinadas por Localização") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Gráfico de daily_people_vaccinated por location (scatter plot)
ggplot(vacinacao_continentes, aes(x = location, y = daily_people_vaccinated, color = location)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(x = "Localização", y = "Pessoas Vacinadas Por dia", title = "Pessoas Vacinadas Diariamente por Localização") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Gráfico de people_vaccinated por location (barras empilhadas)
ggplot(vacinacao_continentes, aes(x = location, y = people_vaccinated, fill = location)) +
  geom_bar(stat = "identity") +
  labs(x = "Localização", y = "Pessoas Vacinadas", title = "Pessoas Vacinadas por Localização") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de daily_people_vaccinated vs. date
ggplot(vacinacao_continentes, aes(x = date, y = daily_people_vaccinated)) +
  geom_line(color = "#0073C2FF", linewidth = 1.2) +  
  labs(x = "Data", y = "Pessoas Vacinadas Diariamente", 
       title = "Evolução do número de Pessoas Vacinadas Diariamente") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  geom_point(color = "#0073C2FF", size = 2)


# Gráfico de dispersão com linha de regressão
ggplot(vacinacao_continentes, aes(x = date, y = people_vaccinated)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(x = "Data", y = "Total de Vacinações", title = "Total de Vacinações ao Longo do Tempo com Linha de Regressão") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# -------- Cálculos

# Calcular correlação entre date e people_vaccinated
correlacao_pessoasVacinadas_data <- cor(as.numeric(vacinacao_continentes$date), vacinacao_continentes$people_vaccinated)

# Exibir o valor da correlação
correlacao_pessoasVacinadas_data

# Calcular correlação entre date e daily_people_vaccinated
correlacao_vacinacao_diaria_data <- cor(as.numeric(vacinacao_continentes$date), vacinacao_continentes$daily_people_vaccinated) 

correlacao_vacinacao_diaria_data  




