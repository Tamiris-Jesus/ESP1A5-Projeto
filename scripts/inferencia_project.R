total_mortes <- read.csv(file.choose())
vacinacao <- read.csv(file.choose())

# Selecionando dados relevantes para comparar um período específico
word_deaths <- total_mortes$World[
  total_mortes$date >= as.Date("2020-12-02") & total_mortes$date <= as.Date("2024-06-02")
]

word_vaccinations <- vacinacao$total_vaccinations[
  vacinacao$date >= as.Date("2020-12-02") & vacinacao$date <= as.Date("2024-06-02")
]

#Tamanho das variáveis
length(word_deaths)
length(word_vaccinations)

# Teste t para médias sem emparelhamento
t_result <- t.test(word_deaths, word_vaccinations)

# Resultado do teste
print(t_result)

# Conclusão do teste baseado no resultado
alpha <- 0.05
if (t_result$p.value < alpha) {
  cat("Há evidências estatísticas para rejeitar a hipótese nula.\n")
  cat("Isso sugere que a mortalidade pode ter diminuído com o aumento da vacinação.\n")
} else {
  cat("Não há evidências estatísticas para rejeitar a hipótese nula.\n")
  cat("Isso sugere que não há diferença significativa na mortalidade com o aumento da vacinação.\n")
}