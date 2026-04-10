#Análise Visual da Previsão
g_validacao <- previsao_teste %>%
  autoplot(varejo_ts %>% filter_index("2022 Jan" ~ .), level = NULL) +
  labs(title = "Comparação de Modelos SARIMA no Teste (2024)", x = "Mês", y = "Índice") +
  theme_minimal()
print(g_validacao)


# Previsão Final para 2025
previsao_futuro <- modelo_definitivo %>% forecast(h = "12 months")

g_futuro <- previsao_futuro %>%
  autoplot(varejo_ts %>% filter_index("2020 Jan" ~ .)) +
  labs(title = "Previsão: Vendas do Varejo para 2025",
       subtitle = "ARIMA(0,1,3)(0,1,1)[12]",
       y = "Índice de Vendas", 
       x = "Ano/Mês") +
  theme_minimal()

print(g_futuro)