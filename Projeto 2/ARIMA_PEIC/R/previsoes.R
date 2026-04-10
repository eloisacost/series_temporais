# Validação com dados de teste (2025)
previsao_teste <- fit %>% forecast(new_data = teste)

g_validacao <- previsao_teste %>%
  autoplot(peic_ts %>% filter_index("2022 Jan" ~ .), level = NULL) +
  labs(title = "Comparação de Desempenho no Teste (2025)", 
       x = "Mês", y = "Taxa (%)") +
  theme_minimal()
print(g_validacao)

# Previsão Futura (2026)
previsao_futuro <- modelo_definitivo %>% forecast(h = "12 months")

g_futuro <- previsao_futuro %>%
  autoplot(peic_ts %>% filter_index("2020 Jan" ~ .)) +
  labs(title = "Previsão Oficial: Taxa de Inadimplência para 2026",
       subtitle = "Projeção com o Modelo ETS(A,M,N)",
       y = "Taxa (%)", x = "Mês/Ano") +
  theme_minimal()
print(g_futuro)
