# Análise de Séries Temporais com Modelo ARIMA
# Inadimplência do Consumidor (PEIC)

library(tidyverse)
library(fpp3)
library(knitr)

# Importação dos dados
dados_peic <- read.csv("/Users/eloisa/Documents/UEPB 2/SERIES/Projeto 2/ARIMA_PEIC/data/dados_inadin.csv", sep = ";", dec = ",")

peic_ts <- dados_peic %>%
  mutate(Data = ym(as.character(Data)), Mes = yearmonth(Data)) %>%
  select(Mes, valor = endividamento.e.inadimplência.do.consumidor) %>% 
  as_tsibble(index = Mes)

# Visualização e Análise Inicial
g_original <- autoplot(peic_ts, valor) +
  labs(title = "Taxa de Inadimplência Original (PEIC)", 
       x = "Mês", y = "Taxa (%)") +
  theme_minimal()
print(g_original)

#ACF e PACF Inicial
acf <- peic_ts %>%
  ACF(valor, lag_max = 20) %>% 
  autoplot() +
  labs(title = "ACF")

pacf <- peic_ts %>%
  PACF(valor, lag_max = 20) %>% 
  autoplot() +
  labs(title = "PACF")

gridExtra::grid.arrange(
  g_original, acf, pacf,
  layout_matrix = rbind(c(1,1),c(2,3))
)

# Teste de estacionariedade original
peic_ts %>% features(valor, unitroot_kpss) %>% kable()

# Separação treino e teste
treino <- peic_ts %>% filter_index(. ~ "2024 Dec")
teste  <- peic_ts %>% filter_index("2025 Jan" ~ .)

# Análise da Série Diferenciada
plot_serieD <- treino %>%
  autoplot(difference(valor)) +
  labs(
    title = "Inadimplencia (diferenciada)",
    y = "% taxa"
  )
plot_serieD

# ACF e PACF serie diferenciada
acf_D <- treino %>%
  ACF(difference(valor), lag_max = 20) %>% 
  autoplot() +
  labs(title = "ACF")

pacf_D <- treino %>%
  PACF(difference(valor), lag_max = 20) %>% 
  autoplot() +
  labs(title = "PACF")

gridExtra::grid.arrange(
  plot_serieD, acf_D, pacf_D,
  layout_matrix = rbind(c(1,1),c(2,3))
)

# Teste de estacionariedade diferenciada
treino %>% features(difference(valor), unitroot_kpss) %>% kable()
