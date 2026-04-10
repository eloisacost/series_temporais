# Análise de Séries Temporais com Modelo SARIMA
# Índice de Vendas do Varejo

# 1. pacotes
library(tidyverse)
library(fpp3)
library(dplyr)
library(TSA)
library(forecast)
library(knitr)

# DADOS 
url_bcb <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.1455/dados?formato=csv"
dados_brutos <- read.csv(url_bcb, sep = ";", dec = ",")

varejo_ts <- dados_brutos %>%
  mutate(Data = dmy(data), Mes = yearmonth(Data)) %>%
  select(Mes, valor) %>%
  as_tsibble(index = Mes) %>%
  filter_index("2015 Jan" ~ "2024 Dec")

# Gráfico da Série Original
g_original <- autoplot(varejo_ts, valor) +
  labs(title = "Série Original: Vendas do Varejo", x = "Mês", y = "Índice") +
  theme_minimal()
print(g_original)

# Teste de estacionariedade na série original
varejo_ts %>% features(valor, unitroot_kpss) %>% kable()

# Decomposição da Série
g_decomposicao <- varejo_ts %>%
  model(STL(valor ~ trend(window = 13) + season(window = "periodic"))) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição STL da Série Original")
print(g_decomposicao)

# Separação treino e teste
treino <- varejo_ts %>% filter_index(. ~ "2023 Dec")
teste  <- varejo_ts %>% filter_index("2024 Jan" ~ .)

# 1ª diferenciação (Sazonal)
treino_diff_sazonal <- treino %>%
  mutate(diff_12 = difference(valor, 12)) %>%
  drop_na()

g_acf_1 <- treino_diff_sazonal %>%
  gg_tsdisplay(diff_12, plot_type = 'partial', lag_max = 36) +
  labs(title = "Diferença (Sazonal)", subtitle = "")
print(g_acf_1)

# 2ª diferenciação (Tendência e Sazonal)
treino_diff_total <- treino_diff_sazonal %>%
  mutate(diff_final = difference(diff_12, 1)) %>%
  drop_na()

g_acf_final <- treino_diff_total %>%
  gg_tsdisplay(diff_final, plot_type = 'partial', lag_max = 36) +
  labs(title = "Série Totalmente Estacionária", subtitle = "")
print(g_acf_final)

# Teste KPSS
treino_diff_total %>% 
  features(diff_final, unitroot_kpss) %>% kable()
