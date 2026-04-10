# Carrega pacotes necessários
if(!require(forecast)) install.packages("forecast")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(lubridate)) install.packages("lubridate")


library(forecast)
library(tidyverse)
library(lubridate)


# 1. Leitura dos Dados
# O arquivo usa separador ';' e decimal ','


dados_brutos <- read.csv("C:/Users/Elô/Documents/UEPB/SERIES/Projeto 1/data/dados.csv", sep = ";", dec = ",")

# 2. Limpeza e Tratamento
dados_clean <- dados_brutos %>%
  rename(data = Data, valor = 2) %>% # Renomeia colunas
  mutate(
    # Ajusta formato da data de YYYY.MM para Date real
    data_formatada = ymd(paste0(str_replace(data, "\\.", "-"), "-01"))
  )

dados_clean

# 3. Transformação em Objeto TS (Time Series)
# Frequência 12 (Mensal), Inicio 2013, Mês 1
ts_inadimplencia <- ts(dados_clean$valor, start = c(2013, 1), frequency = 12)

saveRDS(ts_inadimplencia, "C:/Users/Elô/Documents/UEPB/SERIES/Projeto 1/data/ts_inadimplencia.rds")

