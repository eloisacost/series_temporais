library(TSA)
library(forecast)
library(fpp3)
library(readr)
library(readxl)
library(dplyr)

PIB_MENSAL1 <- read_excel("UEPB/SERIES/PIB_MENSAL1.xls")
head(PIB_MENSAL1)

## Transformando a variavel Sales no formato TS
y = ts( PIB_MENSAL1$PIB, start = 1990,end = 2025,  frequency = 12  )

plot.ts( y  )

### transformando para o formato tstible *
  serie <- PIB_MENSAL1 %>%
  mutate(Data = yearmonth(Data)) %>%
  as_tsibble(index = Data)
serie


### Plot das series
serie %>%
  pivot_longer(-Data) %>%
  ggplot(aes(x = Data, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

### Estudando a sazonalidade

serie %>%
  gg_season(PIB, labels = "both") +
  labs(y = "",
       title = "Sazonalidade")

serie %>%
  gg_subseries(PIB) +
  labs(
    y = "",
    title = "Sazonalidade:PIB "
  )


serie$PIB <- as.numeric(serie$PIB)

serie %>%
  model(classical_decomposition(PIB, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição clássica aditiva do PIB")

serie %>%
  model(
    classical_decomposition(PIB, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição clássica aditiva do PIB")

