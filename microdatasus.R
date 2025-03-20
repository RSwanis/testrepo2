## Acesso ao Microdata SUS ##

#install.packages("remotes")
library(remotes)

remotes::install_github("rfsaldanha/microdatasus")

## Realizado as devidas instalações dos pacotes ##

library(microdatasus)
library(dplyr)
library(tidyverse)

## Coleta de dados ## Exemplo 

dados = fetch_datasus(year_start = 2000,
                      year_end = 2023,
                      uf = "MG",
                      information_system = "SIM-DO")
### Processando os dados ### ficar no padrão

dados_processo = process_sim(dados)

## filtrar cidade relativa 

dados_processo_filter = dados_processo %>%
  filter(grepl("314610",CODMUNOCOR))

dados_contato = dados_processo_filter %>%
  mutate(ano = year(DTOBITO)) %>%
  count(ano, CAUSABAS)

## Ordadenando pelas principais causas básicas ###

dados_ordenados = dados_contato[order(-dados_contato$ano, -dados_contato$n), ]

## Fazendo um slice (pareto) das top 3 causas básicas ##

top_10_por_ano <- dados_ordenados %>%
  group_by(ano) %>%
  slice_head(n = 3) %>%
  mutate(percentual = (n / sum(n))*100)

ggplot(top_10_por_ano, aes(x = ano, y = percentual)) +
  geom_col(aes(fill = CAUSABAS, color = CAUSABAS)) +
  theme_minimal() +
  labs(title = "Principais causas de morte - Ouro Preto (2010-2021)",
       subtitle = "CID - Classificação Internacional de Doenças",
       caption = "Elaboração: Vinicius Santos - Dados: Datasus",
       y = "%",
       x = "")


