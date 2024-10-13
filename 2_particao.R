# Carregar bibliotecas
library(tidyverse)
library(caret)
library(tictoc)

dados = dados_filtrados_sem_na

# Definir semente para reprodutibilidade
set.seed(659)

# Adicionar identificador
dados_particao <- dados %>%
  dplyr::mutate(ID = row_number())

# Primeira partição: 70% para treinamento, 30% para teste e validação
particao_treino <- createDataPartition(dados_particao$status_doenca_final_trat,
                                       p = 0.8,
                                       list = FALSE)
dados_treinamento <- dados_particao[particao_treino, ]

dados_teste <- dados_particao[-particao_treino, ]

estrutura_treino <-
  dados_treinamento %>%
  ff_glimpse()

estrutura_treino = estrutura_treino$Categorical

estrutura_teste <-
  dados_teste %>%
  ff_glimpse()

estrutura_teste = estrutura_teste$Categorical

# ajustando fatores -------------------------------------------------------
make_valid_factor_levels <- function(df) {
  factor_vars <- sapply(df, is.factor)
  
  df[factor_vars] <- lapply(df[factor_vars], function(x) {
    levels(x) <- make.names(levels(x))
    x
  })
  
  return(df)
}

# Aplicar a função aos conjuntos de dados
dados_treinamento <- make_valid_factor_levels(dados_treinamento)
dados_teste <- make_valid_factor_levels(dados_teste)

rm(list =
     base::setdiff(
       ls(),
       c(
         "dados_treinamento",
         "dados_teste",
         "dados"
       )
     ))

save.image("2_particao_dados_09102024.Rdata")
