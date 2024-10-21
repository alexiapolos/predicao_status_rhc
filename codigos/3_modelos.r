library(randomForest)
library(caret)
library(dplyr)
library(openxlsx)
library(MLmetrics)
library(tictoc)
library(xgboost)
library(doParallel)

dados_treinamento =
  dados_treinamento %>%
  dplyr::select(-ID)

dados_teste =
  dados_teste %>%
  dplyr::select(-ID)

num_variaveis <- 14

# Random Forest -----------------------------------------------------------
set.seed(352)

rf_grid <- expand.grid(
  mtry = c(3, 4, 5),                     # Valores para `mtry`
  splitrule = c("gini"),                  # Utilizar apenas "gini"
  min.node.size = c(1, 4)                 # Valores para `min.node.size`
)

# Configurar o controle de treinamento com 4 folds de cross-validation
ctrl <- trainControl(
  method = "cv",                          # Validação cruzada
  number = 4,                             # Número de folds
  classProbs = TRUE,                      # Calcular probabilidades
  summaryFunction = multiClassSummary,    # Resumo para classificação multiclasse
  returnData = TRUE,                      # Retornar os dados usados no treinamento
  savePredictions = TRUE,                 # Salvar as predições do modelo
  allowParallel = TRUE,                   # Permitir processamento paralelo
  verboseIter = FALSE                     # Suprimir mensagens de progresso
)

cl <- makeCluster(detectCores() - 1) 

registerDoParallel(cl)

# Lista de hiperparâmetros para número de árvores (trees)
num_arvores <- c(100, 200, 500)

# Loop para rodar o modelo para cada número de árvores definido
modelos_rf <- lapply(num_arvores, function(ntree) {
  
  # Definir semente para reprodutibilidade
  set.seed(352)
  
  # Treinamento do modelo Random Forest com os hiperparâmetros definidos
  rf_model <- train(
    status_doenca_final_trat ~ .,                    # Fórmula do modelo (variável alvo ~ variáveis preditoras)
    data = dados_treinamento,             # Conjunto de dados de treinamento
    method = "ranger",                    # Método Random Forest utilizando o pacote `ranger`
    trControl = ctrl,                     # Controle de treinamento
    tuneGrid = rf_grid,                   # Grid de hiperparâmetros especificado
    importance = "impurity",              # Calcular importância das variáveis
    na.action = na.omit,                  # Remover valores ausentes (NA)
    metric = "Accuracy",                  # Métrica para avaliar o modelo (Acurácia)
    num.trees = ntree                     # Número de árvores (ntree)
  )
  
  return(rf_model)
})

# Nomear os modelos treinados de acordo com o número de árvores
names(modelos_rf) <- paste0("RF_", num_arvores, "_arvores")

# Exibir o resumo do melhor modelo (como exemplo)
best_model <- modelos_rf[[which.max(sapply(modelos_rf, function(x) max(x$results$Accuracy)))]]  # Modelo com a melhor acurácia
print(best_model)

save.image("rf_0910.Rdata")

# XGBoost -----------------------------------------------------------------
set.seed(362)

tune_length_xgb <- 4

ctrl_xgb <- trainControl(
  method = "cv",
  number = 4,
  classProbs = TRUE,
  summaryFunction = multiClassSummary,
  returnData = TRUE,
  savePredictions = "final",
  allowParallel = TRUE,
  verboseIter  = TRUE,
  search = "random"
)

grid_xgb <- expand.grid(
  nrounds = sample(c( 100), tune_length_xgb, replace = TRUE),
  max_depth = sample(3:4, tune_length_xgb, replace = TRUE),
  eta = runif(tune_length_xgb, min = 0.05, max = 0.1),
  gamma = runif(tune_length_xgb, min = 0, max = 2),
  colsample_bytree = runif(tune_length_xgb, min = 0.6, max = 0.8),
  min_child_weight = sample(1:4, tune_length_xgb, replace = TRUE),
  subsample = runif(tune_length_xgb, min = 0.5, max = 0.8)        
)

cl <- makeCluster(detectCores() - 1) 

registerDoParallel(cl)

clusterExport(cl, c("dados_treinamento", "dados_teste", "ctrl_xgb", "grid_xgb"))

tic("Treinamento do Modelo XGBoost")

xgb_model_random <- train(
  status_doenca_final_trat ~ .,                    # Fórmula do modelo (variável alvo ~ variáveis preditoras)
  data = dados_treinamento,
  method = "xgbTree",
  trControl = ctrl_xgb,
  tuneGrid = grid_xgb,
  metric = "Accuracy",
  na.action = na.omit,
  verbosity = 0
  # early_stopping_rounds = 10
)

toc()

print(xgb_model_random)

# Finalizar e desregistrar o cluster paralelo
stopCluster(cl)
registerDoSEQ()



rm(cl)
rm(list = ls(pattern = "^cl"))
gc()

save.image("modelos.Rdata")


# Comparação entre folds --------------------------------------------------
rf_results <- best_model$resample %>%
  summarise(
    MeanAccuracy = mean(Accuracy),
    SDAccuracy = sd(Accuracy)
  )

xgb_results <- xgb_model_random$resample %>%
  summarise(
    MeanAccuracy = mean(Accuracy),
    SDAccuracy = sd(Accuracy)
  )

results_df <- data.frame(
  Model = c("Random Forest", "XGBoost"),
  MeanAccuracy = c(rf_results$MeanAccuracy, xgb_results$MeanAccuracy),
  SDAccuracy = c(rf_results$SDAccuracy, xgb_results$SDAccuracy)
)

