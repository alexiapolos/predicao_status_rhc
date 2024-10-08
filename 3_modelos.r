library(randomForest)
library(caret)
library(dplyr)
library(openxlsx)
library(MLmetrics)
library(tictoc)
library(xgboost)
library(doParallel)

cl <- makeCluster(detectCores() - 1) 

registerDoParallel(cl)

dados_treinamento =
  dados_treinamento %>%
  dplyr::select(-ID)

dados_teste =
  dados_teste %>%
  dplyr::select(-ID)

num_variaveis <- 11

# Random Forest -----------------------------------------------------------
set.seed(352)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = multiClassSummary,
  sampling = "up",
  returnData = TRUE,
  savePredictions = TRUE,
  allowParallel = TRUE,
  verbose = FALSE,
  search = "random"                   
)

tune_length <- 9

if (tune_length > num_variaveis - 1) {
  tune_length <- num_variaveis - 1 
}

rf_random_grid <- expand.grid(
  mtry = sample(2:(num_variaveis - 1), tune_length, replace = FALSE),
  splitrule = c("gini", "extratrees"),
  min.node.size = sample(1:10, tune_length, replace = TRUE)            
)
clusterExport(cl, c("dados_treinamento", "dados_teste", "ctrl", "rf_random_grid"))

tic("Treinamento do Modelo Random Forest")

rf_model_random <- train(
  status_doenca ~ .,
  data = dados_treinamento,
  method = "ranger",
  trControl = ctrl,
  tuneLength = tune_length,
  tuneGrid = rf_random_grid,
  importance = "impurity",
  na.action = na.omit,
  metric = "Accuracy"            
)
toc()

save.image("rf_0510.Rdata")

# XGBoost -----------------------------------------------------------------
set.seed(362)

tune_length_xgb <- 4

ctrl_xgb <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = multiClassSummary,
  sampling = "up",
  returnData = TRUE,
  savePredictions = "final",
  allowParallel = TRUE,
  verboseIter  = TRUE,
  search = "random"
)

grid_xgb <- expand.grid(
  nrounds = sample(c(50, 100), tune_length_xgb, replace = TRUE),
  max_depth = sample(3:4, tune_length_xgb, replace = TRUE),
  eta = runif(tune_length_xgb, min = 0.05, max = 0.1),
  gamma = runif(tune_length_xgb, min = 0, max = 2),
  colsample_bytree = runif(tune_length_xgb, min = 0.6, max = 0.8),
  min_child_weight = sample(1:4, tune_length_xgb, replace = TRUE),
  subsample = runif(tune_length_xgb, min = 0.5, max = 0.8)        
)

clusterExport(cl, c("dados_treinamento", "dados_teste", "ctrl_xgb", "grid_xgb"))

tic("Treinamento do Modelo XGBoost")

xgb_model_random <- train(
  status_doenca ~ .,
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

save.image("modelos.Rdata")


rm(cl)
rm(list = ls(pattern = "^cl"))
gc()


# Comparação entre folds --------------------------------------------------
rf_results <- rf_model_random$resample %>%
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

