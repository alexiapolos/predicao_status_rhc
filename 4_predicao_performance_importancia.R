
# 1. Pacotes ----------------------------------------------------------------
library(caret)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(flextable)
library(officer)
library(tictoc)

# 2. Formatar dados ----------------------------------------------------------
dados_teste =
  dados_teste %>%
  dplyr::mutate(ID = row_number())

dados_teste_sem_status =
  dados_teste %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::select(-status_doenca_final_trat)

# 3. Desempenho do modelo ---------------------------------------------------

png(
  filename = "xgb_model_random.png",
  width = 4000,
  height = 3000,
  res = 300
)

plot(xgb_model_random)

dev.off()

# 4. Previsões --------------------------------------------------------------
tic()
previsoes_xgb_model_random <- predict(xgb_model_random, newdata = dados_teste_sem_status)
toc()
previsoes_xgb_model_random <- factor(previsoes_xgb_model_random, levels = levels(dados_teste$status_doenca))

dados_preditos_xgb_model_random <- dados_teste_sem_status %>%
  dplyr::mutate(predicoes = previsoes_xgb_model_random)

write.xlsx(dados_pred, "dados_preditos_xgb.xlsx")

# 5. Matriz de confusão ---------------------------------------------------
matriz_confusao_previsoes_xgb_model_random <- confusionMatrix(previsoes_xgb_model_random, dados_teste$status_doenca)

accuracy <- matriz_confusao_previsoes_xgb_model_random$overall['Accuracy']

metricas <- matriz_confusao_previsoes_xgb_model_random$byClass %>%
  as.data.frame() %>%
  mutate(Acuracia_Geral = accuracy)

write.xlsx(metricas,
           file = "metricas_xg.xlsx",
           rowNames = TRUE,
           colNames = TRUE)

matriz_confusao_tabela <- matriz_confusao_previsoes_xgb_model_random$table

matriz_confusao_df <- as.data.frame(matriz_confusao_tabela)

heatmap <- ggplot(matriz_confusao_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(alpha = 0.8) +
  scale_fill_gradient(low = "#cdd3e0", high = "#032263") +
  geom_text(aes(label = Freq), color = "black", size = 14) +  
  labs(title = "XGBoost",
       x = "Classe Referência",
       y = "Classe Predita",
       fill = "N") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid = element_blank()
  )

ggsave(
  filename = "mc_xg.png",
  plot = heatmap,
  width = 14,
  height = 8,
  dpi = 600
)

# 6. Importancia das variaveis ---------------------------------------------
importancia <- varImp(best_model, scale = TRUE)

importancia_df <- importancia$importance %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Variável") %>%
  arrange(desc(Overall))

tabela_importancia <- regulartable(importancia_df)

tabela_importancia <- tabela_importancia %>%
  theme_vanilla() %>%
  set_caption("Tabela de Importância das Variáveis -xg") %>%
  autofit()

doc <- read_docx() %>%
  body_add_flextable(tabela_importancia) %>%
  body_add_par("Tabela", style = "Normal")

print(doc, target = "importancia_variaveis_rf.docx")

importancia_filtrada_df <- importancia_df %>%
  arrange(Overall) %>% 
  slice_head(n = 10) 

importancia_plot <- ggplot(importancia_filtrada_df, aes(x = reorder(Variável, Overall), y = Overall)) +
  geom_bar(stat = "identity",
           fill = "#4e6d9a",
           width = 0.7) +
  geom_text(
    aes(label = round(Overall, 2)),
    hjust = -0.2,
    size = 5,
    color = "black",
    fontface = "bold"
  ) +
  coord_flip() +
  labs(title = "Random Forest", x = "Variáveis", y = "Importância") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray", linetype = "dotted"),
    panel.border = element_blank()
  )

ggsave(
  filename = "var_menos_import_rf.png",
  plot = importancia_plot,
  width = 10,
  height = 8,
  dpi = 600
)
