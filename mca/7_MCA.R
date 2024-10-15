library(tidyverse)
library(FactoMineR) #MCA
library(factoextra)

dados_int =
  dados %>%
  dplyr::select(cacon_unacon,
                regiao,
                lei60_respeitou,
                status_doenca_final_trat)

#Análise de Correspondência Múltipla
MCA <- MCA(dados_int, graph = F)

#Fazer scree plot
scree_plot <- fviz_screeplot(MCA, addLabels = TRUE, ylim = c(0, 45))
ggsave(
  "scree_plot.png",
  plot = scree_plot,
  width = 10,
  height = 6,
  dpi = 600
)

#Gráfico das variáveis
var_plot <- fviz_mca_var(
  MCA,
  choice = "mca.cor",
  geom = c("point", "text"),
  repel = TRUE,
  title = "MCA - Variáveis",
  col.var = "black"
)
ggsave(
  "var_plot.png",
  plot = var_plot,
  width = 10,
  height = 6,
  dpi = 600
)

#Gráfico das categorias
cat_plot <- fviz_mca_var(
  MCA,
  col.var = "black",
  geom = c("point", "text"),
  title = "MCA - Categorias",
  shape.var = "1",
  repel = TRUE
)
ggsave(
  "cat_plot.png",
  plot = cat_plot,
  width = 10,
  height = 6,
  dpi = 600
)

#Gráfico de correspondência variaveis e individuos
biplot <- fviz_mca_biplot(
  MCA,
  geom.ind = "point",
  habillage = 4,
  addEllipses = TRUE,
  geom.var = "point",
  col.var = "black",
  title = "MCA - Correspondência",
  repel = TRUE
)
ggsave(
  "biplot.png",
  plot = biplot,
  width = 10,
  height = 6,
  dpi = 600
)

individuos <- fviz_mca_ind(
  MCA,
  geom.ind = c("point", "text"),
  habillage = 4,
  addEllipses = TRUE,
  title = "MCA - Individuos",
  repel = TRUE
)

ggsave(
  "indiv_plot.png",
  plot = individuos,
  width = 10,
  height = 6,
  dpi = 600
)



