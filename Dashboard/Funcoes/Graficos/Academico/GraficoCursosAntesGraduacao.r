# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
GraficoCursosAntesGraduacao <- function(cursos_antes_graduacaodf) {
  # ==============================================================================
  # BIBLIOTECAS
  # ==============================================================================
  library(ggplot2)
  library(treemapify)
  # ==============================================================================
  # GRÁFICO DE TREEMAP - CURSOS FEITOS ANTES DA GRADUAÇÃO
  # ==============================================================================
  ggplot(
    cursos_antes_graduacaodf,
    aes(area = Quantidade, fill = Curso, label = paste0(Curso, " (", Quantidade, ")"))
  ) +
    geom_treemap(start = "topleft", layout = "srow") +
    geom_treemap_text(colour = "black", size = 30, place = "centre", start = "topleft", layout = "srow") +
    theme(legend.position = "none") +
    scale_fill_manual(values = c(
      "plum1", "lightsteelblue1", "light blue", "paleturquoise1", "orchid1", "peachpuff1", "rosybrown1", "lightpink2", "olivedrab1",
      "lightblue2", "lightsteelblue2", "orchid3"
    ))
}
