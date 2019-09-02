# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
GraficoTipoEmpresa <- function(tipo_empresadf) {
  # ==============================================================================
  # GRÁFICO DE BARRAS - QUANTIDADE DE EGRESSOS EM EMPRESAS PÚBLICAS E PRIVADAS
  # ==============================================================================
  plot_ly(tipo_empresadf, x = ~Tipo, source = "sourceTipoEmpresa") %>%
    add_trace(
      y = ~Quantidade, type = "bar",
      textposition = "inside",
      hoverinfo = "text",
      textfont = list(color = "#FFFFFF", size = 14),
      text = ~ paste(Quantidade, "egresso(s)"),
      marker = list(
        color = c("rgb(163, 21, 16)", "rgb(61, 124, 54)"),
        line = list(color = c("rgb(99, 9, 9)", "rgb(32, 71, 28)"), width = 2)
      ),
      width = .5
    ) %>%
    layout(yaxis = list(title = "Quantidade de egressos"), xaxis = list(title = ""))
}
