# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
GraficoAnoDeEgresso <- function(anos_egressosdf) {
  # ==============================================================================
  # GRÁFICO DE COLUNAS - QUANTIDADE DE EGRESSOS POR ANO
  # ==============================================================================
  plot_ly(anos_egressosdf,
    x = ~AnoEgresso, y = ~Quantidade, type = "bar",
    hoverinfo = "text",
    text = ~ paste(Quantidade, "egresso(s)"),
    marker = list(color = "rgb(35, 101, 131)", line = list(color = "rgb(13, 60, 81)", width = 2))
  ) %>%
    layout(
      yaxis = list(title = "Quantidade de egressos"),
      xaxis = list(title = "Anos", autotick = F)
    )
}
