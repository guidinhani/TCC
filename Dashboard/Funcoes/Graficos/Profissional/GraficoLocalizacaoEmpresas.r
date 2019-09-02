# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
GraficoLocalizacaoEmpresas <- function(localizacao_empresasdf) {
  # ==============================================================================
  # GRÁFICO DE BARRAS - CIDADE DAS EMPRESAS QUE OS EGRESSOS TRABALHAM ATUALMENTE
  # ==============================================================================
  plot_ly(localizacao_empresasdf, x = ~Quantidade) %>%
    add_trace(
      y = ~ reorder(Cidade, -Quantidade), type = "bar",
      hoverinfo = "text", text = ~ paste(Quantidade, "egresso(s)"),
      marker = list(color = "rgb(35, 101, 131)", line = list(color = "rgb(13, 60, 81)", width = 2)),
      width = .5
    ) %>%
    layout(
      yaxis = list(title = ""),
      xaxis = list(title = "Quantidade de egressos", autotick = F, range = c(0, 10)), margin = list(l = 160)
    )
}
