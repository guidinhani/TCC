# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
GraficoCargosAtuais <- function(cargos_atuaisdf) {
  # ==============================================================================
  # GRÁFICO DE COLUNAS - CARGOS QUE OS EGRESSOS TRABALHAM ATUALMENTE
  # ==============================================================================
  plot_ly(cargos_atuaisdf, x = ~Quantidade) %>%
    add_trace(
      y = ~ reorder(Cargo, -Quantidade), type = "bar", orientation = "h",
      hoverinfo = "text",
      text = ~ paste(Quantidade, "egresso(s)"),
      marker = list(
        color = "rgb(35, 101, 131)", line = list(color = "rgb(13, 60, 81)", width = 2),
        width = .5
      )
    ) %>%
    layout(
      yaxis = list(title = "", tickmode = "linear"),
      xaxis = list(title = "Quantidade de egressos", autotick = F, range = c(0, 5)), margin = list(l = 300)
    )
}
