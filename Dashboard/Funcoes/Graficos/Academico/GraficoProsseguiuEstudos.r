# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
GraficoProsseguiuEstudos <- function(prosseguiu_estudosdf) {
  # ==============================================================================
  # GRÁFICO DE BARRAS EMPILHADO - SE O EGRESSO FEZ ALGUM CURSO APÓS A GRADUAÇÃO
  # ==============================================================================
  plot_ly(prosseguiu_estudosdf, x = ~Titulo, source = "source") %>%
    add_trace(
      type = "bar",
      y = ~Sim, name = "Nao",
      textposition = "inside",
      hoverinfo = "text",
      textfont = list(color = "#FFFFFF", size = 14),
      text = ~ paste(Sim, "egresso(s)"),
      marker = list(color = c("rgb(163, 21, 16)"), line = list(color = c("rgb(99, 9, 9)"), width = 2)),
      width = .4
    ) %>%
    add_trace(
      type = "bar",
      y = ~Nao, name = "Sim",
      textposition = "inside",
      hoverinfo = "text",
      textfont = list(color = "#FFFFFF", size = 14),
      text = ~ paste(Nao, "egresso(s)"),
      marker = list(color = c("rgb(35, 101, 131)"), line = list(color = c("rgb(13, 60, 81)"), width = 2)),
      width = .4
    ) %>%
    layout(
      yaxis = list(title = "Quantidade de egressos"), xaxis = list(title = ""), showlegend = T, barmode = "stack"
    )
}
