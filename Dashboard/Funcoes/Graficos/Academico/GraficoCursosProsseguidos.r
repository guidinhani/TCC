# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
GraficoCursosProsseguidos <- function(cursos_prosseguidosdf) {
  # ==============================================================================
  # GRÁFICO DE BARRAS - QUAIS OS CURSOS PROSSEGUIDOS PARA QUEM CONTINUOU
  # ==============================================================================
  plot_ly(cursos_prosseguidosdf, x = ~Quantidade) %>%
    add_trace(
      y = ~ reorder(Curso, -Quantidade), type = "bar", orientation = "h",
      hoverinfo = "text", text = ~ paste(Quantidade, "egresso(s)"),
      textfont = list(color = "#FFFFFF", size = 14),
      marker = list(color = "rgb(35, 101, 131)", line = list(color = "rgb(13, 60, 81)", width = 2)),
      width = .5
    ) %>%
    layout(
      yaxis = list(title = ""),
      xaxis = list(title = "Quantidade de egressos", autotick = F, range = c(0, 8)), margin = list(l = 240)
    )
}
