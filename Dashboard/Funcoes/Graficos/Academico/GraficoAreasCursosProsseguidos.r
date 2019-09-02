# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
GraficoAreasCursosProsseguidos <- function(areas_cursos_prosseguidosdf) {
  # ==============================================================================
  # GRÁFICO DE COLUNAS - AREAS DOS CURSOS PROSSEGUIDOS PARA QUEM CONTINUOU
  # ==============================================================================
  plot_ly(areas_cursos_prosseguidosdf, x = ~Quantidade) %>%
    add_trace(
      y = ~ reorder(Area, -Quantidade), type = "bar", orientation = "h",
      hoverinfo = "text", text = ~ paste(Quantidade, "curso(s)"),
      textfont = list(color = "#FFFFFF", size = 14),
      marker = list(color = "rgb(35, 101, 131)", line = list(color = "rgb(13, 60, 81)", width = 2)),
      width = .5
    ) %>%
    layout(
      yaxis = list(title = ""),
      xaxis = list(title = "Quantidade de cursos por área", autotick = F), margin = list(l = 190)
    )
}
