# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
GraficoTrabalhaAreaFormacao <- function(trabalha_area_formacaodf) {
  # ==============================================================================
  # CONSULTA - QUANTIDADE DE EGRESSOS QUE TRABALHAM NA AREA DE FORMAÇÃO
  # ==============================================================================
  quantidade_trabalha_na_area <- trabalha_area_formacaodf %>%
    select(Trabalha) %>%
    group_by(Trabalha) %>%
    summarise(Quantidade = n())

  TITULO <- c("TRABALHA NA ÁREA DE FORMAÇÃO?")
  SIM <- quantidade_trabalha_na_area[, 2][2, ]
  NAO <- quantidade_trabalha_na_area[, 2][1, ]
  quantidade_trabalha_na_area <- data.frame(TITULO, SIM, NAO)
  names(quantidade_trabalha_na_area) <- c("TITULO", "SIM", "NAO")

  # ==============================================================================
  # GRÁFICO DE BARRAS EMPILHADO
  # ==============================================================================
  plot_ly(quantidade_trabalha_na_area,
    x = ~TITULO, source = "sourceTrabalhaNaArea"
  ) %>%
    add_trace(
      type = "bar",
      y = ~NAO, name = "NAO", hoverinfo = "text", text = ~ paste(NAO, "egresso(s)"),
      textfont = list(color = "#FFFFFF", size = 14),
      textposition = "inside",
      marker = list(
        color = c("rgb(163, 21, 16)"),
        line = list(color = c("rgb(99, 9, 9)"), width = 2)
      ),
      width = .5
    ) %>%
    add_trace(
      type = "bar",
      y = ~SIM, name = "SIM", hoverinfo = "text", text = ~ paste(SIM, "egresso(s)"),
      textfont = list(color = "#FFFFFF", size = 14),
      textposition = "inside",
      marker = list(
        color = c("rgb(35, 101, 131)"), line = list(color = c("rgb(13, 60, 81)"), width = 2)
      ),
      width = .5
    ) %>%
    layout(yaxis = list(title = "Quantidade de egressos"), xaxis = list(title = ""), showlegend = T, barmode = "stack")
}
