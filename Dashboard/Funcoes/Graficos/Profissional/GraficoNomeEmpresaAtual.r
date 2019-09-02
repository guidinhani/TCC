# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
GraficoNomeEmpresaAtual <- function(nome_empresa_atualdf) {
  # ==============================================================================
  # EVENTO DE CLICK
  # ==============================================================================
  eventdata <- event_data("plotly_click", source = "sourceTipoEmpresa")
  # ==============================================================================
  # VALIDAÇAO DO EVENTO DE CLICK
  # ==============================================================================
  validate(need(!is.null(eventdata), "Clique no gráfico de barras ao lado para saber mais informações"))
  # ==============================================================================
  # DEFINIÇÃO DE CORES
  # ==============================================================================
  if (eventdata$x == "PRIVADA") {
    colors <- "rgb(163, 21, 16)"
    fillColors <- "rgb(99, 9, 9)"
  }
  if (eventdata$x == "PUBLICA") {
    colors <- "rgb(61, 124, 54)"
    fillColors <- "rgb(32, 71, 28)"
  }
  # ==============================================================================
  # CONSULTA - TIPO DA EMPRESA ATUAL DE TRABALHO DOS EGRESSOS DE ACORDO COM A BARRA SELECIONADA
  # ==============================================================================
  empresa_atual <- nome_empresa_atualdf %>%
    group_by(Empresa) %>%
    filter(Tipo %in% eventdata$x) %>%
    summarise(Quantidade = n())
  # ==============================================================================
  # VALIDAÇAO DO EVENTO DE CLICK
  # ==============================================================================
  validate(need(!nrow(empresa_atual) == 0, "Clique no gráfico de barras ao lado para saber mais informações"))
  # ==============================================================================
  # GRÁFICO DE COLUNAS - NOME DAS EMPRESAS QUE OS EGRESSOS TRABALHAM ATUALMENTE
  # ==============================================================================
  plot_ly(empresa_atual, x = ~Quantidade) %>%
    add_trace(
      y = ~ reorder(Empresa, -Quantidade), type = "bar", orientation = "h",
      hoverinfo = "text", text = ~ paste(Quantidade, "egresso(s)"),
      marker = list(color = colors, line = list(color = fillColors, width = 2)),
      width = .5
    ) %>%
    layout(
      yaxis = list(title = "", tickmode = "linear"),
      xaxis = list(title = "Quantidade de egressos"), margin = list(l = 200)
    )
}
