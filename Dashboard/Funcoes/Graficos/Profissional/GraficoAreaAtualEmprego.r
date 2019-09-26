# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
GraficoAreaAtualEmprego <- function(area_atual_empregodf) {
  # ==============================================================================
  # MODIFICAÇÃO NA CONSULTA PARA O GRÁFICO
  # ==============================================================================
  mesmo_cargo <- area_atual_empregodf %>%
    select(Area, AreaCargo) %>%
    mutate(
      AreaCor = if_else(Area == AreaCargo, "#236583", "#a31510"),
      AreaOutline = if_else(Area == AreaCargo, "#0d3c51", "#630909"),
      AreaCargo = str_replace_all(AreaCargo, "ADMINISTRACAO", "ADM"),
      AreaCargo = str_replace_all(AreaCargo, "GESTAO DE EMPRESAS", "GEST. EMPRESAS"),
      AreaCargo = str_replace_all(AreaCargo, "GESTAO DE PESSOAS", "GEST. PESSOAS"),
      AreaCargo = str_replace_all(AreaCargo, "MARKETING", "MARKETING"),
      AreaCargo = str_replace_all(AreaCargo, "PROCESSOS INDUSTRIAIS", "PROC. IND."),
      AreaCargo = str_replace_all(AreaCargo, "RECURSOS NATURAIS", "R. NATURAIS")
    ) %>%
    group_by(AreaCargo, AreaCor, AreaOutline) %>%
    summarise(
      Quantidade = n()
    )
  
  # ==============================================================================
  # GRÁFICO DE BARRAS - AREAS DOS EMPREGOS DOS EGRESSOS DO ATUAL TRABALHO
  # ==============================================================================
  grafico <- ggplot(mesmo_cargo, aes(x = reorder(AreaCargo, -Quantidade), y = Quantidade, fill = AreaCor, color = AreaOutline, text = paste(Quantidade, "egresso(s)"))) +
    geom_col() +
    scale_fill_identity() +
    scale_color_identity() +
    xlab("") +
    ylab("Quantidade de egressos") +
    theme_minimal(base_size = 11, base_family = "Roboto") +
    theme(axis.text = element_text(size = 10, angle = 45), legend.position = "none", panel.grid.major.x = element_blank())

  ggplotly(grafico, tooltip = "text")
}
