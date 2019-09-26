# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
ConsultaAreasFormacaoEmprego <- function(egressos, sigla_curso) {
  # ==============================================================================
  # CONSULTA - ÁREA DO CURSO CONCLUIDO NA GRADUAÇÃO
  # ==============================================================================
  area_graduacao <- estudos %>%
    select(Nome, Area) %>%
    filter(estudos$Escola == "IFSP", estudos$Curso == sigla_curso)

  if (identical(egressos, EgressosADS())) {
    area_graduacao <- area_graduacao %>%
      add_row(Nome = "FATIMA LOURENCO", Area = "INFORMACAO")
  }
  if (identical(egressos, EgressosGPI())) {
    area_graduacao <- area_graduacao %>%
      add_row(Nome = "ANDERSON NUNES DE OLIVEIRA JUNIOR", Area = "PROCESSOS INDUSTRIAIS") %>%
      add_row(Nome = "ANDERSON PINHEIRO CESARIO", Area = "PROCESSOS INDUSTRIAIS") %>%
      add_row(Nome = "PAULO ROBERTO POLLI", Area = "PROCESSOS INDUSTRIAIS")
  }
  # ==============================================================================
  # CONSULTA - ÁREA DO ATUAL TRABALHO
  # ==============================================================================
  area_emprego <- empregos %>%
    select(Nome, AreaCargo) %>%
    group_by(Nome) %>%
    filter(row_number() == 1) %>%
    filter(Nome %in% c(egressos$Nome))

  # ==============================================================================
  # CONSULTA - SE TRABALHA NA AREA DE FORMAÇÃO DA GRADUAÇÃO
  # ==============================================================================
  trabalha_na_area <- left_join(area_graduacao, area_emprego, by = "Nome") %>%
    mutate(
      Trabalha = str_detect(Area, AreaCargo),
      Trabalha = str_replace_all(Trabalha, "FALSE", "NÃO"),
      Trabalha = str_replace_all(Trabalha, "TRUE", "SIM")
    )
  # GARANTE QUE O EGRESSO TRABALHE EM UM ÚNICO LOCAL
  trabalha_na_area <- unique(trabalha_na_area) %>% na.omit(trabalha_na_area)
  return(trabalha_na_area)
}
