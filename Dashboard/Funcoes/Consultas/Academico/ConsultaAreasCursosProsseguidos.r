# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
ConsultaAreasCursosProsseguidos <- function(egressos, flag) {
  # ==============================================================================
  # CONSULTA - CURSOS REALIZADOS PELOS EGRESSOS DEPOIS DA GRADUAÇÃO
  # ==============================================================================
  estudos_apos_egresso <- estudos %>%
    select(Nome, Escola, Curso, Area, flag) %>%
    na.omit(estudos_apos_egresso) %>%
    group_by(Nome) %>%
    filter(Nome %in% c(egressos$Nome)) %>%
    distinct(Escola, Curso, .keep_all = TRUE) %>%
    mutate(
      Flag = cumsum(get(flag))
    ) %>%
    filter(Flag != 1)
  # ==============================================================================
  # CONSULTA - QUANTIDADE DE PESSOAS POR TIPO DE CURSO APÓS A GRADUAÇÃO
  # ==============================================================================
  quantidade_pessoas_area <- estudos_apos_egresso %>%
    mutate(
      Area = str_squish(Area)
    ) %>%
    group_by(Area) %>%
    summarise(Quantidade = n()) %>%
    arrange(desc(Quantidade))
}
