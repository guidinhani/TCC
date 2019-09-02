ConsultaAnoDeEgresso <- function(egressos, sigla_curso) {
  # ==============================================================================
  # CONSULTA - ANOS DE EGRESSO NO IFSP (CURSO DA GRADUAÇÃO)
  # ==============================================================================
  anos_egresso_IFSP <- estudos %>%
    filter(Escola == "IFSP") %>%
    filter(Curso == sigla_curso) %>%
    filter(Nome %in% c(egressos$Nome)) %>%
    distinct(Nome, .keep_all = TRUE) %>%
    group_by(AnoEgresso) %>%
    summarise(Quantidade = n())
}
