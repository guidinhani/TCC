# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
EgressosGPI <- function() {
  # ==============================================================================
  # CONSULTA - NOME DOS EGRESSOS DE GPI
  # ==============================================================================
  egressos_gpi <- estudos %>%
    select(Nome) %>%
    filter(estudos$ehGPI == TRUE) %>%
    distinct(Nome, .keep_all = TRUE)
}
