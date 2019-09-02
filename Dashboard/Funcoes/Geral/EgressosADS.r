# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
EgressosADS <- function() {
  # ==============================================================================
  # CONSULTA - NOME DOS EGRESSOS DE ADS
  # ==============================================================================
  egressos_ads <- estudos %>%
    select(Nome) %>%
    filter(estudos$ehADS == TRUE) %>%
    distinct(Nome, .keep_all = TRUE) %>%
    mutate(
      Nome = str_trim(Nome, side = c("both")),
      Nome = str_squish(Nome)
    )
}
