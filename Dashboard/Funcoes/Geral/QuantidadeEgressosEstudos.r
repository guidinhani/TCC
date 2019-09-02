# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
# ==============================================================================
# CONSULTA - QUANTIDADE DE EGRESSOS COM DADOS ACADÊMICOS
# ==============================================================================
QuantidadeEgressosEstudos <- function(flag) {
  quantidade_egressos_estudos <- estudos %>%
    select(Nome, ehADS, ehGPI) %>%
    dplyr::filter(get(flag) == TRUE) %>%
    distinct(Nome, .keep_all = TRUE) %>%
    summarise(Quantidade = n())
}
