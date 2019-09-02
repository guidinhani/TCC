# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
# ==============================================================================
# CONSULTA - QUANTIDADE DE EGRESSOS COM DADOS PROFISSIONAIS
# ==============================================================================
QuantidadeEgressosProfissional <- function(egressos) {
  quantidade_egressos_profissional <- empregos %>%
    select(Nome) %>%
    filter(Nome %in% c(egressos$Nome)) %>%
    distinct(Nome, .keep_all = TRUE) %>%
    summarise(Quantidade = n())
}
