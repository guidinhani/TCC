# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
ConsultaCargosAtuais <- function(egressos) {
  # ==============================================================================
  # BIBLIOTECAS
  # ==============================================================================
  library(gdata)
  # ==============================================================================
  # CONSULTA - ATUAIS CARGOS DOS EGRESSOS NOS ATUAIS EMPREGOS
  # ==============================================================================
  cargos_atuais <- empregos %>%
    select(Nome, Cargo) %>%
    group_by(Nome) %>%
    filter(Nome %in% c(egressos$Nome)) %>%
    filter(row_number() == 1) %>%
    trim(localEmpresas)

  cargos_atuais[is.na(cargos_atuais)] <- "SEM DADOS"
  # ==============================================================================
  # CONSULTA - QUANTIDADE DE EGRESSOS POR CARGOS
  # ==============================================================================
  quantidade_cargos_atuais <- cargos_atuais %>%
    group_by(Cargo) %>%
    summarise(Quantidade = n())
}
