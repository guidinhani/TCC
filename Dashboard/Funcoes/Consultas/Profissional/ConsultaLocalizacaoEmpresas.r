# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
ConsultaLocalizacaoEmpresas <- function(egressos) {
  # ==============================================================================
  # BIBLIOTECAS
  # ==============================================================================
  library(gdata)
  # ==============================================================================
  # CONSULTA - LOCALIZAÇÃO DAS EMPRESAS DOS ATUAIS EMPREGOS DOS EGRESSOS
  # ==============================================================================
  local_empresas <- empregos %>%
    select(Nome, Cidade) %>%
    group_by(Nome) %>%
    filter(Nome %in% c(egressos$Nome)) %>%
    filter(row_number() == 1) %>%
    trim(local_empresas)
  # ==============================================================================
  # CONSULTA - QUANTIADE DE EGRESSOS POR LOCALIZAÇÃO
  # ==============================================================================
  quantidade_local_empresas <- local_empresas %>%
    group_by(Cidade) %>%
    summarise(Quantidade = n())
}
