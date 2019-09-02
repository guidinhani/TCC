# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
ConsultaNomeEmpresaAtual <- function(egressos) {
  # ==============================================================================
  # CONSULTA - TIPO DA EMPRESA ATUAL DE TRABALHO DOS EGRESSOS
  # ==============================================================================
  empresa_trabalho <- empregos %>%
    select(Nome, Empresa, ehPublica) %>%
    group_by(Nome) %>%
    filter(row_number() == 1) %>%
    filter(Nome %in% c(egressos$Nome)) %>%
    mutate(
      ehPublica = str_replace_all(ehPublica, "FALSE", "PRIVADA"),
      ehPublica = str_replace_all(ehPublica, "TRUE", "PUBLICA")
    )
  
  # AJUSTE NO DATAFRAME
  names(empresa_trabalho)[3] <- "Tipo"
  empresa_trabalho[is.na(empresa_trabalho)] <- "SEM DADOS"
  return(empresa_trabalho)
}
