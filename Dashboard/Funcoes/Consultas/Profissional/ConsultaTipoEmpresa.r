# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
ConsultaTipoEmpresa <- function(egressos) {
  # ==============================================================================
  # CONSULTA - TIPO DA EMPRESA ATUAL DE TRABALHO DOS EGRESSOS [PÚBLICA OU PRIVADA]
  # ==============================================================================
  empresa_trabalho <- empregos %>%
    select(Nome, Empresa, ehPublica) %>%
    group_by(Nome) %>%
    filter(Nome %in% c(egressos$Nome)) %>%
    filter(row_number() == 1) %>%
    mutate(
      ehPublica = str_replace_all(ehPublica, "FALSE", "PRIVADA"),
      ehPublica = str_replace_all(ehPublica, "TRUE", "PUBLICA")
    )

  names(empresa_trabalho)[3] <- "Tipo"
  empresa_trabalho[is.na(empresa_trabalho)] <- "SEM DADOS"
  # ==============================================================================
  # CONSULTA - QUANTIDADE DE EMPRESAS POR TIPO
  # ==============================================================================
  tipo_da_empresa <- empresa_trabalho %>%
    select(Tipo) %>%
    group_by(Tipo) %>%
    summarise(Quantidade = n())
}
