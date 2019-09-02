# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
ConsultaProsseguiuEstudos <- function(egressos, flag) {
  # ==============================================================================
  # CONSULTA - QUAIS EGRESSOS PROSSEGUIRAM COM OS ESTUDOS NA AREA DA GRADUAÇÃO
  # ==============================================================================
  prosseguiu_estudos <- estudos %>%
    select(Nome, Curso, flag) %>%
    group_by(Nome) %>%
    filter(Nome %in% c(egressos$Nome)) %>%
    filter(row_number() == 1) %>%
    mutate(
      flag = str_replace_all(flag, "FALSE", "SIM"),
      flag = str_replace_all(flag, "TRUE", "NAO")
    )

  # AJUSTE NO DATAFRAME DA CONSULTA ACIMA PARA O GRÁFICO
  names(prosseguiu_estudos)[3] <- "Continuou"
  prosseguiu_estudos <- na.omit(prosseguiu_estudos)

  # ==============================================================================
  # CONSULTA - QUANTIDADE DE EGRESSOS QUE CONTINUARAM A ESTUDAR DEPOIS DA FORMAÇÃO
  # ==============================================================================
  quantidade_prosseguiu_estudos <- prosseguiu_estudos %>%
    group_by(Continuou) %>%
    summarise(Quantidade = n())

  # AJUSTE NO DATAFRAME DA CONSULTA ACIMA PARA O GRÁFICO
  Titulo <- c("FEZ ALGUM CURSO DIFERENTE APÓS A GRADUAÇÃO?")
  Sim <- quantidade_prosseguiu_estudos[, 2][2, ]
  Nao <- quantidade_prosseguiu_estudos[, 2][1, ]
  quantidade_prosseguiu_estudos <- data.frame(Titulo, Sim, Nao)
  names(quantidade_prosseguiu_estudos) <- c("Titulo", "Sim", "Nao")
  return(quantidade_prosseguiu_estudos)
}
