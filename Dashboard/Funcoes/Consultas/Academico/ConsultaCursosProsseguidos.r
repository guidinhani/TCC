# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
ConsultaCursosProsseguidos <- function(egressos, flag) {
  # ==============================================================================
  # CONSULTA - ÚLTIMOS CURSOS REALIZADOS PELOS EGRESSOS DEPOIS DA GRADUAÇÃO
  # ==============================================================================
  estudos_apos_graduacao <- estudos %>%
    select(Nome, Escola, Curso, Area, flag) %>%
    na.omit(estudos_apos_graduacao) %>%
    group_by(Nome) %>%
    filter(Nome %in% c(egressos$Nome)) %>%
    distinct(Escola, Curso, .keep_all = TRUE) %>%
    mutate(
      Flag = cumsum(get(flag)),
      # PADRONIZANDO OS CURSOS PARA O GRÁFICO
      Curso = str_replace_all(Curso, "^CURSO DE EXTENSAO.*", "CURSO DE EXTENSAO"),
      Curso = str_replace_all(Curso, ".*MBA.*", "MBA"),
      Curso = str_replace_all(Curso, ".*INGLES.*|.*ESPANHOL.*|.*FRANCES.*", "LINGUAS"),
      Curso = str_replace_all(Curso, ".*DOUTORADO.*", "DOUTORADO"),
      Curso = str_replace_all(Curso, ".*ESPECIALIZACAO.*", "PÓS GRADUAÇÃO/ESPECIALIZAÇÃO"),
      Curso = str_replace_all(Curso, ".*MESTRADO.*", "MESTRADO"),
      Curso = str_replace_all(Curso, ".*POS GRADUACAO.*", "PÓS GRADUAÇÃO/ESPECIALIZAÇÃO"),
      Curso = str_replace_all(Curso, ".*TECNICO.*", "CURSO TECNICO"),
      Curso = str_replace_all(Curso, ".*ADS.*|.*GRADUACAO.*|.*DE INFORMACAO.*|^ENGENHARIA.*", "OUTRA GRADUAÇÃO")
    ) %>%
    filter(Flag != 1)

  # ==============================================================================
  # CONSULTA - QUANTIDADE DE PESSOAS POR TIPO DE CURSO APÓS A GRADUAÇÃO
  # ==============================================================================
  quantidade_pessoas_curso <- estudos_apos_graduacao %>%
    mutate(
      Curso = str_squish(Curso)
    ) %>%
    group_by(Curso) %>%
    summarise(Quantidade = n()) %>%
    arrange(desc(Quantidade))
}
