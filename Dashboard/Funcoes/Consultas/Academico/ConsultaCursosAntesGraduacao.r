# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
ConsultaCursosAntesGraduacao <- function(egressos, flag, sigla_curso) {
  # ==============================================================================
  # CONSULTA - TODOS OS CURSOS FEITOS ANTES DA GRADUAÇÃO
  # ==============================================================================
  cursos_antes_graduacao <- estudos %>%
    select(Nome, Curso, flag) %>%
    na.omit(cursos_antes_graduacao) %>%
    group_by(Nome) %>%
    filter(Nome %in% c(egressos$Nome)) %>%
    distinct(Curso, .keep_all = TRUE) %>%
    mutate(
      Flag = cumsum(get(flag))
    ) %>%
    filter(Flag != 0) %>%
    filter(Curso != sigla_curso) %>%
    mutate(
      Curso = str_squish(Curso)
    ) %>%
    group_by(Curso) %>%
    summarise(Quantidade = n())
}
