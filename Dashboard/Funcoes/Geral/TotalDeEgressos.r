# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
# ==============================================================================
# TOTAL DE EGRESSOS DE CADA CURSO BASEADO NOS DADOS PASSADOS PELA CSP DO IFSP
# ==============================================================================
TotalDeEgressos <- function(sigla_curso) {
  if (sigla_curso == "ADS") {
    total <- "67"
  }
  if (sigla_curso == "GPI") {
    total <- "84"
  }

  return(total)
}
