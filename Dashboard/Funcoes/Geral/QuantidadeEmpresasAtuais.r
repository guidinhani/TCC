# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
# ==============================================================================
# CONSULTA - QUANTIDADE DE EMPRESAS DIFERENTES QUE EMPREGAM OS EGRESSOS
# ==============================================================================
QuantidadeEmpresasAtuais <- function(egressos) {
    quantidade_empresas_atuais <- empregos %>%
      select(Nome, Empresa) %>%
      filter(Nome %in% c(egressos$Nome)) %>% 
      group_by(Nome) %>%
      filter(row_number() == 1) %>% 
      group_by(Empresa) %>%
      summarise(Quantidade = n())
    
      return(dim(quantidade_empresas_atuais)[1])
}
