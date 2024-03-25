#' Identifica e Marca Pacientes com Dados Gemelares
#'
#' Esta função percorre um dataframe e marca as linhas que contêm indicações
#' de dados gemelares na coluna especificada. Utiliza expressões regulares para
#' identificar várias indicações de gemelaridade, como "GEMELAR", "GEMEO", "GEMEA",
#' entre outros padrões. Uma nova coluna chamada 'gemelar' é criada (ou atualizada,
#' se já existir), onde cada linha correspondente a um paciente gemelar é marcada com 1.
#'
#' @param df Dataframe contendo os dados dos pacientes.
#' @param coluna Nome da coluna no dataframe que contém os nomes ou descrições dos pacientes a serem verificados.
#'
#' @return Dataframe original com uma coluna adicional 'gemelar', onde cada linha
#' que corresponde a um registro gemelar é marcada com 1, e 0 caso contrário.
#'
#' @export
gemelar <- function(df, coluna){
  # Inicializa a coluna 'gemelar' com 0 para todos os registros
  df$gemelar <- 0

  # Define as condições regex em um vetor
  padroes <- c("GEMELAR", " GEM ", "GEMEO", "GEMEA", " GEM1", " GEM2",
               "PRIMEIRO", "SEGUNDO G", "PRIM ", "SEG ", "GEMELAR","GEMELAR ",
               "GEMELAR I ","GEMELAR I", "GEMELAR II ","GEMELAR II","I GEMELAR","II GEMELAR")

  # Aplica cada padrão para atualizar a coluna 'gemelar'
  for (padrao in padroes) {
    df$gemelar <- ifelse(grepl(padrao, df[[coluna]]), 1, df$gemelar)
  }

  return(df)
}
