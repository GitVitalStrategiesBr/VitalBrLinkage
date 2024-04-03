#' Copia Coluna de Nomes
#'
#' Esta função copia os nomes originais das variáveis para novas colunas específicas,
#' apenas se as variáveis originais existirem no conjunto de dados.
#'
#' @param df Um data frame contendo as variáveis a serem copiadas.
#'
#' @return Retorna o data frame com as novas colunas adicionadas.
#'
#' @export
copia_nomes <- function(df){

  # Verifica se a variável original existe antes de copiar
  if ("ds_nome_pac" %in% colnames(df)) {
    df$nome_original <- df$ds_nome_pac
  }

  if ("ds_nome_mae" %in% colnames(df)) {
    df$nome_mae_original <- df$ds_nome_mae
  }

  if ("ds_nome_pai" %in% colnames(df)) {
    df$nome_pai_original <- df$ds_nome_pai
  }

  return(df)
}
