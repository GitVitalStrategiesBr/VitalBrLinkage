#' Cria a coluna dt_nasc a aprtir da data de nascimento
#'
#' @param df Nome do data frame
#'
#' @return Retorna uma coluna nova chamada ANO
#' @export
ano_sinan <- function(df){

  df$ano_nasc <- format(df$dt_nasc, "%Y")

  return(df)

}
