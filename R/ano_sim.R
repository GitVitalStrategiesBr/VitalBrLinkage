#' Cria a coluna ANO a aprtir de DTOBITO
#'
#' @param df Nome do data frame
#'
#' @return Retorna uma coluna nova chamada ANO
#' @export
ano_sim <- function(df){

  df$ano <- format(df$dt_obito, "%Y")

  df$ano_nasc <- format(df$dt_nasc, "%Y")

  return(df)

}
