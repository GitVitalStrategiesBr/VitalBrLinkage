#' Ajusta as colunas que começam com "dt_" no formato YYYY-MM-DD
#'
#' @param df Nome do data frame
#'
#' @return Retorna as acolunas que começam com "dt_" corrigidas no formato de data, desde que estejam no formato YYYY-MM-DD.
#' Exemplo: Se estiver 2010-11-28 retornará como 2010-11-28 no formato correto.
#' @export
ajuste_data_tipo2 <- function(df){

  colunas_dt <- grep("^dt_", names(df), value = TRUE)

  for (coluna in colunas_dt) {
    df[[coluna]] <- lubridate::ymd(df[[coluna]])
  }

  return(df)
}
