#' Ajusta colunas que começam com dt_ no formato YYYYMMDD
#'
#' @param df Nome do data frame
#'
#' @return Retorna as acolunas que começam com "dt_" corrigidas, desde que estejam no formato YYYYMMDD.
#' Exemplo: Se estiver 20101128 retornará como 2010-11-28
#' @export
ajuste_data_tipo1 <- function(df) {
  colunas_dt <- grep("^dt_", names(df), value = TRUE)

  for (coluna in colunas_dt) {
    df[[coluna]] <- lubridate::dmy(df[[coluna]])
  }

  return(df)

}
