#' Corrige todas as colunas de data
#'
#' @param df nome do dataframe
#'
#' @return Transforma todas as colunas que comecem com "dt_" no formato data YYYY-MM-DD
#' @export
corrige_datas <- function(df){

  # Identificando colunas que começam com "dt_"
  colunas_dt <- grep("^dt_", names(df))

  # Transformando as colunas em formato de data
  df[, colunas_dt] <- lapply(df[, colunas_dt], as.Date)

  return(df)
}
