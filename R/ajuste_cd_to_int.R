#' Ajusta colunas que começam com cd_ para números inteiros
#'
#' @param df Nome do data frame
#'
#' @return Retorna as colunas que começam com "cd_" convertidas para números inteiros
#' @export
ajuste_cd_to_int <- function(df) {
  colunas_cd <- grep("^cd_", names(df), value = TRUE)

  for (coluna in colunas_cd) {
    df[[coluna]] <- as.integer(df[[coluna]])
  }

  return(df)
}
