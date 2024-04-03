#' Cria a coluna ANO a aprtir de dt_internacao
#'
#' @param df Nome do data frame
#'
#' @return Retorna uma coluna nova chamada ANO
#' @export
ano_sih <- function(df) {
  df <- df |>
    dplyr::mutate(
      ano = as.character(data.table::year(ymd(dt_internacao))),
      ano_nasc = as.character(data.table::year(ymd(dt_nasc)))
    )
  return(df)
}
