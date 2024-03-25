#' Número de dias internado
#'
#' @param df nome do data frame
#'
#' @return Retorna uma coluna "dias_int" com número de dias que a pessoa fico internada (dt_saida - dt_internacao)
#' @export
dias_int_sih <- function(df){

  df <- df |>
    dplyr::mutate(
      dias_int=ymd(dt_saida)-ymd(dt_internacao)
      )

}
