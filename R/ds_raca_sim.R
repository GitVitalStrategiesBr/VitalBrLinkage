#' Decodifica raça/cor do SIM
#'
#' @param df Nome do dataframe do SIM
#'
#' @return Nova coluna chamada "ds_raca" com a descrição da raça/cor
#' @export
ds_raca_sim <- function(df){

  df <- df |>
    dplyr::mutate(
      ds_raca=dplyr::case_when(
        ds_raca=="1"~"BRANCA",
        ds_raca=="2"~"PRETA",
        ds_raca=="3"~"AMARELA",
        ds_raca=="4"~"PARDA",
        ds_raca=="5"~"INDIGENA",
        T~"IGNORADA"
      )
    )

  }

