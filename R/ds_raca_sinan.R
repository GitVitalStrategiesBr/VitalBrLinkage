#' Decodifica raça/cor
#'
#' @param df Nome do dataframe do SINAN
#'
#' @return Nova coluna chamada "ds_raca" com a descrição da raça/cor
#' @export
ds_raca_sinan <- function(df){
  df <- df |>
    dplyr::mutate(ds_raca=dplyr::case_when(
      cd_raca=="1"~"BRANCA",
      cd_raca=="2"~"PRETA",
      cd_raca=="3"~"AMARELA",
      cd_raca=="4"~"PARDA",
      cd_raca=="5"~"INDIGENA",
      T~"IGNORADA"))

}
