#' Corretor de sg_sexo no SINAN viol
#'
#' @param df Nome do dataframe
#'
#' @return Retorna a correção da coluna "sg_sexo" do sinan
#' @export
corrige_sg_sexo <- function(df){

  df <- df |>
    dplyr::mutate(sg_sexo=dplyr::case_when(
      sg_sexo=="I"~"SEM INFORMACAO",
      T~sg_sexo
    ))

}
