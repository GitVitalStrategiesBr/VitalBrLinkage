#' Limpa textos sobre ignoados
#'
#' @param df Nome do data frame
#'
#' @return Retorna NA nos valores das colunas que contenham textos como "ignorados", etc
#' @export
limpa_ignorados_sim <- function(df){

  df <- df  |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~case_when(.%in%c(
          "IGNORADO",
          "NAO DECLARADO",
          "DESCONHECIDO",
          "NAO INFORMADO",
          "NA",
          "ND",
          "NAO CONSTA",
          "IGN",
          "NC",
          "XXXXX",
          "-----"
        )~NA_character_,
        T~.)))

}
