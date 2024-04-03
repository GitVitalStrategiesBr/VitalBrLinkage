#' Cria a coluna de idade do SIH, quando preciso
#'
#' @param df nome do Data frame
#'
#' @return Retorna a coluna cd_idade como número inteiro
#' @export
idade_sih <- function(df){
  df <- df |>
    dplyr::mutate(
      cd_idade=as.integer(cd_idade)
    )

}
