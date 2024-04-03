#' Croluna da idade em anos
#'
#' @param df Nome do data frame
#'
#' @return Retorna a coluna nu_idade_anos
#' @export
nu_idade_anos_sih <- function(df){

  df <- df |>
    dplyr::mutate(nu_idade_anos = as.integer(cd_idade))

}
