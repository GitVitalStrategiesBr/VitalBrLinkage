#' Croluna da idade em anos
#'
#' @param df Nome do data frame
#'
#' @return Retorna a coluna nu_idade_anos
#' @export
nu_idade_anos_sim <- function(df){

  df <- df  |>
    dplyr::mutate(
      cd_idade=as.numeric(cd_idade),
      nu_idade_anos=dplyr::case_when(
        cd_idade<401~0,
        cd_idade>400&cd_idade<600~cd_idade-400,
        cd_idade==999~NA)
      )

}
