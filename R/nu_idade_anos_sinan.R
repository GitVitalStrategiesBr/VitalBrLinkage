#' Cria as idades do sinan
#'
#' @param df Nome do dataframe
#'
#' @return Retorna a coluna "nu_idade_anos" com as idades nos registros
#' @export
nu_idade_anos_sinan <- function(df){

  df <- df  |>
    dplyr::mutate(cd_idade=as.numeric(cd_idade),
           nu_idade_anos=dplyr::case_when(
             cd_idade<4001~0,
             cd_idade>4000&cd_idade<6000~cd_idade-4000,
             T~NA))

  return(df)

}
