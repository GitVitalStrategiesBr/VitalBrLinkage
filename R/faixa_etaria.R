#' Categoriza a idade em grupos específicos
#'
#' Esta função categoriza a idade em grupos específicos.
#'
#' @param df Um data frame contendo a coluna 'nu_idade_anos' representando idades em anos.
#'
#' @return O data frame de entrada com uma coluna adicional 'faixa_etaria' representando os grupos etários.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(nu_idade_anos = c(4, 15, 27, 35, 50, 65, 80))
#' resultado <- faixa_etaria(df)
#' }
#'
#' @export
faixa_etaria <- function(df){
  df <- df |>
    dplyr::mutate(faixa_etaria=dplyr::case_when(
      nu_idade_anos < 5 ~ "<5",
      nu_idade_anos >= 5 & nu_idade_anos <= 9 ~ "05-09",
      nu_idade_anos >= 10 & nu_idade_anos <= 19 ~ "10-19",
      nu_idade_anos >= 20 & nu_idade_anos <= 29 ~ "20-29",
      nu_idade_anos >= 30 & nu_idade_anos <= 39 ~ "30-39",
      nu_idade_anos >= 40 & nu_idade_anos <= 49 ~ "40-49",
      nu_idade_anos >= 50 & nu_idade_anos <= 59 ~ "50-59",
      nu_idade_anos >= 60 & nu_idade_anos <= 69 ~ "60-69",
      nu_idade_anos >= 70 & nu_idade_anos <= 79 ~ "70-79",
      nu_idade_anos >= 80 ~ "80+",
      T ~ as.character(nu_idade_anos)
    )
    )
}
