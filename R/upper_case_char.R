#' Upper nos textos das colunas de Character
#'
#' @param df Nome do data frame
#'
#' @return Todas as colunas que estiverem como Character retornarão com texto em caixa alta
#' @export
upper_case_char <- function(df) {
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(~ is.character(.x) && !any(grepl("\\d", .x))),
        toupper
      )
    )
}
