#' Transforma as variáveis em Character
#'
#' @param df Nome do data frame
#'
#' @return Retorna o data frame com as variáveis no formato character
#' @export
as_char <- function(df){

  data.frame(lapply(df, as.character), stringsAsFactors = FALSE)

}
