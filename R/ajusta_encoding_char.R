#' Correção de encoding em colunas Character
#'
#' @param df Nome do data frame
#'
#' @return Retorna o encoding ajustado para todas as colunas no formato character no data frame
#' @export
ajusta_encoding_char <- function(df){

  # Obter nomes das colunas que são do tipo character
  colunas_texto <- names(df)[sapply(df, is.character)]

  # Iterar sobre as colunas de texto e aplicar a conversão para UTF-8 ASCII
  for (coluna in colunas_texto) {
    df[[coluna]] <- iconv(df[[coluna]], "UTF-8", "ASCII")
  }

  return(df)

}
