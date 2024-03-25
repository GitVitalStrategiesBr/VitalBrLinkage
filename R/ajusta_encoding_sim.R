#' Ajusta o encoding de NOMEPAI, NOME e NOMEMAE do SIM
#'
#' @param df nome do data frame
#'
#' @return Retorna o encoding das colunas NOMEPAI, NOME e NOMEMAE do SIM corrigidos
#' com UTF-8, ASCII
#' @export
ajusta_encoding_sim <- function(df){

  df$NOMEPAI <- iconv(df$NOMEPAI, "UTF-8", "ASCII")
  df$NOME <- iconv(df$NOME, "UTF-8", "ASCII")
  df$NOMEMAE <- iconv(df$NOMEMAE, "UTF-8", "ASCII")
  df$ENDOCOR <- iconv(df$ENDOCOR, "UTF-8", "ASCII")
  df$BAIOCOR <- iconv(df$BAIOCOR, "UTF-8", "ASCII")

  return(df)

}
