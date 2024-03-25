#' Variáveis principais do SIM
#'
#' @param df nome do Data frame
#'
#' @return Data frame reduzido, apenas com as colunas necessárias
#' @export
variaveis_principais_sim <- function(df){

  df <- df |>
    dplyr::select(
      NUMERODO,
      NUMERODV,
      DTOBITO,
      NUMSUS,
      NATURAL,
      CODMUNNATU,
      NOME,
      NOMEPAI,
      NOMEMAE,
      DTNASC,
      IDADE,
      SEXO,
      RACACOR,
      CODMUNRES,
      BAIRES,
      ENDRES,
      NUMRES,
      COMPLRES,
      CEPRES,
      LOCOCOR,
      CODESTAB,
      CODESTOCOR,
      IDADEMAE,
      NUMERODN,
      LINHAA,
      LINHAB,
      LINHAC,
      LINHAD,
      LINHAII,
      CAUSABAS,
      ATESTANTE,
      CODMUNOCOR,
      ANO,
      GRAVIDEZ,
      GESTACAO,
      OBITOPARTO,
      OBITOGRAV,
      OBITOPUERP
    )

}
