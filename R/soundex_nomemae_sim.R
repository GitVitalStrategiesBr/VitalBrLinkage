#' Aplica SoundexBR na coluna de NOMEMAE do SIM
#'
#' @param df Nome do data frame
#'
#' @return Retorna colunas com os nomes da mãe transformado em sons para o computador
#' @export
soundex_nomemae_sim <- function(df){

  ## Separar componentes do NOMEMAE
  df$NOMEMAE1 = stringi::stri_extract_first_words(df$NOMEMAE)
  df$NOMEMAE2 = sub(".+? ", "", df$NOMEMAE)
  df$NOMEMAE2 = sub("\\s*\\w*$", "", df$NOMEMAE2)
  df$NOMEMAE3 = stringi::stri_extract_last_words(df$NOMEMAE)

  df <- df |>
    dplyr::relocate(
      c(NOMEMAE1,NOMEMAE2,NOMEMAE3),
      .after = NOMEMAE
    )

  df$NOMEMAE1_SOUND = SoundexBR::soundexBR(df$NOMEMAE1, BR=TRUE, useBytes = FALSE)
  df$NOMEMAE2_SOUND = SoundexBR::soundexBR(df$NOMEMAE2, BR=TRUE, useBytes = FALSE)
  df$NOMEMAE3_SOUND = SoundexBR::soundexBR(df$NOMEMAE3, BR=TRUE, useBytes = FALSE)

  df <- df  |>
    dplyr::relocate(
      c(NOMEMAE1_SOUND,NOMEMAE2_SOUND,NOMEMAE3_SOUND),
      .after = NOMEMAE3
    ) |>
    dplyr::mutate(
      NOMEMAE_SOUND=paste0(NOMEMAE1_SOUND,NOMEMAE2_SOUND,NOMEMAE3_SOUND)
    )

}
