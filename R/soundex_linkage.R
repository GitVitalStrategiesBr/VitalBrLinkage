#' Generaliza a separação de componentes de um nome e aplica SoundexBR
#'
#' Esta função aceita um data frame e o nome de uma coluna contendo nomes completos.
#' Ela realiza a separação dos componentes do nome e aplica o algoritmo SoundexBR
#' para cada parte do nome, gerando novas colunas com os resultados.
#'
#' @param df Um data frame contendo a coluna de nomes.
#' @param col_name O nome da coluna que contém os nomes completos.
#'
#' @return O data frame de entrada com colunas adicionais para as partes do nome e os códigos SoundexBR.
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(NOME = c("John Doe", "Jane Smith"))
#'   resultado <- soundex_linkage(df, "NOME")
#' }
#'
#' @export
soundex_linkage <- function(df, col_name) {

  df[[col_name]] <- iconv(df[[col_name]], "UTF-8", "ASCII")

  # df[[col_name]] <- stringi::stri_trans_general(df[[col_name]], "latin-ascii")
  # Separar componentes do nome
  col_name1 <- paste0(col_name, "1")
  col_name2 <- paste0(col_name, "2")
  col_name3 <- paste0(col_name, "3")

  df[[col_name1]] <- stringi::stri_extract_first_words(df[[col_name]])
  df[[col_name2]] <- sub(".+? ", "", df[[col_name]])
  df[[col_name2]] <- sub("\\s*\\w*$", "", df[[col_name2]])
  df[[col_name3]] <- stringi::stri_extract_last_words(df[[col_name]])

  df <- df |>
    dplyr::relocate(c(col_name1, col_name2, col_name3), .after = col_name)

  # SOUNDEXBR
  col_name1_sound <- paste0(col_name, "1_sound")
  col_name2_sound <- paste0(col_name, "2_sound")
  col_name3_sound <- paste0(col_name, "3_sound")

  df[[col_name1_sound]] <- SoundexBR::soundexBR(df[[col_name1]], BR = TRUE, useBytes = FALSE)
  df[[col_name2_sound]] <- SoundexBR::soundexBR(df[[col_name2]], BR = TRUE, useBytes = FALSE)
  df[[col_name3_sound]] <- SoundexBR::soundexBR(df[[col_name3]], BR = TRUE, useBytes = FALSE)

  df <- df |>
    dplyr::relocate(c(col_name1_sound, col_name2_sound, col_name3_sound), .after = col_name3) |>
    dplyr::mutate(!!paste0(col_name, "_sound") := paste0(get(col_name1_sound), get(col_name2_sound), get(col_name3_sound)),
                  across(contains("_sound"), ~ ifelse(. == "NANANA", NA, .)))


  return(df)
}

