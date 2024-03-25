#' Ajusta colunas do tipo character que contêm "_doc" para números inteiros.
#'
#' Esta função realiza ajustes em um data frame, transformando as colunas do tipo character
#' que contêm "_doc" em seus nomes para números inteiros. Os ajustes incluem a remoção de
#' espaços extras, a transcrição de caracteres acentuados para ASCII e a remoção de caracteres
#' não numéricos.
#'
#' @param df data frame a ser ajustado.
#'
#' @return Um data frame modificado, onde as colunas do tipo character contendo "_doc" em seus
#' nomes foram transformadas em números inteiros, mantendo apenas os valores numéricos presentes.
#' @export
ajuste_doc <- function(df){

  # Remove mais de um espaço e ajusta nas colunas de texto
  for (coluna in names(df)) {
    if (is.character(df[[coluna]])) {
      # Verifica se o nome da coluna contém "nome" ou "NOME"
      if (grepl("_doc", coluna, ignore.case = TRUE)) {
        # Translitera caracteres acentuados
        df[[coluna]] <- iconv(df[[coluna]], "UTF-8", "ASCII")
        df[[coluna]] <- toupper(df[[coluna]])

        # Remove espaços extras
        df[[coluna]] <- gsub("\\s{2,}", " ", df[[coluna]])

        # garante apenas valores numéricos
        df[[coluna]] <- gsub("[^0-9]", "", df[[coluna]])

        # Converte para número inteiro
        df[[coluna]] <- as.integer(df[[coluna]])
      }
    }
  }

  return(df)
}
