#' Ajusta colunas de texto em um dataframe
#'
#' Esta função realiza ajustes específicos nas colunas de texto de um dataframe, incluindo
#' a remoção de espaços extras, a transliteração de caracteres acentuados para ASCII,
#' a normalização para letras maiúsculas, e outras operações específicas para colunas
#' que contêm a substring "nome" (ignorando maiúsculas e minúsculas).
#'
#' @param df Um dataframe a ser ajustado.
#'
#' @return Retorna o dataframe com os ajustes realizados.
#'
#' @details
#' A função percorre cada coluna do dataframe e, se for uma coluna de texto e
#' contiver a substring "nome" (ignorando maiúsculas e minúsculas), realiza os seguintes ajustes:
#'   - Translitera caracteres acentuados para ASCII.
#'   - Converte o texto para letras maiúsculas.
#'   - Remove espaços extras.
#'   - Remove prefixos específicos, como "RECEM NASCIDO", "NATIMORTO", entre outros.
#'   - Remove sufixos específicos, como "FILHO", "NETO", "SOBRINHO", "JUNIOR", entre outros.
#'   - Normaliza a utilização de preposições ("DE", "DOS", "DA", "DAS", "DO", "DDAS", "E") para espaços.
#'   - Remove caracteres não alfabéticos e não numéricos.
#'   - Troca novamente 2 ou mais espaços por um único espaço.
#'   - Se a letra repete mais de duas vezes, remove uma.
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(nome_completo = c("João da Silva", "Maria dos Santos", "José Neto Junior"))
#'   resultado <- ajuste_txt(df)
#' }
#'
#' @export
ajuste_txt <- function(df){

    # Remove mais de um espaço e ajusta nas colunas de texto
  for (coluna in names(df)) {
    if (is.character(df[[coluna]])) {
      # Verifica se o nome da coluna contém "nome" ou "NOME"
      if (grepl("_nome", coluna, ignore.case = TRUE)) {
        # Translitera caracteres acentuados
        df[[coluna]] <- iconv(df[[coluna]], "UTF-8", "ASCII")
        df[[coluna]] <- toupper(df[[coluna]])

        # Remove espaços extras
        df[[coluna]] <- gsub("\\s{2,}", " ", df[[coluna]])

        # Ajustes específicos para colunas contendo "nome" ou "NOME"
        df[[coluna]] <- stringr::str_squish(df[[coluna]])
        df[[coluna]] <- gsub("^RN |RECEM NASCIDO |RN NASCIDO |NATIMORTO|NATIMORTI |FETO MORTO|FETO|MORTO|NASCIDO VIVO|VIVO|NASCIDO|NAO IDENTIFICADO|SEM DOC|CADAVER|NATIMORTE|RECEM|IGNORADO|RECEM NASCIDO DE ", "", df[[coluna]])
        df[[coluna]] <- gsub(" FILHO| NETO| SOBRINHO| JUNIOR", "", df[[coluna]])
        df[[coluna]] <- gsub(" DE | DOS | DA | DOS | DAS | DO | DDAS | E ", " ", df[[coluna]])
        # Remove caracteres não alfabéticos
        df[[coluna]] <- gsub("[^A-Za-z ]", "", df[[coluna]])
        # Troca novamente 2 ou mais espaços para um único espaço
        df[[coluna]] <- gsub("\\s{2,}", " ", df[[coluna]])
        # Se a letra repete mais de duas vezes, remove uma
        df[[coluna]] <- gsub("(.)\\1{2,}", "\\1", df[[coluna]], perl = TRUE)

      }
    }
  }

  return(df)
}
