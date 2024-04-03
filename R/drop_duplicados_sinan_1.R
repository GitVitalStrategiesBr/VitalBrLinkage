
#' Dropa registros duplicados no SINAN
#'
#' @param df Nome do dataframe
#'
#' @return A função considera duas regras de duplicados:
#' Inicialmente ele verifica se existem registros exatamente iguais, se sim, aplica o drop deixando apenas um registro;
#' Em seguida, as colunas NU_NOTIFIC e NM_PACIENT são concatenadas. Se houverem duplicadas nessas regras, um dos registros será deletado.
#' @export
drop_duplicados_sinan_1 <- function(df){

  # Verifica se tem linhas duplicadas
  duplicated_rows <- df[duplicated(df), ]

  # remove linhas duplicadas
  # Aplica apenas se duplicated_rows tiver valores
  if (nrow(duplicated_rows) > 0) {
    # Remove linhas duplicadas
    df <- df[!duplicated(df), ]
  }
  rm(duplicated_rows)


  # Concatenando o número de notificação com o código do paciente
  df <- df |>
    dplyr::mutate(id_del1=paste0(as.character(NU_NOTIFIC),NM_PACIENT))

  # Identificar registros duplicados
  t <- df |>
    dplyr::group_by(NU_NOTIFIC,NM_PACIENT) |>
    dplyr::summarise(
      n = n(),
      .groups = 'drop'
    ) |>
    dplyr::filter(n > 1) |>
    dplyr::arrange(NU_NOTIFIC) |>
    dplyr::mutate(id_del1=paste0(as.character(NU_NOTIFIC),NM_PACIENT))

  # Filtrando e criando um novo conjunto de dados com registros a serem deletados
  notif_to_del <- df |>
    dplyr::mutate(id_del1=paste0(as.character(NU_NOTIFIC),NM_PACIENT)) |>
    dplyr::filter(id_del1%in%unique(t$id_del1))

  # nome das variáveis
  nm_df <- names(df)

  #tirando NUMERODO do vetor de nomes
  nm_df <- setdiff(nm_df, c("id_del1"))

  #Preenchendo as linhas em que uma NUNOTIFIC é missing por outra que não é missing, todos os campos.
  notif_to_del <- notif_to_del |>
    dplyr::group_by(id_del1) |>
    tidyr::fill(nm_df, .direction = "downup") |>
    dplyr::ungroup()


  #selecionando uma NOTIFIC aleatória
  notif_to_del <- notif_to_del |>
    dplyr::group_by(id_del1) |>
    dplyr::slice_sample(n = 1) |>
    dplyr::ungroup()

  #Adicionando as NOTIF aleatórias repetidas
  df <- df |>
    dplyr::filter(!id_del1%in%unique(t$id_del1)) |>
    rbind(dplyr::select(notif_to_del)) |>
    dplyr::select(-id_del1)

  return(df)

}
