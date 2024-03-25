#' Trata os duplicados do SIM
#'
#' @param df nome do data frame
#'
#' @return Retorna o dataframe com os duplicados removidos
#' @export
drop_duplicados_sim <- function(df){
  #TOTAL SIM POS UNIQUE 265106
  ###REMOVENDO DUPLICIDADES ETAPAS

  #Verificando NUMERO DO
  t <- df |>
    dplyr::group_by(NUMERODO) |>
    dplyr::summarise(n = n()) |>
    dplyr::filter(n > 1)

  df_to_del <- merge(df, t, by = "NUMERODO")

  nm_df <- names(df)

  #tirando NUMERODO do vetor de nomes
  nm_df <- lubridate::setdiff(nm_df, c("NUMERODO"))

  #Preenchendo as linhas em que uma DO é missing por outra que não é missing, todos os campos.
  df_to_del <- df_to_del |>
    dplyr::group_by(NUMERODO) |>
    tidyr::fill(nm_df, .direction = "downup") |>
    dplyr::ungroup()

  #selecionando uma DO aleatória
  df_to_del <- df_to_del |>
    dplyr::group_by(NUMERODO) |>
    dplyr::slice_sample(n = 1)

  t <- as.vector(t$NUMERODO)


  #Adicionando as DOs aleatórias repetidas
  df <- df |>
    dplyr::filter(!NUMERODO%in%t) |>
    rbind(dplyr::select(df_to_del,-n))


  ######TRATAR DUPLICACOES NO df

  df <- df |>
    dplyr::mutate(ANO_NASC=substr(DTNASC,5,8))

  nm_iguais <- df |>
    dplyr::filter(is.na(NOME)==F) |>
    dplyr::group_by(    NOME,
                        NOMEPAI,
                        NOMEMAE,ANO,ANO_NASC) |>
    dplyr::summarise(n=n()) |>
    dplyr::filter(n>1) |>
    dplyr::mutate(id=paste0(NOME,
                     NOMEPAI,
                     NOMEMAE,ANO,ANO_NASC)) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::mutate(id=paste0(NOME,
                     NOMEPAI,
                     NOMEMAE,ANO,ANO_NASC))


  t_id <- as.vector(nm_iguais$id)

  nm_iguais <- nm_iguais |>
    dplyr::select(id,n)

  df_to_del2 <- merge(df, nm_iguais, by = "id")

  nm_df <- names(df)

  #tirando NUMERODO do vetor de nomes
  nm_df <- lubridate::setdiff(nm_df, c("id"))

  #Preenchendo as linhas em que uma DO é missing por outra que não é missing, todos os campos.
  df_to_del2 <- df_to_del2 |>
    dplyr::group_by(id) |>
    tidyr::fill(nm_df, .direction = "downup") |>
    dplyr::ungroup()

  #selecionando uma DO aleatória
  df_to_del2 <- df_to_del2 |>
    dplyr::group_by(id) |>
    dplyr::slice_sample(n = 1)

  t2 <- as.vector(nm_iguais$id)


  #Adicionando as DOs aleatórias repetidas
  df <- df |>
    dplyr::filter(!id%in%t2) |>
    rbind(dplyr::select(df_to_del2,-n))


  #####TRATAR DUPLICACOES NO df 2 C_2 stata


  regra_c2 <- df |>
    dplyr::group_by(    NUMSUS,
                        DTOBITO,
                        DTNASC) |>
    dplyr::summarise(n=n()) |>
    dplyr::filter(n>1,is.na(NUMSUS)==F) |>
    dplyr::mutate(id=paste0( NUMSUS,
                      DTOBITO,
                      DTNASC)) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::mutate(id=paste0( NUMSUS,
                      DTOBITO,
                      DTNASC))


  t_id <- as.vector(regra_c2$id)

  regra_c2 <- regra_c2 |>
    dplyr::select(id,n)

  df_to_del_c2 <- merge(df, regra_c2, by = "id")

  nm_df <- names(df)

  #tirando NUMERODO do vetor de nomes
  nm_df <- lubridate::setdiff(nm_df, c("id"))

  #Preenchendo as linhas em que uma DO é missing por outra que não é missing, todos os campos.
  df_to_del_c2 <- df_to_del_c2 |>
    dplyr::group_by(id) |>
    tidyr::fill(nm_df, .direction = "downup") |>
    dplyr::ungroup()

  #selecionando uma DO aleatória
  df_to_del_c2 <- df_to_del_c2 |>
    dplyr::group_by(id) |>
    dplyr::slice_sample(n = 1)

  t2 <- as.vector(regra_c2$id)


  #Adicionando as DOs aleatórias repetidas
  df <- df |>
    dplyr::filter(!id%in%t2) |>
    rbind(dplyr::select(df_to_del_c2,-n))







  #####TRATAR DUPLICACOES NO df

  regra_c2 <- df |>
    dplyr::group_by(
      NOME_SOUND,
      DTOBITO,
      DTNASC) |>
    dplyr::summarise(n=n()) |>
    dplyr::filter(n>1,is.na(DTNASC)==F) |>
    dplyr::mutate(id=paste0( NOME_SOUND,
                      DTOBITO,
                      DTNASC)) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::mutate(id=paste0( NOME_SOUND,
                      DTOBITO,
                      DTNASC))


  t_id <- as.vector(regra_c2$id)

  regra_c2 <- regra_c2 |>
    dplyr::select(id,n)

  df_to_del_c2 <- merge(df, regra_c2, by = "id")

  nm_df <- names(df)

  #tirando NUMERODO do vetor de nomes
  nm_df <- lubridate::setdiff(nm_df, c("id"))

  #Preenchendo as linhas em que uma DO é missing por outra que não é missing, todos os campos.
  df_to_del_c2 <- df_to_del_c2 |>
    dplyr::group_by(id) |>
    tidyr::fill(nm_df, .direction = "downup") |>
    dplyr::ungroup()

  #selecionando uma DO aleatória
  df_to_del_c2 <- df_to_del_c2 |>
    dplyr::group_by(id) |>
    dplyr::slice_sample(n = 1)

  t2 <- as.vector(regra_c2$id)


  #Adicionando as DOs aleatórias repetidas
  df <- df |>
    dplyr::filter(!id%in%t2) |>
    rbind(dplyr::select(df_to_del_c2,-n))


  df <- df |>
    dplyr::select(-id)


  ######TRATANDO mais uma possibilidade


  #####TRATAR DUPLICACOES NO df

  regra_c3 <- df |>
    dplyr::group_by(
      NOME_SOUND,
      ANO,
      DTNASC) |>
    dplyr::summarise(n=n()) |>
    dplyr::filter(n>1,is.na(DTNASC)==F) |>
    dplyr::mutate(id=paste0( NOME_SOUND,
                      ANO,
                      DTNASC)) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::mutate(id=paste0( NOME_SOUND,
                      ANO,
                      DTNASC))


  t_id <- as.vector(regra_c3$id)

  regra_c3 <- regra_c3 |>
    dplyr::select(id,n)

  df_to_del_c3 <- merge(df, regra_c3, by = "id")

  nm_df <- names(df)

  #tirando NUMERODO do vetor de nomes
  nm_df <- lubridate::setdiff(nm_df, c("id"))

  #Preenchendo as linhas em que uma DO é missing por outra que não é missing, todos os campos.
  df_to_del_c3 <- df_to_del_c3 |>
    dplyr::group_by(id) |>
    tidyr::fill(nm_df, .direction = "downup") |>
    dplyr::ungroup()

  #selecionando uma DO aleatória
  df_to_del_c3 <- df_to_del_c3 |>
    dplyr::group_by(id) |>
    dplyr::slice_sample(n = 1)

  t2 <- as.vector(regra_c3$id)


  #Adicionando as DOs aleatórias repetidas
  df <- df |>
    dplyr::filter(!id%in%t2) |>
    rbind(dplyr::select(df_to_del_c3,-n))


  df <- df |>
    dplyr::select(-id)

  ##MAE, mae, datas e causabasc
  #####TRATAR DUPLICACOES NO df
  df <- df |>
    dplyr::mutate(causa_3d=substr(CAUSABAS,1,3))

  regra_c3 <- df |>
    dplyr::group_by(    NOMEMAE_SOUND,
                        DTNASC,
                        DTOBITO,
                        causa_3d) |>
    dplyr::summarise(n=n()) |>
    dplyr::filter(n>1,is.na(DTNASC)==F) |>
    dplyr::mutate(id=paste0( NOMEMAE_SOUND,
                      DTNASC,
                      DTOBITO,
                      causa_3d)) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::mutate(id=paste0( NOMEMAE_SOUND,
                      DTNASC,
                      DTOBITO,
                      causa_3d))


  t_id <- as.vector(regra_c3$id)

  regra_c3 <- regra_c3 |>
    dplyr::select(id,n)

  df_to_del_c3 <- merge(df, regra_c3, by = "id")

  nm_df <- names(df)

  #tirando NUMERODO do vetor de nomes
  nm_df <- lubridate::setdiff(nm_df, c("id"))

  #Preenchendo as linhas em que uma DO é missing por outra que não é missing, todos os campos.
  df_to_del_c3 <- df_to_del_c3 |>
    dplyr::group_by(id) |>
    tidyr::fill(nm_df, .direction = "downup") |>
    dplyr::ungroup()

  #selecionando uma DO aleatória
  df_to_del_c3 <- df_to_del_c3 |>
    dplyr::group_by(id) |>
    dplyr::slice_sample(n = 1)

  t2 <- as.vector(regra_c3$id)


  #Adicionando as DOs aleatórias repetidas
  df <- df |>
    dplyr::filter(!id%in%t2) |>
    rbind(dplyr::select(df_to_del_c3,-n))


  df <- df |>
    dplyr::select(-id)



  ##PAI, datas e causabasc
  #####TRATAR DUPLICACOES NO df

  regra_c3 <- df |>
    dplyr::group_by(
      NOMEPAI_SOUND,
      DTNASC,
      DTOBITO,
      causa_3d) |>
    dplyr::summarise(n=n()) |>
    dplyr::filter(n>1,is.na(DTNASC)==F) |>
    dplyr::mutate(id=paste0( NOMEPAI_SOUND,
                      DTNASC,
                      DTOBITO,
                      causa_3d)) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::mutate(id=paste0( NOMEPAI_SOUND,
                      DTNASC,
                      DTOBITO,
                      causa_3d))


  t_id <- as.vector(regra_c3$id)

  regra_c3 <- regra_c3 |>
    dplyr::select(id,n)

  df_to_del_c3 <- merge(df, regra_c3, by = "id")

  nm_df <- names(df)

  #tirando NUMERODO do vetor de nomes
  nm_df <- lubridate::setdiff(nm_df, c("id"))

  #Preenchendo as linhas em que uma DO é missing por outra que não é missing, todos os campos.
  df_to_del_c3 <- df_to_del_c3 |>
    dplyr::group_by(id) |>
    tidyr::fill(nm_df, .direction = "downup") |>
    dplyr::ungroup()

  #selecionando uma DO aleatória
  df_to_del_c3 <- df_to_del_c3 |>
    dplyr::group_by(id) |>
    dplyr::slice_sample(n = 1)

  t2 <- as.vector(regra_c3$id)


  #Adicionando as DOs aleatórias repetidas
  df <- df |>
    dplyr::filter(!id%in%t2) |>
    rbind(dplyr::select(df_to_del_c3,-n))


  df <- df |>
    dplyr::select(-id,-causa_3d)



  names(df)
  df <- df |>
    dplyr::mutate(id=rownames(df))

}
