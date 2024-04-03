#' Remove duplicatas específicas do dataframe.
#'
#' Esta função remove duplicatas do dataframe de acordo com regras específicas.
#'
#' @param df Um dataframe contendo os dados a serem processados.
#' @return Um dataframe sem as duplicatas identificadas pelas regras estabelecidas.
#' @export
drop_duplicados_sim_padronizado <- function(df){

  #TOTAL SIM POS UNIQUE 265106
  ###REMOVENDO DUPLICIDADES ETAPAS

  #Verificando NUMERO DO
  t <- df |>
    dplyr::group_by(nu_do) |>
    dplyr::summarise(n = n()) |>
    dplyr::filter(n > 1)

  df_to_del <- merge(df, t, by = "nu_do")

  nm_df <- names(df)

  #tirando nu_do do vetor de nomes
  nm_df <- lubridate::setdiff(nm_df, c("nu_do"))

  #Preenchendo as linhas em que uma DO é missing por outra que não é missing, todos os campos.
  df_to_del <- df_to_del |>
    dplyr::group_by(nu_do) |>
    tidyr::fill(nm_df, .direction = "downup") |>
    dplyr::ungroup()

  #selecionando uma DO aleatória
  df_to_del <- df_to_del |>
    dplyr::group_by(nu_do) |>
    dplyr::slice_sample(n = 1)

  t <- as.vector(t$nu_do)


  #Adicionando as DOs aleatórias repetidas
  df <- df |>
    dplyr::filter(!nu_do%in%t) |>
    rbind(dplyr::select(df_to_del,-n))


  ######TRATAR DUPLICACOES NO df

  df <- df |>
    dplyr::mutate(ano_nasc=substr(dt_nasc,5,8))

  nm_iguais <- df |>
    dplyr::filter(is.na(ds_nome_pac)==F) |>
    dplyr::group_by(    ds_nome_pac,
                        ds_nome_pai,
                        ds_nome_mae,ano,ano_nasc) |>
    dplyr::summarise(n=n()) |>
    dplyr::filter(n>1) |>
    dplyr::mutate(id=paste0(ds_nome_pac,
                            ds_nome_pai,
                            ds_nome_mae,ano,ano_nasc)) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::mutate(id=paste0(ds_nome_pac,
                            ds_nome_pai,
                            ds_nome_mae,ano,ano_nasc))


  t_id <- as.vector(nm_iguais$id)

  nm_iguais <- nm_iguais |>
    dplyr::select(id,n)

  df_to_del2 <- merge(df, nm_iguais, by = "id")

  nm_df <- names(df)

  #tirando nu_do do vetor de nomes
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
    dplyr::group_by(    nu_cns,
                        dt_obito,
                        dt_nasc) |>
    dplyr::summarise(n=n()) |>
    dplyr::filter(n>1,is.na(nu_cns)==F) |>
    dplyr::mutate(id=paste0( nu_cns,
                             dt_obito,
                             dt_nasc)) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::mutate(id=paste0( nu_cns,
                             dt_obito,
                             dt_nasc))


  t_id <- as.vector(regra_c2$id)

  regra_c2 <- regra_c2 |>
    dplyr::select(id,n)

  df_to_del_c2 <- merge(df, regra_c2, by = "id")

  nm_df <- names(df)

  #tirando nu_do do vetor de nomes
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
      ds_nome_pac_sound,
      dt_obito,
      dt_nasc) |>
    dplyr::summarise(n=n()) |>
    dplyr::filter(n>1,is.na(dt_nasc)==F) |>
    dplyr::mutate(id=paste0( ds_nome_pac_sound,
                             dt_obito,
                             dt_nasc)) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::mutate(id=paste0( ds_nome_pac_sound,
                             dt_obito,
                             dt_nasc))


  t_id <- as.vector(regra_c2$id)

  regra_c2 <- regra_c2 |>
    dplyr::select(id,n)

  df_to_del_c2 <- merge(df, regra_c2, by = "id")

  nm_df <- names(df)

  #tirando nu_do do vetor de nomes
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
      ds_nome_pac_sound,
      ano,
      dt_nasc) |>
    dplyr::summarise(n=n()) |>
    dplyr::filter(n>1,is.na(dt_nasc)==F) |>
    dplyr::mutate(id=paste0( ds_nome_pac_sound,
                             ano,
                             dt_nasc)) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::mutate(id=paste0( ds_nome_pac_sound,
                             ano,
                             dt_nasc))


  t_id <- as.vector(regra_c3$id)

  regra_c3 <- regra_c3 |>
    dplyr::select(id,n)

  df_to_del_c3 <- merge(df, regra_c3, by = "id")

  nm_df <- names(df)

  #tirando nu_do do vetor de nomes
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
    dplyr::mutate(causa_3d=substr(cd_causabas,1,3))

  regra_c3 <- df |>
    dplyr::group_by(    ds_nome_mae_sound,
                        dt_nasc,
                        dt_obito,
                        causa_3d) |>
    dplyr::summarise(n=n()) |>
    dplyr::filter(n>1,is.na(dt_nasc)==F) |>
    dplyr::mutate(id=paste0( ds_nome_mae_sound,
                             dt_nasc,
                             dt_obito,
                             causa_3d)) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::mutate(id=paste0( ds_nome_mae_sound,
                             dt_nasc,
                             dt_obito,
                             causa_3d))


  t_id <- as.vector(regra_c3$id)

  regra_c3 <- regra_c3 |>
    dplyr::select(id,n)

  df_to_del_c3 <- merge(df, regra_c3, by = "id")

  nm_df <- names(df)

  #tirando nu_do do vetor de nomes
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
      ds_nome_pai_sound,
      dt_nasc,
      dt_obito,
      causa_3d) |>
    dplyr::summarise(n=n()) |>
    dplyr::filter(n>1,is.na(dt_nasc)==F) |>
    dplyr::mutate(id=paste0( ds_nome_pai_sound,
                             dt_nasc,
                             dt_obito,
                             causa_3d)) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::mutate(id=paste0( ds_nome_pai_sound,
                             dt_nasc,
                             dt_obito,
                             causa_3d))


  t_id <- as.vector(regra_c3$id)

  regra_c3 <- regra_c3 |>
    dplyr::select(id,n)

  df_to_del_c3 <- merge(df, regra_c3, by = "id")

  nm_df <- names(df)

  #tirando nu_do do vetor de nomes
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
