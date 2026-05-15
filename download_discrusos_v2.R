if (!"tidyverse" %in% installed.packages()) {
  install.packages("tidyverse")
}
library(tidyverse)

if (!"httr2" %in% installed.packages()) {
  install.packages("httr2")
}
library(httr2)

if (!"arrow" %in% installed.packages()) {
 install.packages("arrow")
}
library(arrow)

url_api <- "https://dadosabertos.camara.leg.br/"

buscar_todas_as_paginas <- function(req) {
  tryCatch({
    dados <- data.frame()
    repeat {
      resp <- req |>
        req_throttle(capacity = 100, fill_time_s = 60) |>
        req_perform()
      resp_json <- resp_body_json(resp, simplifyDataFrame = TRUE)
      dados <- bind_rows(dados, resp_json$dados)
      next_link <- resp_json$links |> filter(rel == 'next')
      if (nrow(next_link) == 0) {
        break
      }
      next_link <- next_link$href
      req <- request(next_link) |>
        req_throttle(capacity = 100, fill_time_s = 60)
    }
    return(dados)
  }, error = function(e) {
    write_file(paste(Sys.time(), "-", req$url, "\n"), "erros_requisicao.txt", append = TRUE)
    print(paste("Erro ao realizar a requisição:", req$url))
    return(data.frame())
  })
}

obter_discursos_camara <- function(df_deputados, data_inicio, data_fim) {
  col_originais <- colnames(df_deputados)
  discursos <- df_deputados |>
    mutate(req_discursos = map(id, \(id_deputado) req_discursos_deputado(id_deputado, data_inicio, data_fim))) |>
    mutate(resp_discursos = req_perform_parallel(req_discursos)) |>
    mutate(discursos = map(resp_discursos, extrair_dados_resposta)) |> # respostas
    filter(lengths(discursos) > 0) |> # filtrar linhas sem discurso
    unnest(discursos)              # desaninhar dados de discursos
  
  if (nrow(discursos) > 0) {
    discursos <- discursos |>
      unnest_wider(faseEvento, names_sep = "_") |> # desaninhar dados de evento
      select(                                      # selecionar colunas relevantes
        col_originais,
        url_texto = urlTexto,
        hora_inicio = dataHoraInicio,
        fase_evento = faseEvento_titulo,
        tipo_discurso = tipoDiscurso,
        keywords,
        sumario,
        transcricao
      )
    } else {
      discursos <- data.frame()
    }
    return(discursos)
  }

buscar_legislaturas <- function() {
  request(url_api) |>
    req_url_path("api", "v2", "legislaturas") |>
    buscar_todas_as_paginas()
}

buscar_deputados_legislatura <- function(id_legislatura) {
  dt <- request(url_api) |>
    req_url_path("api", "v2", "deputados") |>
    req_url_query(
      idLegislatura = id_legislatura, # número da legislatura
      nome = "",                      # parte do nome
      siglaUF = "",                   # sigla UF eleito
      siglaPartido = "",              # sigla do partido
      siglaSexo = "",                 # sexo (M ou F)
      itens = 1000
    ) |>
    buscar_todas_as_paginas()
  if (nrow(dt) > 0) {
    dt <- dt |>
      select(                       # seleciona colunas relevantes
        id,
        nome,
        uri
      ) |>
      mutate(id_legislatura = id_legislatura) |>
      distinct()
  } else {
    dt <- data.frame()
  }
  return(dt)
}

cached <- function(nome, f, ...) {
  cache_path <- str_c("cache/", nome, str_c(list(...), collapse = "_"), ".rds")
  if (file.exists(cache_path)) {
    print(paste("Lendo dados do cache...", cache_path))
    return(read_rds(cache_path))
  }
  res <- f(...)
  write_rds(res, cache_path)
  return(res)
}

buscar_discursos <- function(id_deputado, data_inicio, data_fim) {
  print(paste("Baixando discursos de", data_inicio, "até", data_fim, "do deputado", id_deputado))
  dt <- request(url_api) |>
    req_url_path("api", "v2", "deputados", id_deputado, "discursos") |>
    req_url_query(
      dataInicio = data_inicio,
      dataFim = data_fim,
      itens = 1000
    ) |>
    buscar_todas_as_paginas()
}

buscar_dados_deputados <- function(id_legislatura) {
  dt <- request(url_api) |>
    req_url_path("api", "v2", "deputados") |>
    req_url_query(
      itens = 1000,
      idLegislatura = id_legislatura
    ) |>
    buscar_todas_as_paginas()
}

buscar_discursos_cache <- function(id_deputado, data_inicio, data_fim) {
  cached("discursos", buscar_discursos, id_deputado, data_inicio, data_fim)
}

buscar_deputados_legislaturas <- function() {
  dt_legislaturas <- buscar_legislaturas()
  dt_deputados <- dt_legislaturas |>
    mutate(deputados = map(id, buscar_deputados_legislatura, .progress = TRUE)) |>
    unnest(deputados, names_sep = "_")

  dt_deputados
}

library(furrr)

#future::plan(multisession, workers = 10)
future::plan(sequential)

dt_deputados <- cached("deputados_legislaturas", buscar_deputados_legislaturas) |> 
  filter(id %in% 51:57)


dt_discursos <- dt_deputados |>
  rename(
    id_deputado = deputados_id,
    data_inicio = dataInicio,
    data_fim = dataFim
  ) |>
  filter(id_deputado != 160508) |> # dado retornando Erro do servidor
  mutate(discursos = pmap(
    across(c("id_deputado", "data_inicio", "data_fim")),
    buscar_discursos_cache,
    .progress = TRUE
  ))

dados_deputados <- map(dt_discursos$id |> unique(), buscar_dados_deputados)

dados_deputados <- dados_deputados |>
  bind_rows() |>
  select(id, idLegislatura, nome, siglaPartido, siglaUf) |>
  distinct() |>
  group_by(id, idLegislatura) |>
  slice_head(n = 1)

dt_discursos <- dt_discursos |>
  unnest(discursos) |>
  unnest_wider(faseEvento, names_sep = "_") 


dt_discursos <- dt_discursos |>
  left_join(dados_deputados, by = c("id" = "idLegislatura", "id_deputado" = "id"))

#|>
#  left_join(dados_deputados, by = c("id_deputado" = "id", "id" = "idLegislatura"))

dt_discursos2 <- dt_discursos |>
  select(
    id = id_deputado,
    nome = deputados_nome,
    uf = siglaUf,
    partido = siglaPartido,
    uri = deputados_uri,
    nome_civil = deputados_nome,
    sexo = NULL,
    data_nascimento = NULL,
    data_falecimento = NULL,
    uf_nascimento = NULL,
    municipio_nascimento = NULL,
    escolaridade = NULL,
    situacao = NULL,
    data_situacao = NULL,
    ultimo_partido = siglaPartido,
    hora_inicio = dataHoraInicio,
    hora_fim = dataHoraFim,
    fase_evento = faseEvento_titulo,
    tipo_discurso = tipoDiscurso,
    keywords,
    sumario,
    transcricao
  )

anos <- year(dt_discursos2$hora_inicio) |> unique()

for (a in anos) {
  dt_discursos2 |>
    filter(year(hora_inicio) == a) |>
    write_csv(str_c("discursos_camara_", a, ".csv"))
}

