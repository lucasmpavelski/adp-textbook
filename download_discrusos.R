# library(MASS)
# library(broom)
# library(ggplot2)

# # 1. Ajustar o modelo
# modelo_nb <- glm.nb(y ~ x1 + x2 + offset(log(tempo)), data = docvars(corpus_discursos))
# 
# # 2. Organizar os dados (exponentiate = TRUE converte log-odds para IRR)
# resumo_modelo <- tidy(modelo_nb, conf.int = TRUE, exponentiate = TRUE)
# 
# # Remover o intercepto para o gráfico
# resumo_modelo <- resumo_modelo[resumo_modelo$term != "(Intercept)", ]
# 
# # 3. Plotar com ggplot2
# ggplot(resumo_modelo, aes(x = estimate, y = term)) +
#   geom_point(size = 3, color = "darkblue") +
#   geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
#   geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
#   scale_x_log10() + # Escala logarítmica é o padrão ouro para razões
#   labs(x = "Incidence Rate Ratio (log scale)", 
#        y = "Variáveis",
#        title = "Forest Plot de IRRs") +
#   theme_minimal()

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

extrair_dados_resposta <- function(resp) {
  resposta <- resp_body_json(resp, simplifyDataFrame = TRUE)
  return(resposta$dados)
}

req_discursos_deputado <- function(id_deputado, data_inicio, data_fim) {
  req <- request(url_api) |>
    req_url_path("api", "v2", "deputados", id_deputado, "discursos") |>
    req_url_query(
      dataInicio = data_inicio,
      dataFim = data_fim,
      itens = 1000
    ) |>
    req_throttle(capacity = 100, fill_time_s = 60)
  return(req)
}

buscar_todos_discursos <- function(id_deputado, data_inicio, data_fim) {
  req <- request(url_api) |>
    req_url_path("api", "v2", "deputados", id_deputado, "discursos") |>
    req_url_query(
      dataInicio = data_inicio,
      dataFim = data_fim,
      itens = 100
    ) |>
    req_throttle(capacity = 100, fill_time_s = 60)
  dados <- list()
  repeat {
    resp <- req_perform(req)
    resp_json <- resp_body_json(resp, simplifyDataFrame = TRUE)
    print(str(resp_json))
    dados <- append(dados, resp_json$dados)
    next_link <- resp_json$links |> filter(rel == 'next')
    if (nrow(next_link) == 0) {
      break
    }
    next_link <- next_link$href
    req <- request(next_link) |>
      req_throttle(capacity = 100, fill_time_s = 60)
  }
  return(dados)
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
  url_api <- "https://dadosabertos.camara.leg.br/"
  
  req <- request(url_api) |>
    req_url_path("api", "v2", "legislaturas") |>
    req_perform()
  
  resp <- resp_body_json(req, simplifyDataFrame = TRUE)
  
  return(resp$dados)
}

legislaturas <- buscar_legislaturas()

map(1:nrow(legislaturas), function(i) {
  legislatura <- legislaturas[i, ]

  id_legislatura <- legislatura$id
  data_inicio <- lubridate::date(legislatura$dataInicio)
  data_fim <- lubridate::date(legislatura$dataFim)
  periodos <-   seq(
      from = lubridate::floor_date(data_inicio, "month"),
      to = min(lubridate::today(), lubridate::floor_date(data_fim, "month")),
      by = "month"
    )
  
  df_deputados_legislatura <- request(url_api) |>
    req_url_path("api", "v2", "deputados") |>
    req_url_query(
      idLegislatura = id_legislatura, # número da legislatura
      nome = "",                      # parte do nome
      siglaUF = "",                   # sigla UF eleito
      siglaPartido = "",              # sigla do partido
      siglaSexo = "",                 # sexo (M ou F)
    ) |>
    req_perform() |>              # executa a requisição
    extrair_dados_resposta() |>   # extrai item "dados"
    select(                       # seleciona colunas relevantes
      id,
      nome,
      uri
    ) |>
    mutate(id_legislatura = id_legislatura) |>
    distinct()

  discursos <- map_dfr(seq_along(periodos[-length(periodos)]), function(i) {
    d_inicio <- periodos[i]
    d_fim <- periodos[i + 1] - lubridate::days(1)
    
    message(paste("Baixando discursos de", d_inicio, "até", d_fim))
    
    arquivo_discursos <- str_c("discursos_", id_legislatura, "_", month(d_inicio), "_", year(d_inicio), ".parquet")
    if (!file.exists(arquivo_discursos)) {
      discursos_mes <- obter_discursos_camara(
        df_deputados_legislatura,
        data_inicio = as.character(d_inicio),
        data_fim = as.character(d_fim)
      )
      write_parquet(discursos_mes, arquivo_discursos)
    } else {
      discursos_mes <- read_parquet(arquivo_discursos)
    }
    return(discursos_mes)
  })

  write_parquet(discursos, str_c("discursos_", id_legislatura, ".parquet"))
  1
})
