box::use(
  dplyr[...],
  tidyr[...],
  purrr[keep_at,
        map],
  stringr[str_replace,
          str_replace_all,
          str_pad],
  lubridate[as_datetime,
            date,
            hour,
            minute,
            second,
            month,
            day],
  httr2[request,
        req_method,
        req_headers,
        req_body_raw,
        req_perform,
        resp_body_json,
        req_auth_bearer_token],
  readr[read_csv]
)

multipla_para_binaria <- function(dados, coluna, separador = ","){
  
  saida <- dados %>%
    tidyr::separate_rows({{coluna}}, sep = separador) %>%
    dplyr::mutate(valor_de_saida = 1) %>%
    tidyr::pivot_wider(names_from = {{coluna}},
                       names_prefix = paste0(rlang::englue("{{coluna}}"), "."),
                       names_sort = TRUE,
                       values_from = valor_de_saida,
                       values_fill = 0)
  
  return(saida)
  
}

ano <- "2025"

v1.01.valores <- c("Livros \\(impressos\\)",
                   "Livros digitais \\(epub/pdf\\)",
                   "Tutoriais \\(textos e/ou vídeos\\)",
                   "Podcasts",
                   "Vídeos \\(exceto tutoriais\\)",
                   "Jogos/Games",
                   "Simulador\\(es\\) \\(realidade virtual/aumentada, 2D, 3D\\)",
                   "Apostilas",
                   "Slides",
                   "Outro\\(s\\)",
                   "Nenhum recurso didático foi utlizado")

v3.06.valores <- c("Sim, em bibliotecas físicas",
                   "Sim, em bibliotecas digitais",
                   "Não")

v5.02.valores <- c("Meus professores só passam atividades que não necessitam o uso de Internet",
                   "Prefiro fazer em casa as atividades que exigem o uso da internet",
                   "Tenho dificuldade para usar a Internet",
                   "Outro")

v5.03.valores <- c("Wi-Fi do Senac",
                   "Internet pessoal \\(dados móveis - 3G, 4G etc.\\)",
                   "Ponto fixo de internet \\(cabo/fibra\\)",
                   "Outra forma")

v5.04.valores <- c("A unidade onde estudo não disponibiliza internet para os alunos",
                   "O sinal de Internet da unidade é fraco ou ruim para acessar",
                   "Não sei como acessar a Internet da unidade",
                   "Prefiro usar minha própria Internet",
                   "Não acho seguro acessar a Internet da unidade",
                   "Outro motivo")

v5.05.valores <- c("Notebook próprio",
                   "Notebook do Senac",
                   "Computador de mesa ou fixo",
                   "Celular/Smartphone",
                   "Outro aparelho")

v5.06.valores <- c("Sala de aula",
                   "Biblioteca",
                   "Laboratório de informática ou sala de computadores",
                   "Ambientes de convivência \\(cantina, restaurante, corredores etc.\\)",
                   "Outro")

v5.08.valores <- c("Fazer tarefas passadas pelo professor",
                   "Realizar trabalhos com os colegas de turma",
                   "Tirar dúvidas com o professor/instrutor",
                   "Pesquisar conteúdos relacionados ao curso",
                   "Comunicar-se com a escola \\(secretaria, diretoria, coordenação, TI etc.\\)",
                   "Baixar materiais e lições que os professores passam",
                   "Enviar para os professores as lições e os exercícios que eles pedem",
                   "Prefiro não responder/ Não sei")

v5.09.valores <- c("Te ajudou a usar a Internet para fazer atividades do curso",
                   "Pediu para comparar informações da Internet em sites diferentes",
                   "Disse quais sites você deveria utilizar para fazer atividades do curso",
                   "Ensinou como usar a Internet de maneira segura \\(senhas, privacidade, compartilhamento de informações pessoais etc\\)",
                   "Ensinou como verificar se uma informação ou notícia da Internet é verdadeira",
                   "Conversou com os alunos sobre o uso de Inteligência Artificial")

v6.08.valores <- c("Wi-Fi próprio",
                   "Dados móveis \\(3g, 4g, 5g\\)",
                   "Ponto fixo de internet da sua casa",
                   "Wi-Fi público, emprestado de algum vizinho ou estabelecimento",
                   "Prefiro não responder/ Não sei")

v6.09.valores <- c("Computador de mesa ou fixo",
                   "Notebook",
                   "Televisão",
                   "Celular/ Smartphone",
                   "Prefiro não responder/ Não sei")

###############################################################################.

# 1. Carrega bases de dados geradas pelo Sphinx ---------------------------

# 1.1. Extracao do sphinx -------------------------------------------------

token <- request("https://pesquisa.senac.br/SphinxAuth/connect/token") %>%
  req_method("POST") %>%
  req_headers(`Content-Type` = "application/x-www-form-urlencoded") %>%
  req_body_raw(paste0("username=senac&token=",
                      Sys.getenv("sphinx_token"),
                      "&lang=pt-BR&grant_type=personal_token&client_id=sphinxapiclient")) %>%
  req_perform() %>%
  resp_body_json() %>%
  keep_at("access_token") %>%
  unlist()

pesquisa <- request(paste0("https://pesquisa.senac.br/sphinxapi/api/v4.1/survey/QP",
                           ano,
                           "/data")) %>%
  req_auth_bearer_token(token) %>%
  req_headers(Accept = "application/json") %>%
  req_perform() %>%
  resp_body_json() %>%
  map(unlist) %>%
  bind_rows()

# 1.2. Padrao GPAE --------------------------------------------------------

pesquisa <- pesquisa %>% 
  select(termo,
         starts_with("q"),
         cpf, 
         ead, 
         ead_tecnico, 
         aprendizagem, 
         pop_pesquisa, 
         motivo1, 
         motivo2, 
         motivo3, 
         motivo4, 
         n_curso, 
         nome_curso,
         filtro_cat_esc,
         dt.entrada = DATE_SAISIE,
         dt.conclusao = DATE_ENREG,
         dt.modif = DATE_MODIF, 
         tempo = TEMPS_SAISIE,
         tp.aparelho = APPAREIL_SAISIE,
         sit.quest = PROGRESSION,
         ultima_resp = DERNIERE_QUESTION_SAISIE) %>%
  rename_with(.cols = starts_with("q"), 
              .fn = function(x){
                saida <- str_replace(x, "q", "v")
                saida <- str_replace_all(saida, "_", ".")
                return(saida)
              })

###############################################################################.

funcao_troca <- function(variavel, dic) {
  tam <- seq_along(dic)
  
  for(i in tam) {
    variavel <- str_replace(variavel, dic[i], as.character(i))
  }
  
  return(variavel)
  
}

# 2. Tranformação de variáveis --------------------------------------------

pesquisa <- pesquisa %>% 
  mutate(across(where(is.character), ~na_if(., ""))) %>% 
  mutate(v1.01.check = v1.01,
         across(c(v3.06,
                  v6.08,
                  v6.09),
                ~.x, .names = "{.col}_check"),
         v1.01 = funcao_troca(v1.01, v1.01.valores),
         v3.06 = funcao_troca(v3.06, v3.06.valores),
         v6.08 = funcao_troca(v6.08, v6.08.valores),
         v6.09 = funcao_troca(v6.09, v6.09.valores)) %>% 
  multipla_para_binaria(v1.01, sep = " ; ") %>% 
  multipla_para_binaria(v3.06, sep = " ; ") %>% 
  multipla_para_binaria(v6.08, sep = " ; ") %>% 
  multipla_para_binaria(v6.09, sep = " ; ") %>% 
  relocate(any_of(c("v1.01.check",
                    "v1.01.1",
                    "v1.01.2",
                    "v1.01.3",
                    "v1.01.4",
                    "v1.01.5",
                    "v1.01.6",
                    "v1.01.7",
                    "v1.01.8",
                    "v1.01.9",
                    "v1.01.10",
                    "v1.01.11")),
           .after = "termo") %>% 
  relocate(any_of(c("v3.06_check",
                    "v3.06.1",
                    "v3.06.2",
                    "v3.06.3")),
           .after = "v3.05.4") %>% 
  relocate(any_of(c("v6.08_check",
                    "v6.08.1",
                    "v6.08.2",
                    "v6.08.3",
                    "v6.08.4",
                    "v6.08.5")),
           .after = v6.07) %>% 
  relocate(any_of(c("v6.09_check",
                    "v6.09.1",
                    "v6.09.2",
                    "v6.09.3",
                    "v6.09.4",
                    "v6.08.5")),
           .before = cpf)

pesquisa <- pesquisa %>%  
  select(-ends_with(".NA")) %>% 
  mutate(dt.entrada  = lubridate::as_datetime(dt.entrada, 
                                              format ="%m/%d/%Y %I:%M:%S %p"),
         dt.conclusao = lubridate::as_datetime(dt.conclusao, 
                                               format ="%m/%d/%Y %I:%M:%S %p"),
         check2 = dt.entrada,
         hora.ini    = paste(str_pad(hour(dt.entrada),
                                     width = 2,
                                     side = "left",
                                     pad = "0"),
                             str_pad(minute(dt.entrada),
                                     width = 2,
                                     side = "left",
                                     pad = "0"),
                             str_pad(second(dt.entrada),
                                     width = 2,
                                     side = "left",
                                     pad = "0"),
                             sep = ":"),
         hora.fim    = paste(str_pad(hour(dt.conclusao),
                                     width = 2,
                                     side = "left",
                                     pad = "0"),
                             str_pad(minute(dt.conclusao),
                                     width = 2,
                                     side = "left",
                                     pad = "0"),
                             str_pad(second(dt.conclusao),
                                     width = 2,
                                     side = "left",
                                     pad = "0"),
                             sep = ":"),
         dt.ini      = date(dt.entrada),
         dt.fim      = date(dt.conclusao),
         dia         = day(dt.entrada),
         dt.h        = paste0(str_pad(month(dt.entrada),
                                      width = 2,
                                      side = "left",
                                      pad = "0"),
                              "/",
                              str_pad(day(dt.entrada),
                                      width = 2,
                                      side = "left",
                                      pad = "0"),
                              "-",
                              str_pad(hour(dt.entrada),
                                      width = 2,
                                      side = "left",
                                      pad = "0")),
         tempo       = round(as.numeric(tempo)/60, 2),
         finalizado  = ifelse(termo == "Sim" & sit.quest == "Terminado", 1, 0),
         incompleto  = ifelse(termo == "Sim" & sit.quest == "Em andamento", 1, 0),
         id.pesquisa = row_number(),
         cpf         = as.numeric(cpf),
         pesquisa    = 1,
         across(c(ends_with("a"),
                  starts_with("v4")), tolower)) %>% 
  select(-check2) %>% 
  mutate(valido = ifelse(v4.01 != "" & termo == "Sim" & (sit.quest == "Em andamento" | 
                                                           sit.quest == "Terminado"), 1, 0))

###############################################################################.

# 3. Traz informacoes da Popualacao de pesquisa ---------------------------

# 3.1. População de pesquisa ----------------------------------------------

pop <- read_csv("data/pop_anqp_script.csv") %>% 
  select(-c(check, ead))

pesquisa <- left_join(pesquisa, pop, by = "cpf") #%>%



###############################################################################.

# 5. Salva a base para app e a base inicial -------------------------------

pesquisa <- pesquisa %>% 
  filter(pop_pesquisa == 1)

painel <- pesquisa %>%
  select(cpf, 
         dia,
         tempo,
         dt.conclusao,
         DR = DR2,
         cod.unidade,
         tp.aparelho, 
         ead,
         valido,
         total.unidade,
         total.dr)


hora <- Sys.time() %>% format("%d de %B às %H:%M")


saveRDS(painel,
        "data/dados.Rds")


saveRDS(hora, "data/hora.Rds")


