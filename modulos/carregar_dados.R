box::use(
  dplyr[`%>%`,
        as_tibble,
        mutate,
        case_when]
)


#' @export
brasil <- readRDS("data/br_uf_shape.Rds")

#' @export
opcoes <- readRDS("data/opcoes.Rds")

#' @export
dados_populacao <- readRDS("data/dados_p.rds") %>% 
  as_tibble() %>%
  split(.$ead)

#' @export
dados_primarios <- readRDS("data/dados.Rds") %>%
  as_tibble() %>%
  mutate(DR2 = case_when(DR == "AC" ~ "12",
                         DR == "AL" ~ "27",
                         DR == "AM" ~ "13",
                         DR == "AP" ~ "16",
                         DR == "BA" ~ "29",
                         DR == "CE" ~ "23",
                         DR == "DF" ~ "53",
                         DR == "ES" ~ "32",
                         DR == "GO" ~ "52",
                         DR == "MA" ~ "21",
                         DR == "MG" ~ "31",
                         DR == "MS" ~ "50",
                         DR == "MT" ~ "51",
                         DR == "PA" ~ "15",
                         DR == "PB" ~ "25",
                         DR == "PE" ~ "26",
                         DR == "PI" ~ "22",
                         DR == "PR" ~ "41",
                         DR == "RJ" ~ "33",
                         DR == "RN" ~ "24",
                         DR == "RO" ~ "11",
                         DR == "RR" ~ "14",
                         DR == "RS" ~ "43",
                         DR == "SC" ~ "42",
                         DR == "SE" ~ "28",
                         DR == "SP" ~ "35",
                         DR == "TO" ~ "17",
                         .default = NA_character_)) %>% 
  split(.$ead)


#' @export
hora_da_exportacao <- format(Sys.time(), "%d de %B às %H:%M")