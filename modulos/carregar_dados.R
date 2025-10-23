box::use(
  dplyr[...],
  purrr[imap]
)


#' @export
brasil <- readRDS("data/br_uf_shape.Rds")

#' @export
opcoes <- list(Norte = c(Acre = "AC", 
                         `Amapá` = "AP",
                         Amazonas = "AM", 
                         `Pará` = "PA", 
                         `Rondônia` = "RO",
                         Roraima = "RR",
                         Tocantins = "TO"),
               Nordeste = c(Alagoas = "AL",
                            Bahia = "BA",
                            `Ceará` = "CE", 
                            `Maranhão` = "MA",
                            `Paraíba` = "PB", 
                            Pernambuco = "PE",
                            `Piauí` = "PI",
                            `Rio Grande do Norte` = "RN", 
                            Sergipe = "SE"),
               `Centro-Oeste` = c(`Distrito Federal` = "DF", 
                                  `Goiás` = "GO", `Mato Grosso` = "MT", `Mato Grosso do Sul` = "MS"
                ), Sudeste = c(`Espírito Santo` = "ES", `Minas Gerais` = "MG", 
                               `Rio de Janeiro` = "RJ", `São Paulo` = "SP"), Sul = c(`Paraná` = "PR", 
                                                                                     `Rio Grande do Sul` = "RS", `Santa Catarina` = "SC"), `Departamento Nacional` = list(
                                                                                       `Senac Gastronomia` = "SG"))

#' @export
unidades <- readRDS("data/unidades.Rds")
#' @export
dados_populacao <- readRDS("data/dados_p.rds") %>% 
  as_tibble() %>%
  split(.$ead) %>% 
  imap(~{if(.y == 1) .x %>%
      distinct(DR, ead, pop_a, pop_p) %>%
      mutate(tx = pop_p/pop_a) else{
        DR <- .x %>%
          summarise(cod.unidade = 0,
                    pop_a = sum(pop_a),
                    pop_p = sum(pop_p),
                    .by = c(DR, ead))
        
        BR <- DR %>% 
          summarise(cod.unidade = 0,
                    pop_a = sum(pop_a),
                    pop_p = sum(pop_p),
                    .by = ead) %>%
          mutate(DR = "BR")
        
        .x  %>%
          bind_rows(DR) %>%
          bind_rows(BR) %>%
          mutate(tx = pop_p/pop_a,
                 cod.unidade = paste(DR,
                                     cod.unidade,
                                     sep = "-"))
        
      }
  })

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
                         .default = NA_character_),
         cod.unidade = paste(DR,
                             cod.unidade,
                             sep = "-")) %>% 
  split(.$ead)


#' @export
hora_da_exportacao <- format(Sys.time(), "%d de %B às %H:%M")