box::use(
  shiny[moduleServer, NS],
  dplyr[...],
 reactable[...],
)


#' @export
ui <- function(id) {
  ns <- NS(id)
      
      reactableOutput(ns("TblDrEstatisticas"),
               width = "100%")
    

}

#' @export
server <- function(id, dados, opcoes, DR_selecionado) {
  moduleServer(id, function(input, output, session) {
    
    output$TblDrEstatisticas <- renderReactable({
      
      nomes_ufs <- opcoes |> unname() |> unlist()
      trad <- data.frame(Nomes = nomes_ufs%>% names,
                         DR = unname(nomes_ufs),
                         stringsAsFactors = FALSE)
      
      trad <- trad %>% 
        mutate(Nomes = factor(Nomes))
      
      dados_t <- dados() %>%
        left_join(trad, by = c("DR")) %>%
        filter(!is.na(valido)) %>% 
        mutate(DR = Nomes,
               .keep = "unused") %>% 
        group_by(DR, ead) %>%
        filter(DR != "SG") %>%
        summarise(Validos = sum(valido == "1"),
                  Total = unique(Total),
                  Taxa = (Validos/Total))
      
      dr_nome_selecionado <- trad %>% 
        filter(DR == DR_selecionado()) %>% 
        pull(Nomes) %>% 
        as.character()
     
                  
      
      reactable(dados_t,
                pagination = FALSE,
                filterable = FALSE,
                highlight = FALSE,
                bordered = TRUE,
                striped = FALSE,
                height = 750,
                rowStyle = function(index) {
                  if (length(dr_nome_selecionado) > 0 && 
                      !is.na(dados_t$DR[index]) && 
                      as.character(dados_t$DR[index]) == dr_nome_selecionado) {
                    list(backgroundColor = "#ffcccc", fontWeight = "bold")
                  }
                },
                defaultColDef = colDef(format = colFormat(separators = TRUE,
                                                          locales = "pt-BR")),
                theme = reactableTheme(
                  color = "black",
                  highlightColor = "#8F93FF",
                  headerStyle = list(
                    color = "black",
                    fontWeight = "bold",
                    backgroundColor = "#B1D8B9",
                    fontSize = "18px"
                                     )
                ),
                columns = list(
                  ead = colDef(
                    show = FALSE
                  ),
                  DR = colDef(
                    name = "Departamento Regional"
                  ),
                  Validos = colDef(
                    filterable = FALSE,
                    name = "Total de questionários válidos",
                    align = "center",
                    style = list(
                      fontSize = "16px"
                      
                    )
                                      ),
                  Total = colDef(
                    name = "População Alvo",
                    align = "center"
                  ),
                  Taxa = colDef(
                    name = "Taxa de resposta (%)",
                    filterable = FALSE,
                    format = colFormat(separators = TRUE,
                                       percent = TRUE,
                                       digits = 2),
                    align = "center",
                    style = list(
                      fontSize = "16px"
                    )
                  )
                )
                
                )
                
      
        
    })
    
  })
  }
    
