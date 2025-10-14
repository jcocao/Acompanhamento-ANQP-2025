box::use(
  bslib[value_box,
        layout_column_wrap],
  bsicons[bs_icon]
)

#' @export
caixas_acessos <- layout_column_wrap(
  width = 1,
  #total_acessos = 
    value_box(
    title = "Vendas",
    value = "R$ 45.000",
    showcase = bs_icon("people-fill"),
    showcase_layout = "left center"
  ),
  #tempo_medio_de_resposta =
  value_box(
    title = "Clientes",
    value = "1.230",
    showcase = bs_icon("clipboard-check-fill"),
    showcase_layout = "left center"
  ),
  #tempo_mediano_de_resposta = 
  value_box(
    title = "Satisfação",
    value = "92%",
    showcase = bs_icon("percent"),
    showcase_layout = "left center"
  )
)
